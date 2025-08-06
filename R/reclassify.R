.reclassify_perene_filename <- function(version) {
    paste0("perene", "-", version, ".tif")
}

#' @export
reclassify_trajectory_perene_data <- function(files,
                                              perene_class_id,
                                              vs_class_id,
                                              version,
                                              multicores,
                                              memsize,
                                              output_dir) {
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    stopifnot(is.character(output_dir))

    out_filename <- .reclassify_perene_filename(version)
    out_file <- fs::path(output_dir) / out_filename

    if (file.exists(out_file)) {
        return(out_file)
    }

    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )

    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = (length(files) * terra::nlyr(rast_template)),
        nbytes = 8,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Update multicores parameter based on size of a single block
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter based on the size of memory and number of cores
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = image_size,
        memsize = memsize,
        multicores = multicores
    )
    # Create chunks
    chunks <- sits:::.chunks_create(
        block = block,
        overlap = 0,
        image_size = image_size,
        image_bbox = sits:::.bbox(
            sits:::.raster_bbox(rast_template),
            default_crs = terra::crs(rast_template)
        )
    )
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        block <- sits:::.block(chunk)
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )

        if (file.exists(block_file)) {
            return(block_file)
        }

        values <- sits:::.raster_read_rast(files = files, block = block)

        values <- C_trajectory_transition_analysis(
            data = values,
            reference_class = vs_class_id,
            neighbor_class  = perene_class_id
        )
        values <- C_trajectory_transition_analysis(
            data = values,
            reference_class = perene_class_id,
            neighbor_class  = vs_class_id
        )

        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        block_file
    }, progress = TRUE)

    sits:::.raster_merge_blocks(
        out_files = out_file,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )

    # remove block files
    unlink(block_files)

    return(out_file)
}

#' @export
reclassify_trajectory_perene_result_to_maps <- function(files, file_reclassified, version) {
    purrr::map_chr(seq_len(length(files)), function(idx) {
        file_path <- files[[idx]]
        file_out_path <- stringr::str_replace(file_path,
                                              ".tif",
                                              .reclassify_perene_filename(version))

        message("Processing: ",
                basename(file_reclassified),
                " → ",
                basename(file_out_path))

        sf::gdal_utils(
            util = "translate",
            source = as.character(fs::path_expand(file_reclassified)),
            destination = file_out_path,
            options = sits:::.gdal_params(
                list(
                    "-b"     = as.character(idx),
                    "-of"    = "GTiff",
                    "-co"    = "TILED=YES",
                    "-co"    = "COMPRESS=LZW",
                    "-co"    = "INTERLEAVE=BAND",
                    "-co"    =  "PREDICTOR=2"
                )
            ),
            quiet = FALSE
        )

        sf::gdal_addo(file_out_path)

        file_out_path
    })
}
