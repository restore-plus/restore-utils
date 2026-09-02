#' @title Reclassify temporal consistency using reference class
#' 
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' 
#' @description The function checks over time whether a class is 
#'              present and, if so, reclassifies previous years 
#'              to the target class. This temporal consistency is useful for 
#'              classes that can not be presented in the future if they are not 
#'              present in the past (e.g., forest in INPE-PRODES maps, we can't 
#'              have something else in the past and forest in the future as we are 
#'              not creating original forest).
#' 
#' @param files Character vector of input files
#' @param reference_id Integer representing the reference class id
#' @param target_id Integer representing the target class id
#' @param version Character representing the version
#' @param multicores Integer representing the number of cores
#' @param memsize Integer representing the memory size
#' @param output_dir Character representing the output directory
#' 
#' @returns Character representing the output file
#' 
#' @keywords internal
#' @export
.reclassify_temporal_consistency_reference <- function(files,
                                                       reference_id,
                                                       target_id,
                                                       version,
                                                       multicores,
                                                       memsize,
                                                       output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- .reclassify_temp_filename(version)
    out_file <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(out_file)) {
        return(out_file)
    }
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(files)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(files))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(files),
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
    # Update chunk to save extra information
    chunks[["files"]] <- rep(list(files), nrow(chunks))
    chunks[["out_filename"]] <- out_filename
    chunks[["reference_id"]] <- reference_id
    chunks[["target_id"]] <- target_id
    # Start workers
    started <- sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(started), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        files <- chunk[["files"]][[1]]
        out_filename <- chunk[["out_filename"]]
        target_id <- chunk[["target_id"]][[1]]
        reference_id <- chunk[["reference_id"]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Read raster values
        values <- sits:::.raster_read_rast(files = files, block = block)
        # Call trajectory function in cpp
        values <- restoreutils:::C_trajectory_temporal_consistency_reference(
            data = values,
            reference_class = reference_id,
            target_class = target_id
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = out_file,
        base_file = files,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(out_file)
}

#' @title Extract temporal results in brick format to individual maps
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description The function extracts the temporal results in brick format to individual maps by year.
#' 
#' @param years Integer vector of years
#' @param output_dir Character representing the output directory
#' @param file_brick Character representing the input brick file
#' @param version Character representing the version of the classification
#' 
#' @returns Character vector of output files
#' 
#' @keywords internal
#' @export
reclassify_temporal_results_to_maps <- function(years, output_dir, file_brick, version = "v1") {
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)

    purrr::map_chr(seq_len(length(years)), function(idx) {
        year <- years[idx]
        out_file <- .reclassify_sits_name(version, year)
        out_file <- output_dir / out_file

        message("Processing: ", year)

        sf::gdal_utils(
            util = "translate",
            source = as.character(fs::path_expand(file_brick)),
            destination = as.character(fs::path_expand(out_file)),
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

        sf::gdal_addo(out_file)

        out_file
    })
}

#' @title Remap pixels
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description The function remaps the pixels of a raster file from a 
#'              source to a target based on a set of rules.
#' 
#' @param file Character representing the input file
#' @param file_out Character representing the output file
#' @param rules tibble with columns `source` and `target` representing the 
#'              remapping rules
#' @param multicores Integer representing the number of cores
#' @param memsize Integer representing the memory size
#' @param output_dir Character representing the output directory
#' 
#' @returns Character representing the output file
#' 
#' @export
reclassify_remap_pixels <- function(file,
                                    file_out,
                                    rules,
                                    multicores,
                                    memsize,
                                    output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # If result already exists, return it!
    if (file.exists(file_out)) {
        return(file_out)
    }
    out_filename <- fs::path_file(file_out)
    # The following functions define optimal parameters for parallel processing
    rast_template <- sits:::.raster_open_rast(file)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(file))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(file),
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
    # Update chunk to save extra information
    chunks[["file"]] <- file
    chunks[["rules"]] <- list(rules)
    # Start workers
    started <- sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(started), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        file <- chunk[["file"]]
        rules <- chunk[["rules"]][[1]]
        # Define block file name / path
        block_file <- sits:::.file_block_name(
            pattern = tools::file_path_sans_ext(out_filename),
            block = block,
            output_dir = output_dir
        )
        # If block already exists, return it!
        if (file.exists(block_file)) {
            return(block_file)
        }
        # Read raster values
        values <- sits:::.raster_read_rast(files = file, block = block)
        for (rule_idx in seq_len(nrow(rules))) {
            rule <- rules[rule_idx,]

            values <- restoreutils:::C_remap_values(
                data   = values,
                source = rule[["source"]],
                target = rule[["target"]]
            )
        }
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 255,
            crop_block = NULL
        )
        # Free memory
        gc()
        # Returned block file
        block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = file_out,
        base_file = file,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 255,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(file_out)
}
