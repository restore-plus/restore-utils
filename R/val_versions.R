#' @title Compare versions of a classification cube
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Compares the versions of a classification cube.
#'
#' @param file          Character path for the classification cube file.
#' @param file_mask     Character path for the mask cube file.
#' @param year          Integer specifying the year to compare.
#' @param target_class_id Integer specifying the target class ID.
#' @param target_class_id_mask Integer specifying the target class ID of the mask.
#' @param version         Character specifying the version to compare.
#' @param multicores      Integer specifying the number of cores for parallel
#'                        processing (default: 10).
#' @param memsize         Integer specifying the memory size for parallel
#'                        processing (default: 16).
#' @param output_dir      Character path for the output directory.
#'
#' @returns A character specifying the output file.
#' 
#' @export
validation_compare_versions <- function(file, file_mask, year, target_class_id, target_class_id_mask, version, multicores = 10, memsize = 16, output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- .reclassify_sits_name(version, year)
    file_out <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(file_out)) {
        return(file_out)
    }
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
        npaths = length(file) + length(file_mask),
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
    chunks[["output_dir"]] <- output_dir
    chunks[["file"]] <- rep(list(file), nrow(chunks))
    chunks[["file_mask"]] <- rep(list(file_mask), nrow(chunks))
    chunks[["target_class_id"]] <- target_class_id
    chunks[["target_class_id_mask"]] <- target_class_id_mask
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        output_dir <- chunk[["output_dir"]]
        file <- chunk[["file"]][[1]]
        file_mask <- chunk[["file_mask"]][[1]]
        target_class_id <- chunk[["target_class_id"]]
        target_class_id_mask <- chunk[["target_class_id_mask"]]
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
        # Output mask file name
        mask_block_file <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(file_out, suffix = "_mask"),
            block = block, output_dir = output_dir
        )
        # Project mask block to template block
        # Get band conf missing value
        band_conf <- sits:::.conf_derived_band(
            derived_class = "class_cube", band = "class"
        )
        # Create template block for mask
        sits:::.gdal_template_block(
            block = block, bbox = sits:::.bbox(chunk), file = mask_block_file,
            nlayers = 1L, miss_value = sits:::.miss_value(band_conf),
            data_type = sits:::.data_type(band_conf)
        )
        # Copy values from mask cube into mask template
        sits:::.gdal_merge_into(
            file = mask_block_file,
            base_files = file_mask, multicores = 1L
        )
        # Read raster values
        values <- sits:::.raster_read_rast(
            files = file,
            block = block
        )
        values_mask <- sits:::.raster_read_rast(
            files = mask_block_file,
            block = NULL
        )
        # Process data
        values <- restoreutils:::C_validation_compare_versions(
            data        = values,
            mask        = values_mask,
            data_class  = target_class_id,
            mask_class  = target_class_id_mask
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
        # Unlink files
        unlink(mask_block_file)
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
