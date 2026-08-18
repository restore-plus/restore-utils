#' @title Adjust map geometry variations
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Adjusts the geometry variations of a map using a reference map.
#'
#' @param file_adjust Character representing the file to adjust.
#' @param file_reference Character representing the reference file.
#' @param year Integer representing the year of the map.
#' @param version Character representing the version of the map.
#' @param multicores Integer representing the number of cores to use.
#' @param memsize Integer representing the memory size to use.
#' @param output_dir Character representing the output directory.
#'
#' @returns Character representing the adjusted map.
#'
#' @keywords internal
#' @export
geom_adjust_map_variations <- function(file_adjust,
                                       file_reference,
                                       year,
                                       version,
                                       multicores,
                                       memsize,
                                       output_dir) {
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
    rast_template <- sits:::.raster_open_rast(file_reference)
    image_size <- list(
        nrows = sits:::.raster_nrows(rast_template),
        ncols = sits:::.raster_ncols(rast_template)
    )
    # Get block size
    block <- sits:::.raster_file_blocksize(sits:::.raster_open_rast(file_reference))
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = 0),
        npaths = length(file_adjust) + length(file_reference),
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
    chunks[["file_adjust"]] <- rep(list(file_adjust), nrow(chunks))
    chunks[["file_reference"]] <- rep(list(file_reference), nrow(chunks))
    # Start workers
    started <- sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(started), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        output_dir <- chunk[["output_dir"]]
        file_adjust <- chunk[["file_adjust"]][[1]]
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
            pattern = sits:::.file_pattern(file_out, suffix = "_adjust"),
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
            base_files = file_adjust, multicores = 1L
        )
        # Free memory
        gc()
        # Unlink files
        unlink(block_file)
        # Returned block file
        mask_block_file
    }, progress = TRUE)
    # Merge raster blocks
    sits:::.raster_merge_blocks(
        out_files = file_out,
        base_file = file_adjust,
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
