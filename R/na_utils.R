#' @title Replace NA values by a value in a raster file
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Replaces NA values by a value in a raster file.
#' 
#' @param file Character representing the input raster file.
#' @param year Integer representing the year of the raster file.
#' @param replace_value Integer representing the value to replace NA values.
#' @param version Character representing the version of the raster file.
#' @param multicores Integer representing the number of cores to use.
#' @param memsize Integer representing the memory size to use.
#' @param output_dir Character representing the output directory.
#' 
#' @returns Character representing the output raster file.
#' 
#' @keywords internal
#' @export
replace_na <- function(file, year, replace_value, version, multicores, memsize, output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- restoreutils:::.reclassify_sits_name(version, year)
    out_file <- fs::path(output_dir) / out_filename
    # If result already exists, return it!
    if (file.exists(out_file)) {
        return(out_file)
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
    chunks[["file"]] <- rep(list(file), nrow(chunks))
    chunks[["replace_value"]] <- replace_value
    chunks[["out_filename"]] <- rep(list(out_filename), nrow(chunks))
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        file <- chunk[["file"]][[1]]
        out_filename <- chunk[["out_filename"]]
        replace_value <- chunk[["replace_value"]]
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
        # Process data
        values[is.na(values)] <- replace_value
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
        base_file = file,
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

#' @title Clean NA values in a cube
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Cleans NA values in a cube.
#' 
#' @param cube A \code{sits} cube.
#' @param window_size Integer representing the window size.
#' @param memsize Integer representing the memory size.
#' @param multicores Integer representing the number of cores to use.
#' @param output_dir Character representing the output directory.
#' @param version Character representing the version of the cube.
#' @param roi A \code{sits} ROI.
#' @param progress Logical representing whether to show progress.
#' 
#' @returns A \code{sits} cube with the cleaned NA values.
#' 
#' @keywords internal
#' @export
na_cleaner <- function(cube,
                       window_size = 3L,
                       memsize = 4L,
                       multicores = 2L,
                       output_dir,
                       version = "v1",
                       roi = NULL,
                       progress = TRUE) {
    # Overlapping pixels
    overlap <- ceiling(window_size / 2L) - 1L
    # Get block size
    block <- sits:::.raster_file_blocksize(
        sits:::.raster_open_rast(sits:::.tile_path(cube))
    )
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = overlap),
        npaths = 1L, nbytes = 8L,
        proc_bloat = sits:::.conf("processing_bloat")
    )

    # Get input band
    band <- sits:::.cube_bands(cube)
    # Update multicores parameter
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = sits:::.tile_size(sits:::.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Spatial filter
    if (sits:::.has(roi)) {
        roi <- sits:::.roi_as_sf(roi)
    }
    # Prepare parallelization
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process each tile sequentially
    clean_cube <- sits:::.cube_foreach_tile(cube, function(tile) {
        # Process the data
        .na_cleaner(
            tile = tile,
            block = block,
            band = band,
            window_size = window_size,
            overlap = overlap,
            output_dir = output_dir,
            version = version,
            progress = progress,
            roi = roi
        )
    })
    # Update cube class and return
    sits:::.set_class(clean_cube, "class_cube", class(clean_cube))
}

#' @title Clean NA values in a tile
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Cleans NA values in a tile.
#' 
#' @param tile A \code{sits} tile.
#' @param block A \code{sits} block.
#' @param band Character representing the band to clean.
#' @param roi A \code{sits} ROI.
#' @param window_size Integer representing the window size.
#' @param overlap Integer representing the overlap.
#' @param output_dir Character representing the output directory.
#' @param version Character representing the version of the tile.
#' @param progress Logical representing whether to show progress.
#' 
#' @returns A \code{sits} tile with the cleaned NA values.
#' 
#' @keywords internal
#' @export
.na_cleaner <- function(tile,
                        block,
                        band,
                        roi,
                        window_size,
                        overlap,
                        output_dir,
                        version,
                        progress) {
    update_bbox <- FALSE
    # Output file
    out_file <- sits:::.file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume tile
    if (sits:::.raster_is_valid(out_file, output_dir = output_dir)) {
        # recovery message
        sits:::.check_recovery()
        # Create tile based on template
        tile <- sits:::.tile_derived_from_file(
            file = out_file, band = band,
            base_tile = tile, derived_class = sits:::.tile_derived_class(tile),
            labels = sits:::.tile_labels(tile),
            update_bbox = FALSE
        )
        return(tile)
    }
    # Create chunks as jobs
    chunks <- sits:::.tile_chunks_create(tile = tile, overlap = overlap, block = block)
    # Filter chunks
    if (sits:::.has(roi)) {
        # How many chunks there are in tile?
        nchunks <- nrow(chunks)
        # Remove chunks within the exclusion mask
        chunks <- sits:::.chunks_filter_mask(
            chunks = chunks,
            mask = roi
        )
        # Create crop region
        chunks["mask"] <- sits:::.chunks_crop_mask(
            chunks = chunks,
            mask = roi
        )
        # Should bbox of resulting tile be updated?
        update_bbox <- nrow(chunks) != nchunks
    }
    # Process jobs sequentially
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get job block
        block <- sits:::.block(chunk)
        # Block file name for each fraction
        block_files <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (sits:::.raster_is_valid(block_files)) {
            return(block_files)
        }
        # Read bands data
        values <- sits:::.clean_data_read(
            tile = tile, block = block, band = band
        )
        # Apply kernel modal
        values <- restoreutils:::C_na_cleaner(
            x = as.matrix(values),
            ncols = block[["ncols"]],
            nrows = block[["nrows"]],
            band = 0L,
            window_size = window_size
        )
        # Prepare fractions to be saved
        band_conf <- sits:::.tile_band_conf(tile = tile, band = band)
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_files, block = block, bbox = sits:::.bbox(chunk),
            values = values, data_type = sits:::.data_type(band_conf),
            missing_value = sits:::.miss_value(band_conf),
            crop_block = chunk[["mask"]]
        )
        # Free memory
        gc()
        # Returned block files for each fraction
        block_files
    }, progress = progress)
    # Merge blocks into a new class_cube tile
    sits:::.tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = sits:::.tile_labels(tile),
        base_tile = tile,
        derived_class = sits:::.tile_derived_class(tile),
        block_files = block_files,
        multicores = 1L,
        update_bbox = update_bbox
    )
}

#' @title Clean contextual NA values in a cube
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Cleans contextual NA values in a cube.
#' 
#' @param cube A \code{sits} cube.
#' @param window_size Integer representing the window size.
#' @param target_class Integer representing the target class.
#' @param mode_class Integer representing the mode class.
#' @param memsize Integer representing the memory size.
#' @param multicores Integer representing the number of cores to use.
#' @param output_dir Character representing the output directory.
#' @param version Character representing the version of the cube.
#' @param progress Logical representing whether to show progress.
#' 
#' @returns A \code{sits} cube with the cleaned contextual NA values.
#' 
#' @keywords internal
#' @export
contextual_cleaner <- function(cube,
                               window_size = 3L,
                               target_class = 1L,
                               mode_class = 2L,
                               memsize = 4L,
                               multicores = 2L,
                               output_dir,
                               version = "v1",
                               progress = TRUE) {
    stopifnot(!is.null(target_class))
    stopifnot(!is.null(mode_class))
    # Overlapping pixels
    overlap <- ceiling(window_size / 2L) - 1L
    # Get block size
    block <- sits:::.raster_file_blocksize(
        sits:::.raster_open_rast(sits:::.tile_path(cube))
    )
    # Check minimum memory needed to process one block
    job_block_memsize <- sits:::.jobs_block_memsize(
        block_size = sits:::.block_size(block = block, overlap = overlap),
        npaths = 1L, nbytes = 8L,
        proc_bloat = sits:::.conf("processing_bloat")
    )
    # Get input band
    band <- sits:::.cube_bands(cube)
    # Update multicores parameter
    multicores <- sits:::.jobs_max_multicores(
        job_block_memsize = job_block_memsize,
        memsize = memsize,
        multicores = multicores
    )
    # Update block parameter
    block <- sits:::.jobs_optimal_block(
        job_block_memsize = job_block_memsize,
        block = block,
        image_size = sits:::.tile_size(sits:::.tile(cube)),
        memsize = memsize,
        multicores = multicores
    )
    # Prepare parallelization
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)

    # Process each tile sequentially
    clean_cube <- sits:::.cube_foreach_tile(cube, function(tile) {
        # Process the data
        .contextual_cleaner_tile(
            tile = tile,
            block = block,
            band = band,
            window_size = window_size,
            target_class = target_class,
            mode_class = mode_class,
            overlap = overlap,
            output_dir = output_dir,
            version = version,
            progress = progress
        )
    })
    # Update cube class and return
    sits:::.set_class(clean_cube, "class_cube", class(clean_cube))
}

#' @title Clean contextual NA values in a tile
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Cleans contextual NA values in a tile.
#' 
#' @param tile A \code{sits} tile.
#' @param block A \code{sits} block.
#' @param band Character representing the band to clean.
#' @param window_size Integer representing the window size.
#' @param target_class Integer representing the target class.
#' @param mode_class Integer representing the mode class.
#' @param overlap Integer representing the overlap.
#' @param output_dir Character representing the output directory.
#' @param version Character representing the version of the tile.
#' @param progress Logical representing whether to show progress.
#' 
#' @returns A \code{sits} tile with the cleaned contextual NA values.
#' 
#' @keywords internal
#' @export
#' @param tile A \code{sits} tile.
.contextual_cleaner_tile <- function(tile,
                                     block,
                                     band,
                                     window_size,
                                     target_class,
                                     mode_class,
                                     overlap,
                                     output_dir,
                                     version,
                                     progress) {
    # Output file
    out_file <- sits:::.file_derived_name(
        tile = tile, band = band, version = version, output_dir = output_dir
    )
    # Resume tile
    if (sits:::.raster_is_valid(out_file, output_dir = output_dir)) {
        # recovery message
        sits:::.check_recovery()
        # Create tile based on template
        tile <- sits:::.tile_derived_from_file(
            file = out_file, band = band,
            base_tile = tile, derived_class = sits:::.tile_derived_class(tile),
            labels = sits:::.tile_labels(tile),
            update_bbox = FALSE
        )
        return(tile)
    }
    # Create chunks as jobs
    chunks <- sits:::.tile_chunks_create(tile = tile, overlap = overlap, block = block)
    # Process jobs sequentially
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get job block
        block <- sits:::.block(chunk)
        # Block file name for each fraction
        block_files <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Resume processing in case of failure
        if (sits:::.raster_is_valid(block_files)) {
            return(block_files)
        }
        # Read bands data
        values <- sits:::.clean_data_read(
            tile = tile, block = block, band = band
        )
        # Apply kernel modal
        values <- restoreutils:::C_context_cleaner(
            x = as.matrix(values),
            ncols = block[["ncols"]],
            nrows = block[["nrows"]],
            band = 0L,
            window_size = window_size,
            target_class = target_class,
            mode_class = mode_class
        )
        # Prepare fractions to be saved
        band_conf <- sits:::.tile_band_conf(tile = tile, band = band)
        # Job crop block
        crop_block <- sits:::.block(sits:::.chunks_no_overlap(chunk))
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_files, block = block, bbox = sits:::.bbox(chunk),
            values = values, data_type = sits:::.data_type(band_conf),
            missing_value = sits:::.miss_value(band_conf),
            crop_block = crop_block
        )
        # Free memory
        gc()
        # Returned block files for each fraction
        block_files
    }, progress = progress)
    # Merge blocks into a new class_cube tile
    sits:::.tile_derived_merge_blocks(
        file = out_file,
        band = band,
        labels = sits:::.tile_labels(tile),
        base_tile = tile,
        derived_class = sits:::.tile_derived_class(tile),
        block_files = block_files,
        multicores = 1L,
        update_bbox = FALSE
    )
}

#' @title Detect chunks with NA values
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Detects chunks with NA values.
#' 
#' @param files Character representing the input raster files.
#' @param version Character representing the version of the files.
#' @param multicores Integer representing the number of cores to use.
#' @param memsize Integer representing the memory size.
#' @param output_dir Character representing the output directory.
#' 
#' @returns Character representing the output raster file.
#' 
#' @keywords internal
#' @export
detect_chunks_with_na <- function(files, version, multicores, memsize, output_dir) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    out_filename <- .reclassify_perene_filename(version)
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
    # Fixed block
    block <- c(
        nrows = 2048,
        ncols = 2048
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
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        files <- chunk[["files"]][[1]]
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
        # If there is any NA, save chunk
        if (any(is.na(values))) {
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
            # Returned block file
            return(block_file)
        }
        # Free memory
        gc()
        # Return
        NULL
    }, progress = TRUE)
    # Remove NA values
    block_files <- block_files[!is.na(block_files)]
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
