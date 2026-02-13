#' @title Map user-defined area unit to conversion factor from km² (default raster area unit)
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @param unit Character specifying the area unit. One of
#'             \code{"ha"} (hectares, default), \code{"km2"}
#'             (square kilometers), or \code{"m2"} (square meters).
#'
#' @returns A numeric value with the conversion factor.
#' @noRd
.to_area_factor <- function(unit) {
    switch(unit,
           "m2"  = 1e6,
           "km2" = 1,
           "ha"  = 100,
           cli::cli_abort(c(
               "Invalid unit: {.val {unit}}.",
               "i" = "Use {.val m2}, {.val km2}, or {.val ha}."
           ))
    )
}

#' @title Cross-tabulation of area between two categorical rasters
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Computes the area of intersecting pixels between two
#' categorical rasters. This is similar to \code{\link{crosstable}}, but
#' returns area instead of pixel counts. Area is computed using
#' \code{\link[raster]{area}}, which accounts for cell size variations
#' in geographic coordinate systems.
#'
#' @param map        A \code{sits} cube with the classification map.
#' @param ref        A \code{sits} cube with the reference map.
#' @param output_dir Character specifying the output directory for
#'                   intermediate files.
#' @param unit       Character specifying the area unit. One of
#'                   \code{"ha"} (hectares, default), \code{"km2"}
#'                   (square kilometers), or \code{"m2"} (square meters).
#' @param multicores Integer specifying the number of cores for parallel
#'                   processing (default: 10).
#' @param memsize    Integer specifying the maximum memory size in GB
#'                   (default: 16).
#' @param roi        Optional region of interest (ROI) to filter chunks.
#'
#' @returns A tibble with columns \code{map_values}, \code{ref_values},
#' and \code{area} (in the specified unit).
#'
#' @export
crosstable_area <- function(map, ref, output_dir, unit = "ha",
                            multicores = 10, memsize = 16, roi = NULL) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define temporary output file
    output_file <- output_dir / "area.tif"
    # Define on.exist file
    on.exit(unlink(output_file))
    # Verify ROI
    if (!is.null(roi)) {
        roi <- sits:::.roi_as_sf(roi)
    }
    # Get raster metadata
    file <- sits:::.tile_path(map)
    rast_template <- sits:::.raster_open_rast(file)
    rast_crs <- terra::crs(rast_template)
    # Map user unit to conversion factor (from km²)
    area_factor <- .to_area_factor(unit)
    # The following functions define optimal parameters for parallel processing
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
            default_crs = rast_crs
        )
    )
    # Filter chunks by ROI
    if (!is.null(roi)) {
        # How many chunks do we need to process?
        nchunks <- nrow(chunks)
        # Intersect chunks with ROI
        chunks <- sits:::.chunks_filter_spatial(
            chunks = chunks,
            roi = roi
        )
        # Update bbox to account for ROI
        update_bbox <- nrow(chunks) != nchunks
    }
    # Update chunk to save extra information
    chunks[["map"]] <- map
    chunks[["ref"]]  <- ref
    chunks[["rast_crs"]] <- rast_crs
    chunks[["area_factor"]] <- area_factor
    chunks[["output_file"]] <- output_file
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    data <- sits:::.jobs_map_parallel_dfr(chunks, function(chunk) {
        # Get job block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        map  <- chunk[["map"]]
        ref  <- chunk[["ref"]]
        rast_crs   <- chunk[["rast_crs"]]
        area_factor <- chunk[["area_factor"]]
        out_file   <- chunk[["output_file"]]
        # Output file name
        block_file <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Output mask file name
        mask_block_file <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(out_file, suffix = "_mask"),
            block = block, output_dir = output_dir
        )
        # If there is any mask file delete it
        unlink(mask_block_file)
        # Resume processing in case of failure
        if (sits:::.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Project mask block to template block
        # Get band conf missing value
        band = "class"
        band_conf <- sits:::.conf_derived_band(
            derived_class = "class_cube", band = band
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
            base_files = sits:::.fi_paths(sits:::.fi(ref)), multicores = 1L
        )
        # Build a new tile for mask based on template
        mask_tile <- sits:::.tile_derived_from_file(
            file = mask_block_file,
            band = "class",
            base_tile = sits:::.tile(ref),
            derived_class = "class_cube",
            update_bbox = FALSE
        )
        # Read and preprocess values
        map_values <- sits:::.tile_read_block(
            tile = map, band = sits:::.tile_bands(map), block = block
        )
        # Read and preprocess values of mask block
        ref_values <- sits:::.tile_read_block(
            tile = mask_tile, band = sits:::.tile_bands(mask_tile), block = NULL
        )
        # Build a raster for this block to compute area
        # using raster::area
        block_bbox <- sits:::.bbox(chunk)
        block_nrow <- block[["nrows"]]
        block_ncol <- block[["ncols"]]
        block_rast <- raster::raster(
            nrows = block_nrow, ncols = block_ncol,
            xmn = block_bbox[["xmin"]], xmx = block_bbox[["xmax"]],
            ymn = block_bbox[["ymin"]], ymx = block_bbox[["ymax"]],
            crs = rast_crs
        )
        # Encode (map_class, ref_class) pair as a single integer.
        map_vec <- as.integer(as.vector(map_values))
        ref_vec <- as.integer(as.vector(ref_values))
        # Multiplier 10000 supports class values up to 9999.
        combined <- map_vec * 10000L + ref_vec
        raster::values(block_rast) <- combined
        # Compute area per cell (km²) and aggregate by class combination.
        area_rast <- raster::area(block_rast)
        cell_vals <- raster::values(block_rast)
        cell_areas <- raster::values(area_rast)
        valid <- !is.na(cell_vals)
        area_by_value <- tapply(
            cell_areas[valid], cell_vals[valid], sum
        )
        areas <- data.frame(
            value = as.integer(names(area_by_value)),
            area = as.numeric(area_by_value) * area_factor
        )
        # Decode combined values back to (map, ref) pairs
        tbl <- data.frame(
            map_values = as.character(areas[["value"]] %/% 10000L),
            ref_values = as.character(areas[["value"]] %% 10000L),
            area = areas[["area"]]
        )
        # Delete unneeded mask block file
        unlink(mask_block_file)
        # Free memory
        gc()
        # Returned value
        tbl
    }, progress = TRUE)
    # Summarize area across blocks
    data |>
        dplyr::group_by(.data[["map_values"]], .data[["ref_values"]]) |>
        dplyr::summarise(area = sum(.data[["area"]]), .groups = "drop")
}

#' @title Named cross-tabulation of area between two categorical rasters
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Computes the area of intersecting pixels between two
#' categorical rasters and returns a named pivot table. This is similar
#' to \code{\link{crosstable_named}}, but returns area instead of pixel counts.
#'
#' @param map            A \code{sits} cube with the classification map.
#' @param reference      A \code{sits} cube with the reference map.
#' @param map_name       Character with the label for the map columns.
#' @param reference_name Character with the label for the reference rows.
#' @param unit           Character specifying the area unit. One of
#'                       \code{"ha"} (hectares, default), \code{"km2"}
#'                       (square kilometers), or \code{"m2"} (square meters).
#' @param multicores     Integer specifying the number of cores (default: 10).
#' @param memsize        Integer specifying max memory in GB (default: 16).
#' @param data_dir       Character specifying the output directory for
#'                       intermediate files. If \code{NULL}, uses \code{tempdir()}.
#' @param roi            Optional region of interest (ROI) to filter chunks.
#'
#' @returns A tibble in wide format with named categories and area values.
#'
#' @export
crosstable_area_named <- function(map, reference, map_name, reference_name,
                                  unit = "ha", multicores = 10, memsize = 16,
                                  data_dir = NULL, roi = NULL) {
    # Get maps labels
    map_labels <- sits_labels(map)
    ref_labels <- sits_labels(reference)

    current_col_name <- glue::glue("_{map_name}")
    next_col_name <- glue::glue("_{reference_name}")

    # Define directory
    if (is.null(data_dir)) {
        data_dir <- tempdir()
    }

    # Calculate area crosstable
    crosstable_area(
        map        = map,
        ref        = reference,
        output_dir = data_dir,
        unit       = unit,
        multicores = multicores,
        memsize    = memsize,
        roi        = roi
    ) |>
        dplyr::rename(
            "current" = "map_values",
            "next"    = "ref_values"
        ) |>
        dplyr::mutate(
            "current" = as.factor(.data[["current"]]),
            "next" = as.factor(.data[["next"]]),
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
            "current" = (
                stringr::str_to_title(
                    map_labels[[  levels(.data[["current"]])[.data[["current"]]]  ]]
                ) |>
                    stringr::str_c(glue::glue("_{map_name}"))
            ),
            "next"  = (
                stringr::str_to_title(
                    ref_labels[[  levels(.data[["next"]])[.data[["next"]]]  ]]
                ) |>
                    stringr::str_c(glue::glue("_{reference_name}"))
            )
        ) |>
        dplyr::rename(
            !!current_col_name := "current",
            !!next_col_name := "next"
        ) |>
        tidyr::pivot_wider(
            names_from = !!current_col_name,
            values_from = area,
            values_fill = 0
        )
}

#' @export
crosstable <- function(map, ref, output_dir, multicores = 10, memsize = 16, roi = NULL) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Verify ROI
    if (!is.null(roi)) {
        roi <- sits:::.roi_as_sf(roi)
    }

    # The following functions define optimal parameters for parallel processing
    file <- sits:::.tile_path(map)
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
    # Filter chunks by ROI
    if (!is.null(roi)) {
        # How many chunks do we need to process?
        nchunks <- nrow(chunks)
        # Intersect chunks with ROI
        chunks <- sits:::.chunks_filter_spatial(
            chunks = chunks,
            roi = roi
        )
        # Update bbox to account for ROI
        update_bbox <- nrow(chunks) != nchunks
    }

    # Update chunk to save extra information
    chunks[["map"]] <- map
    chunks[["ref"]]  <- ref
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    data <- sits:::.jobs_map_parallel_dfr(chunks, function(chunk) {
        # Get job block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        map  <- chunk[["map"]]
        ref  <- chunk[["ref"]]
        out_file <- chunk[["file"]]
        # Output file name
        block_file <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(out_file),
            block = block,
            output_dir = output_dir
        )
        # Output mask file name
        mask_block_file <- sits:::.file_block_name(
            pattern = sits:::.file_pattern(out_file, suffix = "_mask"),
            block = block, output_dir = output_dir
        )
        # If there is any mask file delete it
        unlink(mask_block_file)
        # Resume processing in case of failure
        if (sits:::.raster_is_valid(block_file)) {
            return(block_file)
        }
        # Project mask block to template block
        # Get band conf missing value
        band = "class"
        band_conf <- sits:::.conf_derived_band(
            derived_class = "class_cube", band = band
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
            base_files = sits:::.fi_paths(sits:::.fi(ref)), multicores = 1L
        )
        # Build a new tile for mask based on template
        mask_tile <- sits:::.tile_derived_from_file(
            file = mask_block_file,
            band = "class",
            base_tile = sits:::.tile(ref),
            derived_class = "class_cube",
            update_bbox = FALSE
        )
        # Read and preprocess values
        map_values <- sits:::.tile_read_block(
            tile = map, band = sits:::.tile_bands(map), block = block
        )
        # Read and preprocess values of mask block
        ref_values <- sits:::.tile_read_block(
            tile = mask_tile, band = sits:::.tile_bands(mask_tile), block = NULL
        )
        # Calculate contingency table
        tbl <- as.data.frame(
            table(map_values, ref_values)
        )
        # Delete unneeded mask block file
        unlink(mask_block_file)
        # Free memory
        gc()
        # Returned value
        tbl
    }, progress = TRUE)
    # Summarize!
    data |>
        dplyr::group_by(.data[["map_values"]], .data[["ref_values"]]) |>
        dplyr::summarise(n = sum(.data[["Freq"]]), .groups = 'drop')
}

#' @export
crosstable_named <- function(map, reference, map_name, reference_name, multicores = 10, memsize = 16, data_dir = NULL, roi = NULL) {
    # Get maps labels
    map_labels <- sits_labels(map)
    ref_labels <- sits_labels(reference)

    current_col_name <- glue::glue("_{map_name}")
    next_col_name <- glue::glue("_{reference_name}")

    # Define directory
    if (is.null(data_dir)) {
        data_dir <- tempdir()
    }

    # Calculate crosstable
    crosstable(
        map        = map,
        ref        = reference,
        multicores = multicores,
        memsize    = memsize,
        output_dir = data_dir,
        roi        = roi
    ) |>
        dplyr::rename(
            "current" = "map_values",
            "next"    = "ref_values"
        ) |>
        dplyr::mutate(
            "current" = as.factor(.data[["current"]]),
            "next" = as.factor(.data[["next"]]),
        ) |>
        dplyr::rowwise() |>
        dplyr::mutate(
            "current" = (
                stringr::str_to_title(
                    map_labels[[  levels(.data[["current"]])[.data[["current"]]]  ]]
                ) |>
                    stringr::str_c(glue::glue("_{map_name}"))
            ),
            "next"  = (
                stringr::str_to_title(
                    ref_labels[[  levels(.data[["next"]])[.data[["next"]]]  ]]
                ) |>
                    stringr::str_c(glue::glue("_{reference_name}"))
            )
        ) |>
        dplyr::rename(
            !!current_col_name := "current",
            !!next_col_name := "next"
        ) |>
        tidyr::pivot_wider(
            names_from = !!current_col_name,
            values_from = n,
            values_fill = 0
        )
}

