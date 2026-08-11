#' @export
prepare_terraclass_amazon <- function(years, region_id, fix_non_observed = TRUE,
                                      fix_other_uses = TRUE, fix_urban_area = TRUE,
                                      fix_non_forest = TRUE, add_non_forest = TRUE,
                                      memsize = 8, multicores = 1, timeout = 720,
                                      version = "v1", exclude_mask_na = FALSE) {
    # Download all specified years
    extracted_files <- purrr::map_dfr(years, function(year) {
        # Set timeout
        withr::local_options(timeout = timeout)

        # Define output dir
        output_dir <- .terraclass_dir(year = year, version = version)

        # Create directory
        fs::dir_create(output_dir)

        # Download year file
        output_file <- .terraclass_download(
            region = "amazon",
            year = year,
            output_dir = output_dir
        )

        # Extract files from zip
        extracted_files <- .terraclass_extract_files(year       = year,
                                                     file       = output_file,
                                                     output_dir = output_dir)

        # Check if files are already processed
        are_files_finished <- all(unname(extracted_files[["processed"]]))

        # If no, crop raster using the eco region selected by the user
        if (!are_files_finished) {
            # Get eco region polygon
            eco_region_roi <- roi_amazon_regions(
                region_id  = region_id,
                crs        = "EPSG:4674",
                as_union   = TRUE,
                as_file    = TRUE,
                use_buffer = TRUE
            )

            eco_region_roi <- terra::vect(eco_region_roi)

            # Select raster file
            raster_file <- dplyr::filter(extracted_files, type == "raster")
            raster_file <- raster_file[["file"]]

            # Define temporary file
            raster_file_out <- stringr::str_replace(
                raster_file, "v1.tif", "v1-cropped.tif"
            )

            raster_object <- terra::rast(raster_file)
            raster_datatype <- terra::datatype(raster_object)

            # Reproject polygon to raster CRS
            vector_object <- terra::project(
                x = eco_region_roi,
                y = terra::crs(raster_object)
            )

            # Crop raster files in a given eco region
            terra::crop(
                x        = raster_object,
                y        = vector_object,
                filename = raster_file_out,
                datatype = raster_datatype,
                NAflag   = 0,
                mask     = TRUE
            )

            # After crop, remove original one
            fs::file_delete(raster_file)

            # Renamed cropped
            fs::file_move(raster_file_out, raster_file)
        }

        # Return!
        dplyr::select(extracted_files, -.data[["processed"]]) |>
            dplyr::mutate(year = !!year)
    })
    # Add non-forest into terraclass 2024
    if (add_non_forest) {
        if (2024 %in% years) {
            # Load PRODES mask
            mask <- load_prodes_2024(
                multicores = multicores, memsize = memsize
            )
            # Creating cube
            current_cube <- load_terraclass_2024(
                memsize = memsize, multicores = multicores
            )
            # Define output dir
            output_dir <- .terraclass_dir(year = 2024, version = version)
            # Add non-forest class
            reclassified_cube <- sits::sits_reclassify(
                cube = current_cube,
                mask = mask,
                rules = list(
                    "NAO FLORESTA" = mask == "NAO FLORESTA"
                ),
                memsize = memsize,
                multicores = multicores,
                output_dir = output_dir,
                exclude_mask_na = exclude_mask_na,
                version = "v1-mask-non-forest-mask"
            )
            # Move created file
            fs::file_move(
                path = reclassified_cube[["file_info"]][[1]][["path"]],
                new_path = current_cube[["file_info"]][[1]][["path"]]
            )
        }
    }

    # Fix non observed
    if (fix_non_observed) {
        # Show process information
        cli::cli_inform("Mask update: Fixing non-observed")

        # Define valid years
        valid_years <- c(2024, 2022, 2020, 2018, 2014, 2012, 2010, 2008)

        stopifnot(all(valid_years %in% years))

        purrr::map(seq(1, length(valid_years)), function(year_idx) {
            current_year <- valid_years[year_idx]
            lhs <- NULL
            rhs <- NULL
            if (year_idx == 1) {
                rhs <- valid_years[year_idx + 1]

                rhs <- get(paste0("load_terraclass_amazon_", rhs))
                rhs <- rhs(
                    memsize = memsize, multicores = multicores
                )
            } else if (year_idx == length(valid_years)) {
                lhs <- valid_years[year_idx - 1]

                lhs <- get(paste0("load_terraclass_amazon_", lhs))
                lhs <- lhs(
                    memsize = memsize, multicores = multicores
                )
            } else {
                lhs <- valid_years[year_idx - 1]
                rhs <- valid_years[year_idx + 1]

                lhs <- get(paste0("load_terraclass_amazon_", lhs))
                lhs <- lhs(
                    memsize = memsize, multicores = multicores
                )

                rhs <- get(paste0("load_terraclass_amazon_", rhs))
                rhs <- rhs(
                    memsize = memsize, multicores = multicores
                )
            }

            # Define output dir
            output_dir <- .terraclass_dir(year = current_year, version = "temp")
            fs::dir_create(output_dir)

            # Creating cube
            current_cube <- get(paste0("load_terraclass_amazon_", current_year))
            current_cube <- current_cube(
                memsize = memsize, multicores = multicores
            )

            # Define files
            if (!is.null(lhs)) {
                files <- c(
                    sits:::.fi_path(sits:::.fi(current_cube)),
                    sits:::.fi_path(sits:::.fi(lhs))
                )
            } else if (!is.null(rhs)) {
                files <- c(
                    sits:::.fi_path(sits:::.fi(current_cube)),
                    sits:::.fi_path(sits:::.fi(rhs))
                )
            } else {
                files <- c(
                    sits:::.fi_path(sits:::.fi(lhs)),
                    sits:::.fi_path(sits:::.fi(current_cube)),
                    sits:::.fi_path(sits:::.fi(rhs))
                )
            }

            # Reclassify cube
            reclassified_cube <- .update_urban_mask(
                files = files,
                urban_id = 17,
                nb_id = 25,
                output_dir = output_dir,
                multicores = multicores,
                memsize = memsize,
                version = version
            )

            # Replace original cube file
            fs::file_move(
                path = reclassified_cube,
                new_path = current_cube[["file_info"]][[1]][["path"]]
            )

            # Delete temporary output directory
            fs::dir_delete(output_dir)
        })
    }

    # Fix urban area
    if (fix_urban_area) {
        # Show process information
        cli::cli_inform("Mask update: Fixing urban area")

        # Define valid years
        valid_years <- c(2024, 2022, 2020, 2018, 2014, 2012, 2010, 2008)

        stopifnot(all(valid_years %in% years))

        # Sorting years
        sorted_years <- sort(valid_years, decreasing = TRUE)

        purrr::map(seq(2, length(sorted_years)), function(year_idx) {
            # Define year
            current_year <- sorted_years[year_idx]
            previous_year <- sorted_years[year_idx - 1]

            # Define output dir
            output_dir <- .terraclass_dir(year = current_year, version = version)

            # Creating cube
            current_cube <- get(paste0("load_terraclass_amazon_", current_year))
            current_cube <- current_cube(
                memsize = memsize, multicores = multicores
            )

            # Creating mask
            mask <- get(paste0("load_terraclass_amazon_", previous_year))
            mask <- mask(
                memsize = memsize, multicores = multicores
            )

            reclassified_cube <- sits::sits_reclassify(
                cube = current_cube,
                mask = mask,
                rules = list(
                    "NAO-URBANO" = cube == "URBANIZADA" & mask != "URBANIZADA"
                ),
                memsize = memsize,
                multicores = multicores,
                output_dir = output_dir,
                exclude_mask_na = exclude_mask_na,
                version = "v1-mask-urban-area"
            )

            fs::file_move(
                path = reclassified_cube[["file_info"]][[1]][["path"]],
                new_path = current_cube[["file_info"]][[1]][["path"]]
            )
        })
    }

    # Fix non Forest
    if (fix_non_forest) {
        # Show process information
        cli::cli_inform("Mask update: Fixing non forest mask")

        valid_years <- c(2008, 2010, 2012, 2014, 2018)

        stopifnot(all(valid_years %in% years))

        years_to_apply <- c(2008, 2010, 2012, 2014)

        tc_2014 <- load_terraclass_amazon_2014(multicores = multicores, memsize = memsize)
        tc_2018 <- load_terraclass_amazon_2018(multicores = multicores, memsize = memsize)

        # Define output dir
        output_dir_temp <- .terraclass_dir(year = 2018, version = "non-forest")

        # Create output directory
        fs::dir_create(output_dir_temp)

        # Define output dir
        temp_cube <- sits::sits_reclassify(
            cube = tc_2018,
            mask = tc_2014,
            rules = list(
                "URBANIZADA-NON-FOREST" = cube == "URBANIZADA" & mask == "NAO FLORESTA"
            ),
            memsize = memsize,
            multicores = multicores,
            output_dir = output_dir_temp,
            exclude_mask_na = exclude_mask_na,
            version = "temp-mask"
        )

        purrr::map(years_to_apply, function(year_to_apply) {
            # Define output dir
            output_dir <- .terraclass_dir(year = year_to_apply, version = version)

            # Create output dir
            fs::dir_create(output_dir)

            # Creating cube
            current_cube <- get(paste0("load_terraclass_amazon_", year_to_apply))
            current_cube <- current_cube(
                memsize = memsize, multicores = multicores
            )

            reclassified_cube <- sits::sits_reclassify(
                cube = current_cube,
                mask = temp_cube,
                rules = list(
                    "URBANIZADA" = cube == "URBANIZADA" | mask == "URBANIZADA-NON-FOREST"
                ),
                memsize = memsize,
                multicores = multicores,
                output_dir = output_dir,
                exclude_mask_na = exclude_mask_na,
                version = "v2-mask-non-forest"
            )

            fs::file_move(
                path = reclassified_cube[["file_info"]][[1]][["path"]],
                new_path = current_cube[["file_info"]][[1]][["path"]]
            )
        })
        # Delete temporary directory
        fs::dir_delete(output_dir_temp)
    }

    # Fix other uses
    if (fix_other_uses) {
        # Show process information
        cli::cli_inform("Mask update: Fixing other uses mask")

        valid_years <- c(2014, 2012, 2010, 2008)

        stopifnot(all(valid_years %in% years))

        # Years to apply
        years_to_apply <- c(2008, 2010, 2012)

        tc_2014 <- load_terraclass_amazon_2014(multicores = multicores, memsize = memsize)

        purrr::map(years_to_apply, function(year_to_apply) {
            # Define output dir
            output_dir <- .terraclass_dir(year = year_to_apply, version = version)

            # Create output dir
            fs::dir_create(output_dir)

            # Creating cube
            current_cube <- get(paste0("load_terraclass_amazon", year_to_apply))
            current_cube <- current_cube(
                memsize = memsize, multicores = multicores
            )

            reclassified_cube <- sits::sits_reclassify(
                cube = current_cube,
                mask = tc_2014,
                rules = list(
                    "URBANIZADA" = cube == "URBANIZADA" | cube == "OUTROS USOS" & mask == "URBANIZADA"
                ),
                memsize = memsize,
                multicores = multicores,
                output_dir = output_dir,
                exclude_mask_na = exclude_mask_na,
                version = "v3-mask-other-uses"
            )

            fs::file_move(
                path = reclassified_cube[["file_info"]][[1]][["path"]],
                new_path = current_cube[["file_info"]][[1]][["path"]]
            )
        })
    }

    extracted_files
}

.update_urban_mask <- function(files, urban_id, nb_id, output_dir, multicores, memsize, version) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)
    # Define output file
    cur_file <- ifelse(length(files) == 3, files[[2]], files[[1]])

    out_filename <- fs::path_file(cur_file)
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
    chunks[["urban_id"]] <- urban_id
    chunks[["nb_id"]] <- nb_id
    # Start workers
    sits:::.parallel_start(workers = multicores)
    on.exit(sits:::.parallel_stop(), add = TRUE)
    # Process data!
    block_files <- sits:::.jobs_map_parallel_chr(chunks, function(chunk) {
        # Get chunk block
        block <- sits:::.block(chunk)
        # Get extra context defined by restoreutils
        files <- chunk[["files"]][[1]]
        out_filename <- chunk[["out_filename"]]
        urban_id <- chunk[["urban_id"]]
        nb_id <- chunk[["nb_id"]]
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
        # Process data
        values <- restoreutils:::C_urban_transition(
            data = values,
            urban_id = urban_id,
            nb_id  = nb_id
        )
        # Prepare and save results as raster
        sits:::.raster_write_block(
            files = block_file,
            block = block,
            bbox = sits:::.bbox(chunk),
            values = values,
            data_type = "INT1U",
            missing_value = 0,
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
        base_file = cur_file,
        block_files = block_files,
        data_type = "INT1U",
        missing_value = 0,
        multicores = multicores
    )
    # Remove block files
    unlink(block_files)
    # Return!
    return(out_file)
}
