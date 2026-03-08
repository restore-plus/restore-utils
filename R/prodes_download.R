
.prodes_files <- function() {
    base_files <- c(
        "https://www.dropbox.com/scl/fi/dfb8f8q7ebetlc4g6ez6n/prodes_amazonia_legal_2024.zip?rlkey=ue9n4i5gsqyjp3jmpw9jeos64&dl=1",
        "https://www.dropbox.com/scl/fi/qfbx8cco2mv13ysutestq/prodes_amazonia_2024.zip?rlkey=w6bhzqpenx7s29zhxxa6reeio&dl=1",
        "https://www.dropbox.com/scl/fi/r5rza5i0284p3682itwfh/prodes_amazonia_2000.zip?rlkey=jkwyj56clsy92sqd76glxf0wp&dl=1",
        "https://www.dropbox.com/scl/fi/6lg8ec2d41q97b8f67gfp/prodes_amazonia_2001.zip?rlkey=sjn0iag7wj1umeftv43me1dvl&dl=1",
        "https://www.dropbox.com/scl/fi/6rh8obv0qha13yl6ru6nu/prodes_amazonia_2002.zip?rlkey=5bmox0l191dqh8mrdslidhyed&dl=1",
        "https://www.dropbox.com/scl/fi/cfbte1hfquo409ke1xj7p/prodes_amazonia_2003.zip?rlkey=4rwv6xhi9rr83sskng3kqn4zq&dl=1",
        "https://www.dropbox.com/scl/fi/vjh4nss6hcd5onm73cp2i/prodes_amazonia_2004.zip?rlkey=v0nh8pbb28v4sl05ze6vxw1wg&dl=1",
        "https://www.dropbox.com/scl/fi/a90qrbsf34sftu06w2vp1/prodes_amazonia_2005.zip?rlkey=l6o9ceuntl5bim46py5hk0hyb&dl=1",
        "https://www.dropbox.com/scl/fi/4tfrhm4q7vsdor1j8tt3a/prodes_amazonia_2006.zip?rlkey=ucq604mtgc3z3cjnut2zd8e36&dl=1",
        "https://www.dropbox.com/scl/fi/ufr60nf6gs54exzsp28r2/prodes_amazonia_2007.zip?rlkey=m56evjijjhlc05xz0un5envss&dl=1"
    )

    base_tbl <- tibble::tibble(
        file = base_files,
        base = "prodes",
        type = c("nf", rep("map", 9)),
        year = c(2024, 2024, 2000:2007)
    )

    years <- 2000:2022
    map_files <- glue::glue(
        "https://restore-plus.s3.us-east-1.amazonaws.com/zenodo/amazon/v2/raster/{years}/LANDSAT_OLI_MOSAIC_{years}-01-01_{years}-12-31_class_v1.tif"
    )
    map_tbl <- tibble::tibble(
        file = map_files,
        base = "restore",
        type = "map",
        year = years
    )

    dplyr::bind_rows(base_tbl, map_tbl)
}

.prodes_file_metadata <- function(year, type = "map", base = "prodes") {
    file_selected <- .prodes_files() |>
        dplyr::filter(.data[["year"]] == !!year) |>
        dplyr::filter(.data[["type"]] == !!type) |>
        dplyr::filter(.data[["base"]] == !!base)

    # Sanity check: We need to have one file per year
    if (nrow(file_selected) != 1) {
        cli::cli_abort("{year} is invalid and is not available as a Prodes version - Only 2024 is available")
    }

    # Return!
    file_selected
}

.prodes_file_zip <- function(prd_year_file) {
    fs::path_file(prd_year_file[["file"]])
}

.prodes_file_sits_name <- function(year, ext) {
    template <- "LANDSAT_TM-ETM-OLI_MOSAIC_XYZ-01-01_XYZ-12-31_class_v1.EXT"

    stringr::str_replace_all(template, "XYZ", as.character(year)) |>
        stringr::str_replace("EXT", ext)
}

.prodes_download <- function(year, output_dir, type = "map", base = "prodes") {
    # Get file metadata of the selected file year
    prd_year_file <- .prodes_file_metadata(
        year = year, type = type, base = base
    )

    # Define output dir
    output_file <- fs::path(output_dir) / .prodes_file_zip(prd_year_file)

    # If file already exists, return it!
    if (fs::file_exists(output_file)) {
        return(output_file)
    }

    # Download file
    download.file(prd_year_file[["file"]], output_file)

    # Return new file
    output_file
}

.prodes_extract_files <- function(year, file, output_dir) {
    # Fix output type
    output_dir <- fs::path(output_dir)

    if (fs::path_ext(file) == "tif") {
        return(
            tibble::tibble(
                file      = file,
                type      = "raster",
                processed = TRUE
            )
        )
    }

    # Get files to be extracted
    files_zip <- utils::unzip(file, list = TRUE) |>
        dplyr::rename("name"   = "Name",
                      "length" = "Length",
                      "date"   = "Date") |>
        dplyr::filter(
            stringr::str_detect(.data[["name"]], stringr::str_c(c(".qml", ".tif"), collapse = "|"))
        ) |>
        dplyr::mutate(
            file      = output_dir / .data[["name"]],
            file_ext  = ifelse(fs::path_ext(.data[["name"]]) == "qml", "style", "raster"),
            file_sits = output_dir / .terraclass_file_sits_name(
                year,
                ext = (
                    fs::path_ext(.data[["name"]]) |>
                        stringr::str_replace("tiff", "tif")
                )
            )
        ) |>
        dplyr::mutate(
            file_available = fs::file_exists(.data[["file_sits"]])
        )

    # Check if raster file is available
    if (all(unname(files_zip[["file_available"]]))) {
        return(
            tibble::tibble(
                file      = files_zip[["file_sits"]],
                type      = files_zip[["file_ext"]],
                processed = TRUE
            )
        )
    }

    # Extract files
    utils::unzip(file, exdir = output_dir)

    # Rename files
    purrr::map_dfr(seq_len(nrow(files_zip)), function(idx) {
        # Get file metadata
        file_row <- files_zip[idx, ]

        # Rename file
        fs::file_move(file_row[["file"]], file_row[["file_sits"]])

        tibble::tibble(
            file      = file_row[["file_sits"]],
            type      = file_row[["file_ext"]],
            processed = FALSE
        )
    })
}

.prodes_nf_extras_mask <- function(year, multicores, memsize, output_dir) {
    # Define output directory
    output_dir_nonforest <- output_dir / "non-forest" / year

    # Create output directory
    fs::dir_create(output_dir_nonforest)

    # Check non-forest file
    nonforst_file <- .prodes_nonforest_output_file(
        year       = year,
        output_dir = output_dir_nonforest
    )

    if (!fs::file_exists(nonforst_file)) {
        # Download non-forest data
        prodes_nonforest <- .prodes_nonforest_download(output_dir_nonforest)

        # Filter by year
        prodes_nonforest <- prodes_nonforest |>
            dplyr::filter(.data[["year"]] <= !!year)

        # Rasterize non-forest
        .prodes_nonforest_rasterize(
            prodes        = prodes_nonforest,
            year          = year,
            output_dir    = output_dir_nonforest,
            class_id      = 1 # as this is yearly - it is ok to keep 1
        )
    }

    # Load non-forest as cube
    sits::sits_cube(
        source     = "MPC",
        collection = "LANDSAT-C2-L2",
        bands      = "class",
        tiles      = "MOSAIC",
        multicores = multicores,
        memsize    = memsize,
        data_dir   = output_dir_nonforest,
        labels     = c(
            "1"    = "DeforestationInNonForest",
            "2"    = "Others"
        )
    )
}

.crop_prodes <- function(year, region_id, version, type = "map", base = "prodes") {
    # Define output dir
    output_dir <- .prodes_dir(year = year, version = version)

    # Create directory
    fs::dir_create(output_dir)

    # Download year file
    output_file <- .prodes_download(
        year = year, output_dir = output_dir, type = type, base = base
    )

    # Extract files from zip
    extracted_files <- .prodes_extract_files(year       = year,
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
            NAflag   = 255,
            mask     = TRUE
        )

        # After crop, remove original one
        fs::file_delete(raster_file)

        # Renamed cropped
        fs::file_move(raster_file_out, raster_file)
    }

    # Return!
    dplyr::select(extracted_files, -.data[["processed"]])
}

.prodes_restore_base <- function(region_id,
                                 years,
                                 multicores = 1,
                                 memsize = 120,
                                 version = "v2",
                                 exclude_mask_na = FALSE) {
    # Sort years to ensure we are processing pairs: c(year, year + 1)
    years <- sort(years)

    # Processing each year
    purrr::map(seq_len(length(years)), function(idx) {
        # Get current year
        year <- years[[idx]]
        previous_year <- min(year - 1, 2022)

        if (year > 2024) {
            cli::cli_abort("PRODES available only until 2024")
        }

        # Define output dir
        output_version <- glue::glue("base/restore/{year}")

        output_dir <- .prodes_dir(version = output_version, year = year)
        output_dir_prev <- .prodes_dir(version = output_version, year = previous_year)

        # Create output dir
        fs::dir_create(c(output_dir, output_dir_prev))

        # Download and crop the specified year
        .crop_prodes(
            year = previous_year,
            region_id = region_id,
            base = "restore",
            version = output_version
        )

        # Load prodes of the current year
        restore_map_prev_year <- load_restore_mosaic(
            data_dir = output_dir_prev,
            multicores = multicores,
            memsize = memsize,
            labels = labels_amazon_mcti()
        )

        # Load prodes of the next year
        prodes_next_year_orig <- get(paste0("load_prodes_", year))
        prodes_next_year_orig <- prodes_next_year_orig(
            multicores = multicores,
            memsize = memsize
        )

        # Process forest mask
        deforestation_current_year <- paste0("d", year)
        deforestation_future_years <- paste0("d", (year+1):2030)
        deforestation_past_years <- paste0("d", 2000:(year - 1))

        residual_current_year <- paste0("r", year)
        residual_future_years <- paste0("r", (year+1):2030)
        residual_past_years <- paste0("r", 2000:(year - 1))

        rules_expression <- list(
            "Others" = bquote(cube %in% c(.(residual_past_years), .(deforestation_past_years), "Vegetação Nativa")),
            "Vegetação Nativa" = bquote(
                mask == "Floresta" |
                    cube %in% .(deforestation_future_years) |
                    cube %in% .(residual_future_years)
            ),
            bquote(cube == .(deforestation_current_year)),
            bquote(cube == .(residual_current_year))
        )

        names(rules_expression) <- c(
            "Others",
            "Vegetação Nativa",
            deforestation_current_year,
            residual_current_year
        )

        prodes_forest_mask <- eval(bquote(
            sits::sits_reclassify(
                cube       = prodes_next_year_orig,
                mask       = restore_map_prev_year,
                rules      = .(rules_expression),
                multicores = multicores,
                memsize    = memsize,
                output_dir = output_dir,
                exclude_mask_na = exclude_mask_na
            )
        ))

        old_file <- prodes_next_year_orig[["file_info"]][[1]][["path"]]
        new_file <- prodes_forest_mask[["file_info"]][[1]][["path"]]

        # Handle old file
        old_file_rename <- old_file |>
                                fs::path_ext_remove() |>
                                stringr::str_c("-original") |>
                                fs::path_ext_set("tif")

        fs::file_move(old_file, old_file_rename)

        # Move new file to the old directory
        fs::file_move(new_file, old_file)
    })
}

#' @export
download_prodes <- function(year, output_dir, version = "v2") {
    # Transform output dir object
    output_dir <- fs::path(output_dir)

    # Complete output dir
    output_dir <- output_dir / version / year

    # Create output dir
    fs::dir_create(output_dir)

    # Download year file
    output_file <- .prodes_download(year = year, output_dir = output_dir)

    # Extract files from zip
    extracted_files <- .prodes_extract_files(year       = year,
                                             file       = output_file,
                                             output_dir = output_dir)

    # Return files reference
    # (remove `processed` flag as it is only used in internal routines)
    dplyr::select(extracted_files, -.data[["processed"]])
}

#' @export
prepare_prodes_nf <- function(region_id, year = 2024, multicores = 1, memsize = 120, version = "nf", prodes_loader = NULL) {
    if (year != 2024) {
        cli::cli_abort("Invalid year. Only 2024 is supported")
    }

    # Define current year
    year <- 2024

    # Define output dir
    output_dir <- .prodes_dir(version = version, year = year)

    # Check if output file already exist
    output_file <- output_dir / .prodes_file_sits_name(year = year, ext = "tif")

    if (fs::file_exists(output_file)) {
        return(NULL)
    }

    # Crop raster
    .crop_prodes(year = year, region_id = region_id, version = version, type = "nf")

    # Define loader
    if (is.null(prodes_loader)) {
        prodes_loader <- get(paste0("load_prodes_", year))
    }

    # Load PRODES
    prodes_year <- prodes_loader(
        version = version,
        multicores = multicores,
        memsize = memsize
    )

    # Generate NF mask
    prodes_nf_mask <- sits::sits_reclassify(
        cube       = prodes_year,
        mask       = prodes_year,
        rules      = list(
            "OTHERS" = cube != "NAO FLORESTA"
        ),
        multicores = multicores,
        memsize    = memsize,
        output_dir = output_dir,
        version    = "nf-mask"
    )

    # Region 4 contains the "parallel 44", which requires special handling because
    # "amazonia legal data" does not include deforestation in non-forest areas.
    # Therefore, we must incorporate this data using available non-forest
    # deforestation data.
    if (region_id == 4) {
        # Load Extra NF mask
        prodes_nf_extras <- .prodes_nf_extras_mask(
            year = year,
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir
        )

        prodes_nf_mask <- sits::sits_reclassify(
            cube       = prodes_nf_mask,
            mask       = prodes_nf_extras,
            rules      = list(
                "NAO FLORESTA" = (
                    cube == "NAO FLORESTA" | mask == "DeforestationInNonForest"
                )
            ),
            multicores = multicores,
            memsize    = memsize,
            output_dir = output_dir,
            version    = "nf-mask-extras"
        )
    }

    # Get files
    file_old <- prodes_year[["file_info"]][[1]][["path"]]
    file_new <- prodes_nf_mask[["file_info"]][[1]][["path"]]

    # Move files
    fs::file_move(
        path     = file_new,
        new_path = file_old
    )

    # Remove old .rds (created by loader)
    rds_file <- fs::path(fs::path_dir(file_old)) / "prodes.rds"

    if (fs::file_exists(rds_file)) {
        fs::file_delete(rds_file)
    }
}

#' @export
prepare_prodes <- function(
        region_id,
        years = 2024,
        multicores = 1,
        memsize = 120,
        version = "v2",
        base = "prodes",
        prodes_loader = NULL,
        exclude_mask_na = FALSE,
        nonforest_mask = TRUE,
        nonforest_complete = TRUE,
        fix_pre_aggregation_prodes = TRUE
) {
    # Arrange years for processing using PRODES methodology
    years <- sort(years, decreasing = TRUE)

    # If base is restore, use a special processor
    if (base == "restore") {
        return(
            .prodes_restore_base(
                region_id = region_id,
                years = years,
                multicores = multicores,
                memsize = memsize,
                version = version,
                exclude_mask_na = exclude_mask_na
            )
        )
    }

    # Verify if 2008 is into years list with years before 2007
    if (!2008 %in% years && any(years <= 2007) && !fix_pre_aggregation_prodes) {
        cli::cli_alert_info(
            paste0("Forest masks from PRODES 2000 to 2007 wont be fixed ",
                   "once 2008 was not provided. Please process 2008 or use ",
                   "`fix_pre_aggregation_prodes` if you already have it")
        )
    }

    if (nonforest_complete) {
        # Mask fixed in 2024 as this is the reference in the restore+ project
        prepare_prodes_nf(
            region_id  = region_id,
            year       = 2024,
            multicores = multicores,
            memsize    = memsize,
            version    = "nf"
        )
    }

    # List of cropped years
    years_to_crop <- Map(\(x) x <= 2007 || x == 2024, years)

    # Processing each year
    purrr::map(seq_len(length(years_to_crop)), function(idx) {
        # Get current year
        year <- years[[idx]]

        # Define output dir
        output_dir <- .prodes_dir(version = version, year = year)

        # Create output dir
        fs::dir_create(output_dir)

        # Download and crop the specified year
        if (years_to_crop[[idx]]) {
            .crop_prodes(
                year = year,
                region_id = region_id,
                base = base,
                version = version
            )
        }

        # If year is greater or equal to 2024, skip mask generation
        if (year >= 2024) {
            return(NULL)
        }

        # Processing PRODES mask
        prodes_generate_mask(
            target_year = year,
            version = version,
            multicores = multicores,
            memsize = memsize,
            prodes_loader = prodes_loader,
            exclude_mask_na = exclude_mask_na,
            nonforest_mask = nonforest_mask,
            nonforest_complete = nonforest_complete
        )

        # Apply forest fixing from 2000 to 2007
        if ((fix_pre_aggregation_prodes && year <= 2007) || (year <= 2007 && 2008 %in% years)) {
            cli::cli_alert_info("Fixing pre-aggregation PRODES")

            prodes_2008 <- load_prodes_2008(
                version = version,
                multicores = multicores,
                memsize = memsize
            )

            # Load current PRODES
            prodes_loader <- get(paste0("load_prodes_", year))

            # Load PRODES cube
            prodes_year <- prodes_loader(
                version    = version,
                multicores = multicores,
                memsize    = memsize
            )

            # Build reclassification expression
            rules_expression <- bquote(
                list(
                    "Vegetação Nativa" = cube == "Vegetação Nativa" | mask == "Vegetação Nativa"
                )
            )

            # Reclassify!
            prodes_forest_mask <- eval(bquote(
                sits::sits_reclassify(
                    cube       = prodes_year,
                    mask       = prodes_2008,
                    rules      = .(rules_expression),
                    multicores = multicores,
                    memsize    = memsize,
                    output_dir = output_dir,
                    exclude_mask_na = exclude_mask_na,
                    version    = "fix-prodes-mask"
                )
            ))

            # Get files
            file_old <- prodes_year[["file_info"]][[1]][["path"]]
            file_new <- prodes_forest_mask[["file_info"]][[1]][["path"]]

            # Move files
            fs::file_move(
                path     = file_new,
                new_path = file_old
            )
        }
    })
}
