.prodes_dir <- function(version, year) {
    prodes_base_dir <- "data/derived/masks/base/prodes"
    prodes_base_dir <- .project_env_variable("MASK_PRODES_BASE_DIR", prodes_base_dir)

    fs::path(prodes_base_dir) / version / year
}

.prodes_rds <- function(prodes_dir) {
    fs::path(prodes_dir) / "prodes.rds"
}

.prodes_nonforest_output_file <- function(year, output_dir) {
    # Define output file
    file_output <- sprintf(
        "LANDSAT_TM-ETM-OLI_MOSAIC_%s-01-01_%s-12-31_class_v1.tif", 2000, year
    )

    fs::path(output_dir, file_output)
}

.prodes_nonforest_download <- function(output_dir) {
    # Ensure output directory exists
    fs::dir_create(output_dir)

    # List of dataset URLs
    urls <- c(
        yearly_deforestation_nf           = "https://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/vector/yearly_deforestation_nf_biome.zip",
        residual_biome                    = "https://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/vector/residual_nf_biome.zip",
        accumulated_deforestation_2000_nf = "https://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/vector/accumulated_deforestation_2000_nf_biome.zip"
    )

    # Iterate over URLs, download, unzip, and collect .shp
    purrr::imap_dfr(urls, function(url, name) {
        # Subdirectory for this dataset
        dataset_dir <- fs::path(output_dir, name)

        # XXX: Load parallel 44 data to filter geometries on it
        parallel_44_geometry <- system.file("extdata/amazon/prodes-parallel-44.gpkg", package = "restoreutils")
        parallel_44_geometry <- sf::st_read(parallel_44_geometry, quiet = TRUE)

        # If folder exists, skip download
        if (fs::dir_exists(dataset_dir)) {
            res <- fs::dir_ls(dataset_dir, regexp = "\\.shp$", recurse = TRUE) |>
                sf::st_read(quiet = TRUE) |>
                sf::st_make_valid() |>
                dplyr::filter(!sf::st_is_empty(.data[["geometry"]])) |>
                dplyr::mutate(year = as.numeric(stringr::str_replace_all(.data[["class_name"]], "[rd]", ""))) |>
                dplyr::select(.data[["year"]])

            contains <- sf::st_contains(parallel_44_geometry, res, prepared = TRUE)

            # Return existing result
            return(res[unlist(contains),])
        }

        # Temporary file for zip
        tmpfile <- fs::file_temp(ext = ".zip")

        # Download
        download.file(url, tmpfile, mode = "wb")

        # Create dir
        fs::dir_create(dataset_dir)

        # Unzip
        unzip(tmpfile, exdir = dataset_dir)

        # Delete temp file
        fs::file_delete(tmpfile)

        # Find `.shp` files
        shp_files <- fs::dir_ls(dataset_dir, regexp = "\\.shp$", recurse = TRUE)

        if (length(shp_files) == 0) {
            return(NULL)
        }

        res <- suppressWarnings(
            sf::st_read(shp_files, quiet = TRUE) |>
                sf::st_make_valid() |>
                dplyr::filter(!sf::st_is_empty(.data[["geometry"]])) |>
                dplyr::mutate(year = as.numeric(stringr::str_replace_all(.data[["class_name"]], "[rd]", ""))) |>
                dplyr::select(.data[["year"]])
        )

        contains <- sf::st_contains(parallel_44_geometry, res, prepared = TRUE)

        res[unlist(contains),]
    })
}

.prodes_nonforest_rasterize <- function(prodes, year, output_dir, class_id = 1) {
    # Create output directory
    output_dir <- fs::path(output_dir)
    fs::dir_create(output_dir)

    # Working CRS for rasterization (meters)
    crs_rast <- "EPSG:3857"

    # Remove invalid geometries
    prodes_valid <- sf::st_is_valid(prodes)
    prodes <- prodes[prodes_valid,]

    file_output <- .prodes_nonforest_output_file(year, output_dir)

    # Define file output metadata
    meta <- tibble::tibble(
        file       = file_output,
        start_date = 2000,
        end_date   = year,
        class_id   = class_id
    )

    # If file exist, return it
    if (fs::file_exists(file_output)) {
        return(meta)
    }

    # Transform SF to raster CRS
    prodes_3857 <- sf::st_transform(prodes, crs_rast) |>
        dplyr::mutate(class = class_id)

    # Create temp file to save the current sf object
    tmp_gpkg <- fs::file_temp(pattern = paste0("prodes-", start_date, "-", end_date), ext = ".gpkg")

    # Write current sf object
    sf::st_write(prodes_3857, dsn = tmp_gpkg, layer = "year", quiet = TRUE)

    # Define raster extent in `gdal rasterize` format
    sf_bbox <- sf::st_bbox(prodes_3857)

    # Rasterize!
    gdalUtilities::gdal_rasterize(
        src_datasource = tmp_gpkg,
        dst_filename   = file_output,
        a              = "class",
        at             = TRUE,
        tr             = c(10, 10),
        te             = sf_bbox,
        ot             = "UInt16",
        init           = 255,
        a_nodata       = 255,
        co             = c("COMPRESS=DEFLATE", "BIGTIFF=YES"),
        a_srs          = sf::st_crs(crs_rast)$wkt
    )

    # Add overviews
    sf::gdal_addo(file_output)

    # Cleanup
    fs::file_delete(tmp_gpkg)

    # Return!
    meta
}

#' @export
prodes_generate_mask <- function(target_year,
                                 version = "v2",
                                 multicores = 32,
                                 memsize = 120,
                                 prodes_loader = NULL,
                                 exclude_mask_na = FALSE,
                                 nonforest_mask = FALSE,
                                 allow_forest_only = FALSE) {

    cli::cli_inform("> Processing {target_year}")

    if (target_year >= 2024) {
        cli::cli_abort(
            "Nothing to do: 2024 is the most recent year; the forest in this year represents the actual available forest"
        )
    }

    # Define deforestation years (we move from the future to the past).
    # The goal of this function is to "regenerate the forest area".
    # The idea is as follows:
    # If an area is known to be deforested in 2024, by definition, it was forest in 2023.
    # Generalizing: if an area is deforested in year `t`, it was forest in year `t - 1`.
    #
    # Therefore, to generate PRODES with a forest mask, we proceed as follows:
    # Example:
    #   target year = 2016
    #   deforestation years = 2024 – 2017
    # All of these deforestation years correspond to forest in 2016.
    deforestation_years <- paste0("d", (target_year + 1):2024)

    if (target_year == 2000) {
        deforestation_years <- paste0("d", target_year:2024)
    }

    # Define PRODES loader
    if (is.null(prodes_loader)) {
        prodes_loader <- load_prodes_2024

        if (target_year <= 2007) {
            prodes_loader <- get(paste0("load_prodes_", target_year))
        }
    }

    # Load PRODES cube
    prodes_cube <- prodes_loader(
        version    = version,
        multicores = multicores,
        memsize    = memsize
    )

    # Define output dir
    output_dir <- .prodes_dir(version = version, year = target_year)

    # Create output dir
    fs::dir_create(output_dir)

    # Reclassify!
    if (target_year >= 2008 || allow_forest_only && target_year <= 2007) {
        # Build reclassification expression
        rules_expression <- bquote(
            list(
                "Vegetação Nativa" = cube == "Vegetação Nativa" |
                    cube %in% .(deforestation_years)
            )
        )

        prodes_forest_mask <- eval(bquote(
            sits::sits_reclassify(
                cube       = prodes_cube,
                mask       = prodes_cube,
                rules      = .(rules_expression),
                multicores = multicores,
                memsize    = memsize,
                output_dir = output_dir,
                exclude_mask_na = exclude_mask_na,
                version    = "v1"
            )
        ))
    } else {
        # If it is target_year <= 2007, use original PRODES cube
        prodes_forest_mask <- prodes_cube
    }

    # If required, mask non-forest
    if (nonforest_mask) {
        cli::cli_inform("> Processing {target_year} > Applying non-forest mask")

        # Define current deforestation year label
        current_deforestation_year <- paste0("d", target_year)

        # Define output directory
        output_dir_nonforest <- output_dir / "non-forest" / target_year

        # Create output directory
        fs::dir_create(output_dir_nonforest)

        # Check non-forst file
        nonforst_file <- .prodes_nonforest_output_file(
            year       = target_year,
            output_dir = output_dir_nonforest
        )

        if (!fs::file_exists(nonforst_file)) {
            # Download non-forest data
            prodes_nonforest <- .prodes_nonforest_download(output_dir_nonforest)

            # Filter by year
            prodes_nonforest <- prodes_nonforest |>
                dplyr::filter(.data[["year"]] <= !!target_year)

            # Rasterize non-forest
            .prodes_nonforest_rasterize(
                prodes        = prodes_nonforest,
                year          = target_year,
                output_dir    = output_dir_nonforest,
                class_id      = 1 # as this is yearly - it is ok to keep 1
            )
        }

        # Load non-forest as cube
        prodes_nonforest <- sits::sits_cube(
            source     = "MPC",
            collection = "LANDSAT-C2-L2",
            bands      = "class",
            tiles      = "MOSAIC",
            multicores = multicores,
            memsize    = memsize,
            data_dir   = output_dir_nonforest,
            labels     = c(
                "1"   = "CurrentDeforestation",
                "255" = "NoData"
            )
        )

        # Build expression
        rules_expression <- bquote(
            list("DeforestationInNonForest" = mask == "CurrentDeforestation")
        )

        # Apply deforestation in non-forest areas
        prodes_forest_mask_new <- eval(bquote(
            sits::sits_reclassify(
                cube       = prodes_forest_mask,
                mask       = prodes_nonforest,
                rules      = .(rules_expression),
                multicores = multicores,
                memsize    = memsize,
                output_dir = output_dir,
                exclude_mask_na = exclude_mask_na,
                version    = "nonforest-mask"
            )
        ))

        # Get files
        file_new <- prodes_forest_mask_new[["file_info"]][[1]][["path"]]
        file_old <- prodes_forest_mask[["file_info"]][[1]][["path"]]

        # Move files
        fs::file_move(
            path     = file_new,
            new_path = file_old
        )

        # Update cube
        prodes_forest_mask_new[["file_info"]][[1]][["path"]] <- file_old

        # Replace old mask with the new one
        prodes_forest_mask <- prodes_forest_mask_new

        cli::cli_inform("> Processing {target_year} > Finalized non-forest mask")
    }

    # Return result!
    return(prodes_forest_mask)
}

#' @export
load_prodes_2000 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2000)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            # We are using labels with `nuvem` while generating PRODES 2000
            # using 2023 data. This is necessary because PRODES 2024 has a
            # bug and does not provide `d2000` and others until `d2007`.
            labels = c("50"  = "r2010",
                       "51"  = "r2011",
                       "52"  = "r2012",
                       "53"  = "r2013",
                       "54"  = "r2014",
                       "55"  = "r2015",
                       "56"  = "r2016",
                       "57"  = "r2017",
                       "58"  = "r2018",
                       "59"  = "r2019",
                       "60"  = "r2020",
                       "61"  = "r2021",
                       "62"  = "r2022",
                       "63"  = "r2023",
                       "64"  = "r2024",
                       "91"  = "Hidrografia",
                       "99"  = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "Aggregation",
                       "103" = "DeforestationInNonForest"
            )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2001 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2001)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            # We are using labels with `nuvem` while generating PRODES 2000
            # using 2023 data. This is necessary because PRODES 2024 has a
            # bug and does not provide `d2000` and others until `d2007`.
            labels = c("50"  = "r2010",
                       "51"  = "r2011",
                       "52"  = "r2012",
                       "53"  = "r2013",
                       "54"  = "r2014",
                       "55"  = "r2015",
                       "56"  = "r2016",
                       "57"  = "r2017",
                       "58"  = "r2018",
                       "59"  = "r2019",
                       "60"  = "r2020",
                       "61"  = "r2021",
                       "62"  = "r2022",
                       "63"  = "r2023",
                       "64"  = "r2024",
                       "91"  = "Hidrografia",
                       "99"  = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "d2000",
                       "103" = "d2001",
                       "104" = "DeforestationInNonForest"
            )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2002 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2002)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            # We are using labels with `nuvem` while generating PRODES 2000
            # using 2023 data. This is necessary because PRODES 2024 has a
            # bug and does not provide `d2000` and others until `d2007`.
            labels = c("50"  = "r2010",
                       "51"  = "r2011",
                       "52"  = "r2012",
                       "53"  = "r2013",
                       "54"  = "r2014",
                       "55"  = "r2015",
                       "56"  = "r2016",
                       "57"  = "r2017",
                       "58"  = "r2018",
                       "59"  = "r2019",
                       "60"  = "r2020",
                       "61"  = "r2021",
                       "62"  = "r2022",
                       "63"  = "r2023",
                       "64"  = "r2024",
                       "91"  = "Hidrografia",
                       "99"  = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "d2001",
                       "103" = "d2002",
                       "104" = "DeforestationInNonForest"
            )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2003 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2003)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            # We are using labels with `nuvem` while generating PRODES 2000
            # using 2023 data. This is necessary because PRODES 2024 has a
            # bug and does not provide `d2000` and others until `d2007`.
            labels = c("50"  = "r2010",
                       "51"  = "r2011",
                       "52"  = "r2012",
                       "53"  = "r2013",
                       "54"  = "r2014",
                       "55"  = "r2015",
                       "56"  = "r2016",
                       "57"  = "r2017",
                       "58"  = "r2018",
                       "59"  = "r2019",
                       "60"  = "r2020",
                       "61"  = "r2021",
                       "62"  = "r2022",
                       "63"  = "r2023",
                       "64"  = "r2024",
                       "91"  = "Hidrografia",
                       "99"  = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "d2002",
                       "103" = "d2003",
                       "104" = "DeforestationInNonForest"
            )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2004 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2004)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            # We are using labels with `nuvem` while generating PRODES 2000
            # using 2023 data. This is necessary because PRODES 2024 has a
            # bug and does not provide `d2000` and others until `d2007`.
            labels = c("50"  = "r2010",
                       "51"  = "r2011",
                       "52"  = "r2012",
                       "53"  = "r2013",
                       "54"  = "r2014",
                       "55"  = "r2015",
                       "56"  = "r2016",
                       "57"  = "r2017",
                       "58"  = "r2018",
                       "59"  = "r2019",
                       "60"  = "r2020",
                       "61"  = "r2021",
                       "62"  = "r2022",
                       "63"  = "r2023",
                       "64"  = "r2024",
                       "91"  = "Hidrografia",
                       "99"  = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "d2003",
                       "103" = "d2004",
                       "104" = "DeforestationInNonForest"
            )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2005 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2005)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            # We are using labels with `nuvem` while generating PRODES 2005
            # using 2023 data. This is necessary because PRODES 2024 has a
            # bug and does not provide `d2000` and others until `d2007`.
            labels = c("50"  = "r2010",
                       "51"  = "r2011",
                       "52"  = "r2012",
                       "53"  = "r2013",
                       "54"  = "r2014",
                       "55"  = "r2015",
                       "56"  = "r2016",
                       "57"  = "r2017",
                       "58"  = "r2018",
                       "59"  = "r2019",
                       "60"  = "r2020",
                       "61"  = "r2021",
                       "62"  = "r2022",
                       "63"  = "r2023",
                       "64"  = "r2024",
                       "91"  = "Hidrografia",
                       "99"  = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "d2004",
                       "103" = "d2005",
                       "104" = "DeforestationInNonForest"
            )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2006 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2006)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            # We are using labels with `nuvem` while generating PRODES 2005
            # using 2023 data. This is necessary because PRODES 2024 has a
            # bug and does not provide `d2000` and others until `d2007`.
            labels = c("50"  = "r2010",
                       "51"  = "r2011",
                       "52"  = "r2012",
                       "53"  = "r2013",
                       "54"  = "r2014",
                       "55"  = "r2015",
                       "56"  = "r2016",
                       "57"  = "r2017",
                       "58"  = "r2018",
                       "59"  = "r2019",
                       "60"  = "r2020",
                       "61"  = "r2021",
                       "62"  = "r2022",
                       "63"  = "r2023",
                       "64"  = "r2024",
                       "91"  = "Hidrografia",
                       "99"  = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "d2005",
                       "103" = "d2006",
                       "104" = "DeforestationInNonForest"
            )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2007 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2007)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            # We are using labels with `nuvem` while generating PRODES 2005
            # using 2023 data. This is necessary because PRODES 2024 has a
            # bug and does not provide `d2000` and others until `d2007`.
            labels = c("50"  = "r2010",
                       "51"  = "r2011",
                       "52"  = "r2012",
                       "53"  = "r2013",
                       "54"  = "r2014",
                       "55"  = "r2015",
                       "56"  = "r2016",
                       "57"  = "r2017",
                       "58"  = "r2018",
                       "59"  = "r2019",
                       "60"  = "r2020",
                       "61"  = "r2021",
                       "62"  = "r2022",
                       "63"  = "r2023",
                       "64"  = "r2024",
                       "91"  = "Hidrografia",
                       "99"  = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "d2006",
                       "103" = "d2007",
                       "104" = "DeforestationInNonForest"
            )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2008 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2008)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            # We are using labels with `nuvem` while generating PRODES 2010
            # using 2023 data. This is necessary because PRODES 2024 has a
            # bug and does not provide `d2000` and others until `d2007`.
            labels = c("0"   = "d2000",
                       "2"   = "d2002",
                       "4"   = "d2004",
                       "6"   = "d2006",
                       "7"   = "d2007",
                       "8"   = "d2008",
                       "50"  = "r2010",
                       "51"  = "r2011",
                       "52"  = "r2012",
                       "53"  = "r2013",
                       "54"  = "r2014",
                       "55"  = "r2015",
                       "56"  = "r2016",
                       "57"  = "r2017",
                       "58"  = "r2018",
                       "59"  = "r2019",
                       "60"  = "r2020",
                       "61"  = "r2021",
                       "62"  = "r2022",
                       "63"  = "r2023",
                       "64"  = "r2024",
                       "91"  = "Hidrografia",
                       "99"  = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2009 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2009)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            # We are using labels with `nuvem` while generating PRODES 2010
            # using 2023 data. This is necessary because PRODES 2024 has a
            # bug and does not provide `d2000` and others until `d2007`.
            labels = c("0"   = "d2000",
                       "2"   = "d2002",
                       "4"   = "d2004",
                       "6"   = "d2006",
                       "7"   = "d2007",
                       "8"   = "d2008",
                       "9"   = "d2009",
                       "50"  = "r2010",
                       "51"  = "r2011",
                       "52"  = "r2012",
                       "53"  = "r2013",
                       "54"  = "r2014",
                       "55"  = "r2015",
                       "56"  = "r2016",
                       "57"  = "r2017",
                       "58"  = "r2018",
                       "59"  = "r2019",
                       "60"  = "r2020",
                       "61"  = "r2021",
                       "62"  = "r2022",
                       "63"  = "r2023",
                       "64"  = "r2024",
                       "91"  = "Hidrografia",
                       "99"  = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2010 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2010)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            # We are using labels with `nuvem` while generating PRODES 2010
            # using 2023 data. This is necessary because PRODES 2024 has a
            # bug and does not provide `d2000` and others until `d2007`.
            labels = c("0"   = "d2000",
                       "2"   = "d2002",
                       "4"   = "d2004",
                       "6"   = "d2006",
                       "7"   = "d2007",
                       "8"   = "d2008",
                       "9"   = "d2009",
                       "10"  = "d2010",
                       "50"  = "r2010",
                       "51"  = "r2011",
                       "52"  = "r2012",
                       "53"  = "r2013",
                       "54"  = "r2014",
                       "55"  = "r2015",
                       "56"  = "r2016",
                       "57"  = "r2017",
                       "58"  = "r2018",
                       "59"  = "r2019",
                       "60"  = "r2020",
                       "61"  = "r2021",
                       "62"  = "r2022",
                       "63"  = "r2023",
                       "64"  = "r2024",
                       "91"  = "Hidrografia",
                       "99"  = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2011 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2011)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2012 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2012)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2013 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2013)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2014 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2014)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "14" = "d2014",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2015 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2015)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "14" = "d2014",
                       "15" = "d2015",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2016 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2016)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "14" = "d2014",
                       "15" = "d2015",
                       "16" = "d2016",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2017 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2017)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "14" = "d2014",
                       "15" = "d2015",
                       "16" = "d2016",
                       "17" = "d2017",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2018 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2018)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "14" = "d2014",
                       "15" = "d2015",
                       "16" = "d2016",
                       "17" = "d2017",
                       "18" = "d2018",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2019 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2019)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "14" = "d2014",
                       "15" = "d2015",
                       "16" = "d2016",
                       "17" = "d2017",
                       "18" = "d2018",
                       "19" = "d2019",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2020 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2020)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "14" = "d2014",
                       "15" = "d2015",
                       "16" = "d2016",
                       "17" = "d2017",
                       "18" = "d2018",
                       "19" = "d2019",
                       "20" = "d2020",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }

    prodes
}

#' @export
load_prodes_2021 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2021)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "14" = "d2014",
                       "15" = "d2015",
                       "16" = "d2016",
                       "17" = "d2017",
                       "18" = "d2018",
                       "19" = "d2019",
                       "20" = "d2020",
                       "21" = "d2021",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )
        saveRDS(prodes, prodes_rds)
    }

    prodes
}

#' @export
load_prodes_2022 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2022)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "14" = "d2014",
                       "15" = "d2015",
                       "16" = "d2016",
                       "17" = "d2017",
                       "18" = "d2018",
                       "19" = "d2019",
                       "20" = "d2020",
                       "21" = "d2021",
                       "22" = "d2022",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2023 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2023)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {
        prodes <- readRDS(prodes_rds)
    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "14" = "d2014",
                       "15" = "d2015",
                       "16" = "d2016",
                       "17" = "d2017",
                       "18" = "d2018",
                       "19" = "d2019",
                       "20" = "d2020",
                       "21" = "d2021",
                       "22" = "d2022",
                       "23" = "d2023",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2024 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2024)
    prodes_rds <- .prodes_rds(prodes_dir)

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("product", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("0" = "d2000",
                       "2" = "d2002",
                       "4" = "d2004",
                       "6" = "d2006",
                       "7" = "d2007",
                       "8" = "d2008",
                       "9" = "d2009",
                       "10" = "d2010",
                       "11" = "d2011",
                       "12" = "d2012",
                       "13" = "d2013",
                       "14" = "d2014",
                       "15" = "d2015",
                       "16" = "d2016",
                       "17" = "d2017",
                       "18" = "d2018",
                       "19" = "d2019",
                       "20" = "d2020",
                       "21" = "d2021",
                       "22" = "d2022",
                       "23" = "d2023",
                       "24" = "d2024",
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "59" = "r2019",
                       "60" = "r2020",
                       "61" = "r2021",
                       "62" = "r2022",
                       "63" = "r2023",
                       "64" = "r2024",
                       "91" = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "NAO FLORESTA",
                       "102" = "DeforestationInNonForest"
                )
        )

        saveRDS(prodes, prodes_rds)
    }

    prodes
}
