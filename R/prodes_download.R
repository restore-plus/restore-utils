
.prodes_files <- function() {
    files <- c(
        "https://terrabrasilis.dpi.inpe.br/download/dataset/amz-prodes/raster/prodes_amazonia_2024.zip",
        "https://www.dropbox.com/scl/fi/w9982bmhln4oincvg3hej/prodes_amazonia_legal_2000.zip?rlkey=apjy1uyu3u8uyjyv5dd40la6q&dl=1",
        "https://www.dropbox.com/scl/fi/jz1vzmohcoqxlqmj8f9wi/prodes_amazonia_legal_2001.zip?rlkey=sx8yujl1f46a5q8jweyyx3qvk&dl=1",
        "https://www.dropbox.com/scl/fi/acn7n7aeoycxi3ljf4i2g/prodes_amazonia_legal_2002.zip?rlkey=zo16xu5s4tf90lldd8g2pckxu&dl=1",
        "https://www.dropbox.com/scl/fi/d7u1hfpyfxip9eg4bg6qr/prodes_amazonia_legal_2003.zip?rlkey=k40eqkdb0r73rbfncg9mtdhrz&dl=1",
        "https://www.dropbox.com/scl/fi/wmoft2ch92qi0pvqtiwvv/prodes_amazonia_legal_2004.zip?rlkey=v7gqpmhjc4a4ufirj23ndnv1k&dl=1",
        "https://www.dropbox.com/scl/fi/r2hdzxgrie08vmaag4wzp/prodes_amazonia_legal_2005.zip?rlkey=aw6xf1fcqjvb1riek7af7oqwd&dl=1",
        "https://www.dropbox.com/scl/fi/199b2bknaz12nx6fz0fvh/prodes_amazonia_legal_2006.zip?rlkey=dpeihhl75vfxn44clsf7qddky&dl=1",
        "https://www.dropbox.com/scl/fi/w0pnglt05mkr8wxcyhi1m/prodes_amazonia_legal_2007.zip?rlkey=mmaf7t77y0l6tfr4c21otbtcv&dl=1"
    )

    tibble::tibble(
        file = files,
        year = c(2024, 2000:2007)
    )
}

.prodes_file_metadata <- function(year) {
    file_selected <- .prodes_files() |>
        dplyr::filter(.data[["year"]] == !!year)

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

.prodes_download <- function(year, output_dir) {
    # Get file metadata of the selected file year
    prd_year_file <- .prodes_file_metadata(year = year)

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
            file_sits = output_dir / .terraclass_file_sits_name(year, ext = fs::path_ext(.data[["name"]]))
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
prepare_prodes <- function(region_id, years = 2024,  multicores = 1, timeout = 720, version = "v2") {
    # Setup multisession workers
    future::plan(future::multisession, workers = multicores)

    # Download all specified years
    furrr::future_map(years, function(year) {
        # Set timeout
        withr::local_options(timeout = timeout)

        # Define output dir
        output_dir <- .prodes_dir(year = year, version = version)

        # Create directory
        fs::dir_create(output_dir)

        # Download year file
        output_file <- .prodes_download(year = year, output_dir = output_dir)

        # Extract files from zip
        extracted_files <- .prodes_extract_files(year       = year,
                                                 file       = output_file,
                                                 output_dir = output_dir)

        # Check if files are already processed
        are_files_finished <- all(unname(extracted_files[["processed"]]))

        # If no, crop raster using the eco region selected by the user
        if (!are_files_finished) {
            # Get eco region polygon
            eco_region_roi <- roi_ecoregions(
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
    },
        .options = furrr::furrr_options(seed = TRUE)
    )
}
