
.terraclass_files <- function() {
    dplyr::bind_rows(
        .terraclass_files_amazon(),
        .terraclass_files_cerrado()
    )
}

.terraclass_files_amazon <- function() {
    files <- c(
        "https://www.dropbox.com/scl/fi/kav1zlvaais3dr9m0x9ov/AMZ.2004.M.zip?rlkey=om48iucml9cpoin7raxc2tgg7&st=x1o5e50a&dl=1",
        "https://www.dropbox.com/scl/fi/w44ebii8iixe7nzvkils8/AMZ.2008.M.zip?rlkey=pr2rx8wxiflwgb8vk0xjkit9k&st=3ugsl54i&dl=1",
        "https://www.dropbox.com/scl/fi/t3i44v5zsv175untuquh0/AMZ.2010.M.zip?rlkey=8lntmlz7s7pdvuy7xkukw55my&st=8fmy1txh&dl=1",
        "https://www.dropbox.com/scl/fi/eyn1txxcz92q6khpwpcan/AMZ.2012.M.zip?rlkey=ldmfc684g7dgpqwir279xy4u2&st=guwx7yt6&dl=1",
        "https://www.dropbox.com/scl/fi/aiujbpiai56l8zh1vek3d/AMZ.2014.M.zip?rlkey=j6gbs03x297gdiqad0v0fjuz5&st=ba616ggo&dl=1",
        "https://www.dropbox.com/scl/fi/6g4zq9egtgbg2yqrme7nt/AMZ.2016.M.zip?rlkey=gfjbmmc2fb32ij6gx89pn6kk1&dl=1",
        "https://www.dropbox.com/scl/fi/5ky9t0djzbsqstfaxzjlb/AMZ.2018.M.zip?rlkey=jq697xtk1hu4ignvwna8u7qno&st=k49fj43i&dl=1",
        "https://www.dropbox.com/scl/fi/du18c061tazxe8aa9qhms/AMZ.2020.M.zip?rlkey=en7gsyavr8g42s2inpzbarmds&st=3441n02c&dl=1",
        "https://www.dropbox.com/scl/fi/udl9kkakhoj7gct2b34zi/AMZ.2022.M.zip?rlkey=5jpxmzssfqaco4f2q2du24ccq&st=t6e51bg8&dl=1",
        "https://restore-plus.s3.us-east-1.amazonaws.com/tools/restore-utils/assets/tc/2024/AMZ.2024.M.zip"
    )

    tibble::tibble(
        file = files,
        year = as.numeric(stringr::str_extract(files, "(?<=\\.)\\d{4}(?=\\.)")),
        region = "amazon"
    )
}

.terraclass_files_cerrado <- function() {
    files <- c(
        "https://restore-plus.s3.us-east-1.amazonaws.com/tools/restore-utils/assets/tc/2018/CER.2018.M.zip",
        "https://restore-plus.s3.us-east-1.amazonaws.com/tools/restore-utils/assets/tc/2020/CER.2020.M.zip",
        "https://restore-plus.s3.us-east-1.amazonaws.com/tools/restore-utils/assets/tc/2022/CER.2022.M.zip",
        "https://restore-plus.s3.us-east-1.amazonaws.com/tools/restore-utils/assets/tc/2024/CER.2024.M.zip"
    )

    tibble::tibble(
        file = files,
        year = as.numeric(stringr::str_extract(files, "(?<=\\.)\\d{4}(?=\\.)")),
        region = "cerrado"
    )
}


.terraclass_file_metadata <- function(region, year) {
    file_selected <- .terraclass_files() |>
        dplyr::filter(.data[["year"]] == !!year & .data[["region"]] == !!region)

    # Sanity check: We need to have one file per year
    if (nrow(file_selected) != 1) {
        cli::cli_abort("{year} is invalid and is not available as a Terraclass year")
    }

    # Return!
    file_selected
}

.terraclass_file_zip <- function(tc_year_file) {
    fs::path_file(tc_year_file[["file"]])
}

.terraclass_file_sits_name <- function(year, ext, version = "v1") {
    template <- "LANDSAT_TM-ETM-OLI_MOSAIC_XYZ-01-01_XYZ-12-31_class_VERSION.EXT"

    stringr::str_replace_all(template, "XYZ", as.character(year)) |>
        stringr::str_replace("EXT", ext) |>
        stringr::str_replace("VERSION", version)
}

.terraclass_download <- function(region, year, output_dir) {
    # Get file metadata of the selected file year
    tc_year_file <- .terraclass_file_metadata(region = region, year = year)

    # Define output dir
    output_file <- fs::path(output_dir) / .terraclass_file_zip(tc_year_file)

    # If file already exists, return it!
    if (fs::file_exists(output_file)) {
        return(output_file)
    }

    # Download file
    download.file(tc_year_file[["file"]], output_file)

    # Return new file
    output_file
}

.terraclass_extract_files <- function(year, file, output_dir) {
    # Fix output type
    output_dir <- fs::path(output_dir)

    # Get files to be extracted
    files_zip <- utils::unzip(file, list = TRUE) |>
        dplyr::rename("name"   = "Name",
                      "length" = "Length",
                      "date"   = "Date") |>
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
download_terraclass <- function(region, year, output_dir, version = "v1") {
    # Define output dir
    output_dir <- .terraclass_dir(year = year, version = version)

    # Create directory
    fs::dir_create(output_dir)

    # Download year file
    output_file <- .terraclass_download(
        region = region,
        year = year,
        output_dir = output_dir
    )

    # Extract files from zip
    extracted_files <- .terraclass_extract_files(year       = year,
                                                 file       = output_file,
                                                 output_dir = output_dir)

    # Return files reference
    # (remove `processed` flag as it is only used in internal routines)
    dplyr::select(extracted_files, -.data[["processed"]])
}


