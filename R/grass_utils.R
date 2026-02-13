#' @title Find GRASS GIS installation path
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Detects the GRASS GIS installation path. First checks the
#'              \code{GRASS_PATH} environment variable, then tries to detect
#'              it from the system using \code{grass --config path}.
#'
#' @returns Character with the GRASS GIS installation path.
#'
#' @keywords internal
.grass_find_path <- function() {
    # Check environment variable
    grass_path <- Sys.getenv("GRASS_PATH")

    if (!is.null(grass_path) && fs::dir_exists(grass_path)) {
        return(grass_path)
    }

    # Try to detect from system
    grass_config <- tryCatch(
        system("grass --config path", intern = TRUE, ignore.stderr = TRUE),
        error   = function(e) NULL,
        warning = function(w) NULL
    )

    # If there is a valid value
    if (!is.null(grass_config) && length(grass_config) > 0) {

        # Transform path
        grass_config <- trimws(grass_config)

        # Check if path exist
        if (fs::dir_exists(grass_config)) {
            # It is valid!
            return(grass_config)
        }
    }

    # Otherwise, return an error!
    cli::cli_abort(
        "GRASS GIS not found. Set the {.envvar GRASS_PATH} environment variable."
    )
}

#' @title Generate a default naming function for GRASS GIS r.report output files
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Generates a default naming function for GRASS GIS r.report output files.
#'
#' @returns A function that generates a default naming function for GRASS GIS r.report output files.
#'
#' @export
grass_default_naming_fnc <- function(file_name) {

    # Get file name
    file_name <- fs::path_file(file_name)

    # Extract year from file name
    year <- stringr::str_extract(file_name, "\\d{4}")

    # Generate output file name
    glue::glue("area-{year}.txt")
}

#' @title Calculate raster area using GRASS r.report
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Calculates the area (in square meters) of each category in a
#'              raster file by running the GRASS GIS \code{r.report} module
#'              directly from R. Results are parsed with
#'              \code{\link{grass_parse_rreport}}.
#'
#' @param files       Character vector of raster file paths to process
#' @param output_dir  Character specifying a directory to save the
#'                    raw r.report output files. If \code{NULL} (default),
#'                    raw outputs are not saved.
#' @param naming_fnc  Function that generates a default naming function for GRASS
#'                    GRASS GIS r.report output files.
#'
#' @returns A named list of tibbles, one per file, with columns
#'          \code{category} and \code{area_m2}.
#'
#' @export
grass_calc_area <- function(files, output_dir, naming_fnc = grass_default_naming_fnc) {
    # Find GRASS installation
    grass_path <- .grass_find_path()

    # Create output directory if needed
    fs::dir_create(output_dir)

    # Process each file
    purrr::map(files, function(file) {
        # Get file name
        file_name <- fs::path_file(file)

        # Inform user
        cli::cli_alert_info("Calculating area for {file_name}")

        # Load raster to configure GRASS session
        raster <- terra::rast(file)

        # Initialize GRASS session with matching CRS and extent
        rgrass::initGRASS(
            gisBase  = grass_path,
            home     = tempdir(),
            SG       = raster,
            override = TRUE
        )

        # Import raster into GRASS
        rgrass::execGRASS(
            "r.in.gdal",
            input  = file,
            output = "input_map",
            flags  = c("overwrite", "o")
        )

        # Set region to match imported raster
        rgrass::execGRASS("g.region", raster = "input_map")

        # Prepare report file
        report_file <- tempfile(fileext = ".txt")

        # Run r.report (area in square meters)
        rgrass::execGRASS(
            "r.report",
            map         = "input_map",
            units       = "meters",
            nsteps      = 255L,
            null_value  = "*",
            page_length = 0L,
            page_width  = 79L,
            output      = report_file,
            flags       = "overwrite"
        )

        # Define output file name
        out_file <- naming_fnc(file_name)
        out_file <- fs::path(output_dir, out_file)

        # Copy report file to output directory
        fs::file_copy(report_file, out_file, overwrite = TRUE)

        # Clean up temp file
        unlink(report_file)

        # Return!
        out_file
    })
}

#' @title Parse GRASS GIS r.report output file
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Parses a GRASS GIS r.report output file.
#'
#' @param path  Character specifying the file path to the r.report output
#'
#' @returns A tibble with columns \code{category} and \code{area_m2}.
#'
#' @export
grass_parse_rreport <- function(path) {
    # Read file
    lines <- readLines(path, warn = FALSE, encoding = "UTF-8")

    # Match category and area values
    pattern <- "^\\|\\s*([0-9]+|\\*)\\s*\\|.*\\|\\s*([0-9.,]+)\\s*\\|$"

    # Extract lines that match
    matches <- regmatches(lines, regexec(pattern, lines))
    matches <- Filter(length, matches)

    if (length(matches) == 0) {
        stop("No valid category rows found in file.")
    }

    # Extract and clean up values
    categories <- vapply(matches, function(x)
        x[[2]], FUN.VALUE = character(1))
    areas_raw  <- vapply(matches, function(x)
        x[[3]], FUN.VALUE = character(1))

    # Remove commas and convert to numeric
    areas_num <- as.numeric(gsub("[^0-9]", "", areas_raw))

    # Replace '*' with descriptive label
    categories[categories == "*"] <- 255

    # Build data frame
    df <- data.frame(
        category = categories,
        area_m2 = as.numeric(areas_num),
        stringsAsFactors = FALSE
    ) |>
        dplyr::filter(categories != 255)

    df[nrow(df) + 1, ] <- c("total", sum(df[["area_m2"]]))

    return(tibble::as_tibble(df))
}

#' @title Process GRASS r.report files with QML labels
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Processes GRASS GIS r.report files by merging with QML label data
#' and exporting to Excel format. This function assumes a QML file exists in the
#' same directory as each r.report file.
#'
#' @param files  Character vector specifying the r.report file paths to process
#'
#' @returns A list of data frames with category, label, and area information
#'
#' @export
grass_rreport_to_excel <- function(files) {
    purrr::map(files, function(file) {
        # Get file name
        file_name <- fs::path_file(file)

        # Inform user
        cli::cli_alert_info("Processing {file_name}")

        # Find QML file in the same directory
        qml_file <- fs::path(fs::path_dir(file)) |>
            fs::dir_ls(glob = "*.qml")

        # Read QML content
        qml_content <- .qml_to_tibble(qml_file)

        # Parse r.report and merge with labels from QML file
        file_parsed <- grass_parse_rreport(file) |>
            dplyr::mutate(join_id = as.numeric(category)) |>
            dplyr::left_join(qml_content, by = c("join_id" = "pixel")) |>
            dplyr::mutate(label = ifelse(category == "total", "Total Area", .data[["label"]])) |>
            dplyr::select(dplyr::any_of(c("category", "label", "area_m2")))

        # Define output path
        file_output <- fs::path(fs::path_dir(file)) /
            stringr::str_replace(fs::path_file(file), ".txt", ".xlsx")

        # Write to excel file
        openxlsx::write.xlsx(file_parsed, file_output)

        # Return!
        return(file_parsed)
    })
}

#' @title Generate GRASS GIS batch r.report file
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Generates a GRASS GIS batch r.report file.
#'
#' @param files        Character vector specifying the files to process
#' @param output_dir   Character specifying the output directory
#'
#' @returns Path to the generated batch file
#'
#' @export
grass_batch_rreport_generator <- function(files, output_dir) {
    batch_jobs <-
        purrr::map(files, function(file) {
            year_dir <- fs::path_file(fs::path_dir(file))
            dir_type <- fs::path_file(fs::path_dir(fs::path_dir(file)))

            out_path <- fs::path(fs::path_dir(file),
                                 base::sprintf("area-%s-%s.txt", dir_type, year_dir)) |>
                fs::path_abs()

            list(
                PARAMETERS = list(
                    map = list(file),
                    units = 1L,
                    null_value = "*",
                    page_length = 0L,
                    page_width = 79L,
                    nsteps = 255L,
                    sort = 0L,
                    `-h` = FALSE,
                    `-f` = FALSE,
                    `-e` = FALSE,
                    `-n` = FALSE,
                    `-a` = FALSE,
                    `-c` = FALSE,
                    `-i` = FALSE,
                    GRASS_REGION_PARAMETER = NULL,
                    GRASS_REGION_CELLSIZE_PARAMETER = 0.0
                ),
                OUTPUTS = list(output = out_path)
            )
        })

    # Define payload
    payload <- list(format = "batch_3.40", rows = batch_jobs)

    # Save payload
    json_txt <- jsonlite::toJSON(payload,
                                 pretty = TRUE,
                                 auto_unbox = TRUE,
                                 null = "null")

    # Define output path
    output_path <- fs::path(output_dir, "batch-grass-calc-area.batch")

    # Write payload
    base::writeLines(json_txt, output_path)

    # Return!
    return(output_path)
}
