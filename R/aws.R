#' @title Upload files to AWS S3 bucket
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Uploads one or more local files to an AWS S3 bucket.
#'
#' @param files    Character vector of local file paths to upload
#' @param object   Character vector specifying S3 object path prefix(es). Can be a single
#'                 value (used for all files) or same length as files (one per file)
#' @param bucket   Character specifying the S3 bucket name
#' @param ...      Additional parameters passed to \code{aws.s3::put_object()}
#'
#' @returns A list of results from \code{aws.s3::put_object()} for each uploaded file
#'
#' @export
aws_upload <- function(files, object, bucket, ...) {
    # Verify files exists
    stopifnot(any(fs::file_exists(files)))
    stopifnot(length(object) == 1 || length(object) == length(files))

    # Transform into fs
    files <- fs::path(files)
    object <- fs::path(object)

    # Verify if bucket exists
    stopifnot(aws.s3::bucket_exists(bucket))

    # Upload file
    purrr::map2(files, object, function(file_to_upload, obj) {
        cli::cli_alert_info("Uploading {fs::path_file(file_to_upload)}")
        aws.s3::put_object(
            file = file_to_upload,
            object = obj / fs::path_file(file_to_upload),
            bucket = bucket,
            show_progress = TRUE,
            ...
        )
    })
}

#' @title Download file from AWS S3 and save as \code{sits} standard name
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Downloads a single file from an AWS S3 bucket (or any HTTP/HTTPS
#'   URL) to a local directory. The file is saved with a \code{sits}-standard filename:
#'   \itemize{
#'     \item \code{LANDSAT_OLI_MOSAIC_YYYY-01-01_YYYY-12-31_class_VVVV.tif}.
#'   }
#'   If the target file already exists locally, the function returns its path
#'   without re-downloading.
#'
#' @param file       Character with the full URL of the file to download (e.g. S3 HTTP/HTTPS URI).
#' @param version    Character with the version label used in the output path (e.g. \code{"v1"}).
#' @param year       Integer or character with the year used in the output path and filename.
#' @param output_dir Character with the base directory where the file will be saved; the
#'                   actual path will be \code{<output_dir>/<version>/<year>/}.
#'
#' @returns Character path to the downloaded (or existing) local file, as an
#'   \code{fs_path} object.
#'
#' @export
aws_download <- function(file, version, year, output_dir) {
    # Define output directory
    output_dir <- fs::dir_create(output_dir)

    # Define output directory
    output_file <- glue::glue("LANDSAT_OLI_MOSAIC_{year}-01-01_{year}-12-31_class_{version}.tif")
    output_file <- output_dir / output_file

    # If already exist, return it
    if (fs::file_exists(output_file)) {
        return(output_file)
    }

    # Create output dir
    fs::dir_create(output_dir)

    # Download file
    utils::download.file(file, output_file)

    # Return!
    return(output_file)
}

#' @title Build AWS file name for Cerrado
#'
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#'
#' @description Builds file name for Cerrado in Restore-plus S3
#'
#' @param version    Character with the version label used in the output path (e.g. \code{"v1"}).
#' @param year       Integer or character with the year used in the output path and filename.
#'
#' @returns Character with file name.
#'
#' @export
aws_build_filename_cerrado <- function(version, year) {
    # Convert to numeric
    year <- as.numeric(year)
    # Create start and end dates
    start_date <- glue::glue("{year}-01-01")
    end_date <- glue::glue("{year + 1}-12-31")

    # Build file name
    filename <- glue::glue(
        "lulcbrasil_30m_cer_{start_date}_{end_date}_class_{version}.tif"
    )

    # Return!
    return(filename)
}
