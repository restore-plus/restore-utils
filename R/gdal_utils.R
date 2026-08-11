#' @title Add overviews into tiff files
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param cube a sits classification cube.
#'
#' @returns character with classified files.
#'
#' @export
gdal_addo <- function(cube) {
    # Combine file info
    files <- dplyr::bind_rows(cube[["file_info"]])

    # Add overviews into tiff files
    purrr::map_chr(files[["path"]], sf::gdal_addo)

    # Return !
    return(invisible(files))
}
