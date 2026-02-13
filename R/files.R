#' @title Create a data directory
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Creates a data directory.
#' 
#' @returns Character representing the data directory.
#' 
#' @keywords internal
#' @export
create_data_dir <- function(base_dir, name) {
    output_dir <- fs::path(base_dir) / name

    fs::dir_create(output_dir)

    return(output_dir)
}

#' @title Get the year from a mask file
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Gets the year from a mask file.
#' 
#' @returns Integer representing the year.
#' 
#' @keywords internal
get_mask_file_year <- function(file) {
    as.integer(gsub(".*/(\\d{4})/.*", "\\1", file))
}
