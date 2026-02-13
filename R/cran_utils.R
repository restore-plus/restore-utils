#' @title Update the package from GitHub
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Updates the package from GitHub.
#' 
#' @returns Nothing.
#' 
#' @keywords internal
#' @export
self_update <- function() {
    devtools::install_github("restore-plus/restore-utils")
}
