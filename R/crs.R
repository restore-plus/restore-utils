
#' @export
crs_bdc <- function() {
    readRDS(system.file("extdata/crs/bdc.rds", package = "restoreutils"))
}
