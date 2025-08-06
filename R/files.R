
#' @export
create_data_dir <- function(base_dir, name) {
    output_dir <- fs::path(base_dir) / name

    fs::dir_create(output_dir)

    return(output_dir)
}
