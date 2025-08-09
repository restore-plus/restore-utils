.project_env_variable_name <- function(name) {
    paste0("RESTOREPLUS_", stringr::str_to_upper(name))
}

.project_env_variable <- function(name, default) {
    Sys.getenv(.project_env_variable_name(name), default)
}

#' Get the Directory path for Cubes
#'
#' This function retrieves the directory path where cube is stored.
#'
#' It first checks for an environment variable (`RESTORE_PLUS_REG3_DIR`)
#' and falls back to a default directory (`data/derived/cubes`) if the
#' environment variable is not set. The function uses `fs::path()` to
#' ensure proper path construction across different operating systems.
#'
#' @return A character string representing the directory path, constructed
#' using `fs::path()`.
#' @export
project_cubes_dir <- function() {
    default_dir <- "data/derived/cubes"

    fs::path(.project_env_variable("CUBE_DIR", default_dir))
}

#' @export
project_mosaics_dir <- function() {
    default_dir <- "data/derived/mosaics"

    fs::path(.project_env_variable("MOSAIC_DIR", default_dir))
}

#' @export
project_classifications_dir <- function() {
    default_dir <- "data/derived/classifications"

    fs::path(.project_env_variable("CLASSIFICATION_DIR", default_dir))
}

#' @export
project_masks_dir <- function() {
    default_dir <- "data/derived/masks"

    fs::path(.project_env_variable("MASK_DIR", default_dir))
}

#' @export
project_model_file <- function(version) {
    default_dir <- "data/derived/models"

    fs::path(.project_env_variable("MODEL_FILE", default_dir)) / paste0(version, ".rds")
}
