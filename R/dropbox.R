.dropbox_default_folder <- function() {
    default_dir <- "projects/restore-plus/data"

    fs::path(.project_env_variable("DROPBOX_FOLDER", default_dir))
}

#' @export
dropbox_upload <- function(files, dropbox_dir) {
    purrr::map(files, function(file) {
        print(paste0("Uploading: ", file))
        system(paste("rclone copy", file, paste0("dropbox:", dropbox_dir), sep = " "))
    })
}

#' @export
dropbox_dir <- function(name) {
    .dropbox_default_folder() / name
}
