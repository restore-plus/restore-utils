
#' @export
notify <- function(context, message) {
    ntfy::ntfy_send(
        paste0("(", context, ") > ", message)
    )
}
