#' @title Send a notification (using ntfy)
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Sends a notification using ntfy.
#' 
#' @param context Character representing the context.
#' @param message Character representing the message.
#' 
#' @returns Nothing.
#' 
#' @keywords internal
#' 
#' @export
notify <- function(context, message) {
    ntfy::ntfy_send(
        paste0("(", context, ") > ", message)
    )
}
