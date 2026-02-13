#' @title Colors Amazon MCTI
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Returns the colors for the Amazon maps produced for the MCTI.
#' 
#' @returns A tibble with the colors for the Amazon maps produced for the MCTI.
#' 
#' @export
colors_amazon_mcti <- function() {
    tibble::tibble(
        name = c(
            "Agricultura anual", 
            "Agricultura semi-perene", 
            "Água", 
            "Floresta", 
            "Silvicultura", 
            "Vegetação secundária", 
            "Mineração", 
            "Área urbanizada", 
            "Natural não florestal", 
            "Pastagem", 
            "Sazonalmente inundada", 
            "Desmatamento do ano", 
            "Agricultura perene", 
            "Não observado"
        ),
        index = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
        color = c(
            "#ffff00", 
            "#996400", 
            "#0000ff", 
            "#005500", 
            "#a8a800", 
            "#0fc80f", 
            "#ad89cd", 
            "#ffa8c0", 
            "#b4d79e", 
            "#ffec87", 
            "#87ceeb", 
            "#ff0000", 
            "#ff8828", 
            "#ffffff"
        )
    )
}
