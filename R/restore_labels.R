#' @title Labels Amazon MCTI
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Returns the labels for the Amazon maps produced for the MCTI.
#'
#' @returns A vector of labels.
#'
#' @export
labels_amazon_mcti <- function() {
    c(
        "1"  = "Agricultura anual",
        "2"  = "Agricultura semi-perene",
        "3"  = "Água",
        "4"  = "Floresta",
        "5"  = "Silvicultura",
        "6"  = "Vegetação secundária",
        "7"  = "Mineração",
        "8"  = "Área urbanizada",
        "9"  = "Natural não florestal",
        "10" = "Pastagem",
        "11" = "Sazonalmente inundada",
        "12" = "Desmatamento do ano",
        "13" = "Agricultura Perene",
        "14" = "Não observado"
    )
}

#' @title Labels Cerrado of a single classification
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Returns the labels for the Cerrado classification.
#'
#' @returns A vector of labels.
#'
#' @export
labels_cerrado_classification <- function() {
    c(
        "0"  = "NoData",
        "1"  = "Agricultura anual",
        "2"  = "Cerradao",
        "3"  = "Cerrado",
        "4"  = "Mangue",
        "5"  = "Natural Não-Vegetal",
        "6"  = "Open-Cerrado",
        "7"  = "Pasto",
        "8"  = "Agricultura Perene",
        "9"  = "Silvicultura",
        "10" = "Cana",
        "11" = "Água"
    )
}
