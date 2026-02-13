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
