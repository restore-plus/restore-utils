
#' @export
load_restore_map <- function(data_dir, multicores = 32, memsize = 120) {
    sits_cube(
        source = "BDC",
        collection = "LANDSAT-OLI-16D",
        data_dir = data_dir,
        memsize = memsize,
        multicores = multicores,
        parse_info = c("satellite", "sensor",
                       "tile", "start_date", "end_date",
                       "band", "version"),
        bands = "class",
        labels = c("1" = "2ciclos",
                   "2" = "Ag_perene",
                   "3" = "Agr. Semiperene",
                   "4" = "agua",
                   "5" = "Forest",
                   "6" = "Mountainside_Forest",
                   "7" = "past_arbustiva",
                   "8" = "past_herbacea",
                   "9" = "Riparian_Forest",
                   "10" = "Seasonally_Flooded_ICS",
                   "11" = "Silvicultura",
                   "12" = "vegetacao_secundaria",
                   "13" = "Wetland_ICS"
        )
    )
}
