
#' @export
load_restore_map <- function(data_dir, multicores = 32, memsize = 120, ...) {
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
        ),
        ...
    )
}

#' @export
get_restore_masks_files <- function(version, multicores = 32, memsize = 120, ...) {
    files_dir <- create_data_dir("data/derived/masks", version)

    files_pattern <- paste0("^LANDSAT_OLI_MOSAIC_\\d{4}-01-01_\\d{4}-12-01_class_", version, "\\.tif$")

    files <- list.files(
        path = files_dir,
        pattern = files_pattern,
        recursive = TRUE,
        full.names = TRUE
    )

    years <- as.integer(gsub(".*/(\\d{4})/.*", "\\1", files))

    # sort files by year
    ordered_indices <- order(years)
    files <- files[ordered_indices]
    years <- years[ordered_indices]

    expected_years <- seq(min(years), max(years))

    if (!all(years == expected_years)) {
        stop("Sanity check failed: years are missing or out of order.\nFound years: ", paste(years, collapse = ", "))
    }

    return(files)
}

