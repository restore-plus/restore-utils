library(sits)
library(restoremasks)

#
# Auxiliary functions
#
pixel_mapping_common <- function() {
    tibble::tibble(name = character(), source = numeric(), target = numeric()) |>
        tibble::add_row(name = "Agricultura_Anual",      source = 1,  target = 1) |>
        tibble::add_row(name = "Agricultura_Perene",     source = 2,  target = 2) |>
        tibble::add_row(name = "Agricultura_Semiperene", source = 3,  target = 3) |>
        tibble::add_row(name = "Agua",                   source = 4,  target = 4) |>
        tibble::add_row(name = "Floresta",               source = 5,  target = 5) |>
        tibble::add_row(name = "Silvicultura",           source = 11, target = 6) |>
        tibble::add_row(name = "Vegetacao_Secundaria",   source = 12, target = 7) |>
        tibble::add_row(name = "Desmatamento_Do_Ano",    source = 14, target = 8) |>
        tibble::add_row(name = "Mineracao",              source = 18, target = 9) |>
        tibble::add_row(name = "Area_Urbanizada",        source = 19, target = 10) |>
        tibble::add_row(name = "Natural_Nao_Florestal",  source = 20, target = 11)
}

# edge years = 2015, 2024
pixel_mapping_edge_years <- function() {
    pixel_mapping_common() |>
        tibble::add_row(name = "Pastagem",              source = 21,  target = 12) |>
        tibble::add_row(name = "Sazonalmente_Inundada", source = 22,  target = 13)
}

# middle years = 2016, 2017...
pixel_mapping_middle_years <- function() {
    pixel_mapping_common() |>
        tibble::add_row(name = "Pastagem",              source = 22,  target = 12) |>
        tibble::add_row(name = "Sazonalmente_Inundada", source = 23,  target = 13)
}

#
# General definitions
#
mask_version <- "mask-mcti-v3"
files_version <- "mask-aggregated-from-perene-reclassify"
output_version <- "v1-reclass"

memsize <- 200
multicores <- 48


#
# 1. Get files
#
files <- get_restore_masks_files(
    mask_version = mask_version,
    files_version = files_version,
    multicores = multicores,
    memsize = memsize
)

#
# 2. Extract file years
#
files_year <- get_mask_file_year(files)


#
# 3. Define edge years
#
files_edge_years <- c(min(files_year), max(files_year))


#
# 4. Remap files
#
purrr::map(files, function(file) {
    # Define output file
    file_out <- stringr::str_replace(file, files_version, output_version)
    dir_output <- fs::path_dir(file_out)

    # Get file year
    file_year <- get_mask_file_year(file)

    # Define rules
    file_rules <- pixel_mapping_edge_years()

    if (!(file_year %in% files_edge_years)) {
        file_rules <- pixel_mapping_middle_years()
    }

    # Inform user
    message("Processing: ", file_year, " (", fs::path_file(file) , ")")

    # Remap!
    file_out <- reclassify_remap_pixels(
        file = file,
        file_out = file_out,
        rules = file_rules,
        multicores = multicores,
        memsize = memsize,
        output_dir = dir_output
    )

    sf::gdal_addo(file_out)
})
