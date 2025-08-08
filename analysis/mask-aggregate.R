library(sits)
library(restoremasks)

mask_base_labels <- function() {
    c(
        "1"  = "Agricultura_Anual",
        "2"  = "Agricultura_Perene",
        "3"  = "Agricultura_Semiperene",
        "4"  = "Agua",
        "5"  = "Floresta",
        "7"  = "Pastagem_Arbustiva",
        "8"  = "Pastagem_Herbacea",
        "10" = "Sazonalmente_Inundada_ICS",
        "11" = "Silvicultura",
        "12" = "Vegetacao_Secundaria",
        "13" = "Area_Umida_ICS",
        "14" = "Desmatamento_Do_Ano",
        "15" = "Pasture_Wetland",
        "16" = "Pasto_Silvicultura",
        "17" = "Pasto_Semiperene",
        "18" = "Mineracao",
        "19" = "Area_Urbanizada",
        "20" = "Natural_Nao_Florestal"
    )
}

mask_perene_labels <- function() {
    labels <- mask_base_labels()

    labels["21"] <- "Pasto_Perene"
    labels
}

#
# Constants
#
multicores <- 40
memsize <- 180

classification_years <- 2015:2024
classification_version <- "mask-terraclass-step13step14-perene-reclass"
classification_years_edges <- c(min(classification_years), max(classification_years))

output_dir_base <- "data/derived/masks/mask-mcti-v3"


for (classification_year in classification_years) {
    print(paste0("Processing: ", classification_year))

    # Define version
    version <- "mask-aggregated-from-perene-reclassify"

    # Define base directory
    output_dir <- fs::path(output_dir_base) / classification_year

    # Check if it is edge years
    is_edge_year <- classification_year %in% classification_years_edges

    # Define labels
    labels <- mask_base_labels()

    if (!is_edge_year) {
        labels <- mask_perene_labels()
    }

    # Load current classification map
    classification_map <- load_restore_map(
        data_dir = output_dir,
        multicores = multicores,
        memsize = memsize,
        labels = labels,
        version = classification_version
    )

    # Reclassify
    if (is_edge_year) {
        result <- sits_reclassify(
            cube = classification_map,
            mask = classification_map,
            rules = list(
                "Pastagem" = cube %in% c(
                    "Pastagem_Arbustiva",
                    "Pastagem_Herbacea",
                    "Pasture_Wetland",
                    "Pasto_Silvicultura",
                    "Pasto_Semiperene"
                ),
                "Sazonalmente_Inundada" = cube %in% c("Sazonalmente_Inundada_ICS", "Area_Umida_ICS")
            ),
            multicores = multicores,
            memsize    = memsize,
            output_dir = output_dir,
            version    = version
        )
    } else {
        result <- sits_reclassify(
            cube = classification_map,
            mask = classification_map,
            rules = list(
                "Pastagem" = cube %in% c(
                    "Pastagem_Arbustiva",
                    "Pastagem_Herbacea",
                    "Pasture_Wetland",
                    "Pasto_Silvicultura",
                    "Pasto_Semiperene",
                    "Pasto_Perene"
                ),
                "Sazonalmente_Inundada" = cube %in% c("Sazonalmente_Inundada_ICS", "Area_Umida_ICS")
            ),
            multicores = multicores,
            memsize    = memsize,
            output_dir = output_dir,
            version    = version
        )
    }

    sf::gdal_addo(result[["file_info"]][[1]][["path"]])
}
