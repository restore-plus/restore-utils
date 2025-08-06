library(sits)
library(restoremasks)

#
# Constants
#
multicores <- 40
memsize <- 180
tiles <- c("005007", "005008", "006007", "006008", "009010")

result_mask_id <- "2018_test"

output_dir_base <- "data/derived/masks/mask-mcti-test/"


#
# Define output. directory
#
output_dir <- fs::path(output_dir_base) / result_mask_id


#
# PRODES data
#
prodes <- load_prodes_2018(multicores = multicores, memsize = memsize)


#
# Terraclass_2022
#
terraclass_2022 <- load_terraclass_2022(multicores = multicores, memsize = memsize)



#
# Terraclass
#
terraclass <- load_terraclass_2018(multicores = multicores, memsize = memsize)


#
# Classification
#
eco3_class <- load_restore_map(data_dir = output_dir,
                               multicores = multicores,
                               memsize = memsize)


#
# Step 1
#
eco3_class <- sits_clean(
    cube = eco3_class,
    window_size = 5,
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-clean-step1",
    progress = TRUE
)


#
# Step 2
#
eco3_mask <- sits_reclassify(
    cube = eco3_class,
    mask = prodes,
    rules = list(
        "vegetacao_secundaria" = cube == "vegetacao_secundaria" |
            cube %in% c("Forest", "Riparian_Forest", "Mountainside_Forest") &
            mask != "Vegetação Nativa"
    ),
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-prodes-step2"
)


#
# Step 3
#
eco3_mask <- sits_reclassify(
    cube = eco3_mask,
    mask = prodes,
    rules = list(
        "Forest" = cube == "Forest" | mask == "Vegetação Nativa",
        "deforest_year" = mask == "d2018"
    ),
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-prodes-step3"
)


#
# Step 4
#
eco3_mask <- sits_reclassify(
    cube = eco3_mask,
    mask = prodes,
    rules = list(
        "Pasture_Wetland" = (
            cube %in% c("Seasonally_Flooded_ICS", "Wetland_ICS") &
                mask %in% c("d2000", "d2001", "d2002","d2003","d2004", "d2005", "d2006",
                            "d2007", "d2008", "d2009", "d2010", "d2011", "d2012","d2013",
                            "d2014", "d2015", "d2016", "d2017",
                            "r2010", "r2011", "r2012", "r2013", "r2014", "r2015",
                            "r2016", "r2017", "r2018")
        )
    ),
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-prodes-step4"
)


#
# Step 5
#
eco3_mask <- sits_reclassify(
    cube = eco3_mask,
    mask = terraclass,
    rules = list(
        # Onde o TerraClass for Silvicultura, vira Silvicultura
        "Silvicultura" = cube == "Silvicultura" | mask == "SILVICULTURA"
    ),
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-terraclass-step5"
)


#
# Step 6
#
eco3_mask <- sits_reclassify(
    cube = eco3_mask,
    mask = terraclass,
    rules = list(
        # Onde nosso cubo for Silvicultura mas o TerraClass não for, vira pasto_silvicultura
        "pasto_silvicultura" = cube == "Silvicultura" & mask != "SILVICULTURA"
    ),
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-terraclass-step6"
)


#
# Step 7
#
eco3_mask <- sits_reclassify(
    cube = eco3_mask,
    mask = terraclass,
    rules = list(
        # Onde o TerraClass for Semiperene, vira Agr. Semiperene
        "Agr. Semiperene" = cube == "Agr. Semiperene" | mask == "CULTURA AGRICOLA SEMIPERENE"
    ),
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-terraclass-step7"
)


#
# Step 8
#
eco3_mask <- sits_reclassify(
    cube = eco3_mask,
    mask = terraclass,
    rules = list(
        # Onde o cubo for Agr. Semiperene mas o TerraClass não for, vira pasto_semiperene
        "pasto_semiperene" = cube == "Agr. Semiperene" & mask != "CULTURA AGRICOLA SEMIPERENE"
    ),
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-terraclass-step8"
)


#
# Step 9
#
eco3_mask <- sits_reclassify(
    cube = eco3_mask,
    mask = terraclass,
    rules = list(
        "mineracao" = mask == "MINERACAO"
    ),
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-terraclass-step9"
)


#
# Step 10
#
eco3_mask <- sits_reclassify(
    cube = eco3_mask,
    mask = terraclass,
    rules = list(
        "area_urbanizada" = mask == "URBANIZADA"
    ),
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-terraclass-step10"
)


#
# Step 11
#
eco3_mask <- sits_reclassify(
    cube = eco3_mask,
    mask = terraclass,
    rules = list(
        "agua" = (
            mask == "CORPO DAGUA" &
                !cube %in% c("Wetland_ICS", "Seasonally_Flooded_ICS")
        )
    ),
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-terraclass-step11"
)


#
# Step 12
#
eco3_mask <- sits_reclassify(
    cube = eco3_mask,
    mask = terraclass_2022,
    rules = list(
        "nat_non_forest" = mask == "NATURAL NAO FLORESTAL"
    ),
    multicores = multicores,
    memsize = memsize,
    output_dir = output_dir,
    version = "mask-terraclass-step12"
)


#
# Generate COG data
#
sf::gdal_addo(eco3_mask[["file_info"]][[1]][["path"]])

saveRDS(eco3_mask, output_dir / "mask-cube.rds")


#
# Crop cube to tiles
#
if (length(tiles)) {
    cube_files <- crop_to_roi(
        cube = eco3_mask,
        tiles = tiles,
        multicores = multicores,
        output_dir = output_dir,
        grid_system = "BDC_MD_V2"
    )

    saveRDS(cube_files, output_dir / "mask-cube-tile-files.rds")
}
