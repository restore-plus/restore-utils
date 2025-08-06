library(sits)
library(restoremasks)

#
# Auxiliary functions
#
create_output_dir <- function(base_dir, name) {
    output_dir <- fs::path(base_dir) / name

    fs::dir_create(output_dir)

    return(output_dir)
}

#
# General definitions
#
memsize <- 120
multicores <- 36

version <- "v2"
base_output_dir <- "data/raw/masks/prodes-mask-forest/"

# Recover the PRODES classified cube
prodes_2024 <- load_prodes_2024(multicores = multicores, memsize = memsize)

#
# Create base output dir
#
base_output_dir <- create_output_dir(base_output_dir, version)

#
# Note: We start generating masks in 2023, as 2024 is the most recent data, and
#       all forest there is the current forest. So, there is no requirements for
#       extra data transformations
#

#
# 2023
#
output_dir <- create_output_dir(base_output_dir, "2023")

sits_reclassify(
    cube = prodes_2024,
    mask = prodes_2024,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c("d2024")
    ),
    multicores = multicores,
    memsize    = memsize,
    output_dir = output_dir
)

#
# 2022
#
output_dir <- create_output_dir(base_output_dir, "2022")

sits_reclassify(
    cube = prodes_2024,
    mask = prodes_2024,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c("d2024", "d2023")
    ),
    multicores = multicores,
    memsize    = memsize,
    output_dir = output_dir
)

#
# 2021
#
output_dir <- create_output_dir(base_output_dir, "2021")

sits_reclassify(
    cube = prodes_2024,
    mask = prodes_2024,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c("d2024", "d2023", "d2022")
    ),
    multicores = multicores,
    memsize    = memsize,
    output_dir = output_dir
)

#
# 2020
#
output_dir <- create_output_dir(base_output_dir, "2020")

sits_reclassify(
    cube = prodes_2024,
    mask = prodes_2024,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2024", "d2023", "d2022", "d2021"
        )
    ),
    multicores = multicores,
    memsize    = memsize,
    output_dir = output_dir
)

#
# 2019
#
output_dir <- create_output_dir(base_output_dir, "2019")

sits_reclassify(
    cube = prodes_2024,
    mask = prodes_2024,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2024", "d2023", "d2022", "d2021", "d2020"
        )
    ),
    multicores = multicores,
    memsize    = memsize,
    output_dir = output_dir
)

#
# 2018
#
output_dir <- create_output_dir(base_output_dir, "2018")

sits_reclassify(
    cube = prodes_2024,
    mask = prodes_2024,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2024", "d2023", "d2022", "d2021", "d2020", "d2019"
        )
    ),
    multicores = multicores,
    memsize    = memsize,
    output_dir = output_dir
)

#
# 2017
#
output_dir <- create_output_dir(base_output_dir, "2017")

sits_reclassify(
    cube = prodes_2024,
    mask = prodes_2024,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2024", "d2023", "d2022", "d2021", "d2020", "d2019",
            "d2018"
        )
    ),
    multicores = multicores,
    memsize    = memsize,
    output_dir = output_dir
)

#
# 2016
#
output_dir <- create_output_dir(base_output_dir, "2016")

sits_reclassify(
    cube = prodes_2024,
    mask = prodes_2024,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2024", "d2023", "d2022", "d2021", "d2020", "d2019",
            "d2018", "d2017"
        )
    ),
    multicores = multicores,
    memsize    = memsize,
    output_dir = output_dir
)

#
# 2015
#
output_dir <- create_output_dir(base_output_dir, "2015")

sits_reclassify(
    cube = prodes_2024,
    mask = prodes_2024,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2024", "d2023", "d2022", "d2021", "d2020", "d2019",
            "d2018", "d2017", "d2016"
        )
    ),
    multicores = multicores,
    memsize    = memsize,
    output_dir = output_dir
)
