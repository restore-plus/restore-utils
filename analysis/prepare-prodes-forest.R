library(sits)

multicores <- 10
memsize <- 40

# Set the directory for PRODES data
data_dir <- "data/raw/masks/prodes-mask-forest/2023"

# Recover the PRODES classified cube
prodes_2023 <- sits_cube(
    source = "USGS",
    collection = "LANDSAT-C2L2-SR",
    data_dir = data_dir,
    multicores = multicores,
    memsize = memsize,
    parse_info = c("product", "sensor",
                   "tile", "start_date", "end_date",
                   "band", "version"),
    bands = "class",
    labels = c("0" = "d2000",
               "2" = "d2002",
               "4" = "d2004",
               "6" = "d2006",
               "7" = "d2007",
               "8" = "d2008",
               "9" = "d2009",
               "10" = "d2010",
               "11" = "d2011",
               "12" = "d2012",
               "13" = "d2013",
               "14" = "d2014",
               "15" = "d2015",
               "16" = "d2016",
               "17" = "d2017",
               "18" = "d2018",
               "19" = "d2019",
               "20" = "d2020",
               "21" = "d2021",
               "22" = "d2022",
               "23" = "d2023",
               "50" = "r2010",
               "51" = "r2011",
               "52" = "r2012",
               "53" = "r2013",
               "54" = "r2014",
               "55" = "r2015",
               "56" = "r2016",
               "57" = "r2017",
               "58" = "r2018",
               "59" = "r2019",
               "60" = "r2020",
               "61" = "r2021",
               "62" = "r2022",
               "63" = "r2023",
               "91" = "Hidrografia",
               "99" = "Nuvem",
               "100" = "Vegetação Nativa",
               "101" = "Não Floresta"),
    version = "v1-crop"
)

# Set the directory for PRODES data
data_dir_prd2024 <- "data/raw/masks/prodes-mask-forest/2024"

# Recover the PRODES classified cube
prodes_2024 <- sits_cube(
    source = "USGS",
    collection = "LANDSAT-C2L2-SR",
    multicores = multicores,
    memsize = memsize,
    data_dir = data_dir_prd2024,
    parse_info = c("product", "sensor",
                   "tile", "start_date", "end_date",
                   "band", "version"),
    bands = "class",
    labels = c("0" = "d2000",
               "2" = "d2002",
               "4" = "d2004",
               "6" = "d2006",
               "7" = "d2007",
               "8" = "d2008",
               "9" = "d2009",
               "10" = "d2010",
               "11" = "d2011",
               "12" = "d2012",
               "13" = "d2013",
               "14" = "d2014",
               "15" = "d2015",
               "16" = "d2016",
               "17" = "d2017",
               "18" = "d2018",
               "19" = "d2019",
               "20" = "d2020",
               "21" = "d2021",
               "22" = "d2022",
               "23" = "d2023",
               "24" = "d2024",
               "50" = "r2010",
               "51" = "r2011",
               "52" = "r2012",
               "53" = "r2013",
               "54" = "r2014",
               "55" = "r2015",
               "56" = "r2016",
               "57" = "r2017",
               "58" = "r2018",
               "59" = "r2019",
               "60" = "r2020",
               "61" = "r2021",
               "62" = "r2022",
               "63" = "r2023",
               "64" = "r2024",
               "91" = "Hidrografia",
               "100" = "Vegetação Nativa",
               "101" = "Não Floresta"),
    version = "v1"
)

output_dir <- "data/raw/mask/prodes-mask-forest/2024/"
dir.create(output_dir, recursive = TRUE)
sits_reclassify(
    cube = prodes_2023,
    mask = prodes_2024,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | (cube == "Nuvem" & mask == "Vegetação Nativa")
    ),
    multicores = 32,
    memsize = 180,
    version = "with-cloud",
    output_dir = output_dir
)

output_dir <- "data/raw/mask/prodes-mask-forest/2022/"
dir.create(output_dir, recursive = TRUE)
sits_reclassify(
    cube = prodes_2023,
    mask = prodes_2023,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube == "d2023"
    ),
    multicores = 64,
    memsize = 200,
    output_dir = output_dir
)

output_dir <- "/data/PRODES-MASK-FOREST/2021/"
dir.create(output_dir, recursive = TRUE)
sits_reclassify(
    cube = prodes_2023,
    mask = prodes_2023,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2023", "d2022")
    ),
    multicores = 64,
    memsize = 200,
    output_dir = output_dir
)

output_dir <- "/data/PRODES-MASK-FOREST/2020/"
dir.create(output_dir, recursive = TRUE)
sits_reclassify(
    cube = prodes_2023,
    mask = prodes_2023,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2023", "d2022", "d2021")
    ),
    multicores = 64,
    memsize = 200,
    output_dir = output_dir
)

output_dir <- "/data/PRODES-MASK-FOREST/2019/"
dir.create(output_dir, recursive = TRUE)
sits_reclassify(
    cube = prodes_2023,
    mask = prodes_2023,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2023", "d2022", "d2021", "d2020")
    ),
    multicores = 64,
    memsize = 200,
    output_dir = output_dir
)

output_dir <- "/data/PRODES-MASK-FOREST/2018/"
dir.create(output_dir, recursive = TRUE)
sits_reclassify(
    cube = prodes_2023,
    mask = prodes_2023,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2023", "d2022", "d2021", "d2020", "d2019")
    ),
    multicores = 64,
    memsize = 200,
    output_dir = output_dir
)


output_dir <- "data/derived/prodes-mask-forest/2017/"
dir.create(output_dir, recursive = TRUE)
sits_reclassify(
    cube = prodes_2023,
    mask = prodes_2023,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2023", "d2022", "d2021", "d2020", "d2019", "d2018")
    ),
    multicores = 40,
    memsize = 180,
    output_dir = output_dir
)

output_dir <- "data/derived/prodes-mask-forest/2016/"
dir.create(output_dir, recursive = TRUE)
sits_reclassify(
    cube = prodes_2023,
    mask = prodes_2023,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2023", "d2022", "d2021", "d2020", "d2019", "d2018", "d2017")
    ),
    multicores = 64,
    memsize = 200,
    output_dir = output_dir
)

output_dir <- "data/derived/prodes-mask-forest/2015/"
dir.create(output_dir, recursive = TRUE)
sits_reclassify(
    cube = prodes_2023,
    mask = prodes_2023,
    rules = list(
        "Vegetação Nativa" = cube == "Vegetação Nativa" | cube %in% c(
            "d2023", "d2022", "d2021", "d2020", "d2019", "d2018", "d2017", "d2016")
    ),
    multicores = 64,
    memsize = 200,
    output_dir = output_dir
)
