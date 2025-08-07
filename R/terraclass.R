#' @export
load_terraclass_2014 <- function(multicores = 32, memsize = 120) {
    terraclass_dir <- "data/raw/masks/terraclass-mask/2014/"
    terraclass_rds <- fs::path(terraclass_dir) / "terraclass.rds"

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = terraclass_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("satellite", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("1" = "VEGETACAO NATURAL FLORESTAL PRIMARIA",
                       "2" = "VEGETACAO NATURAL FLORESTAL SECUNDARIA",
                       "9" = "SILVICULTURA",
                       "10" = "PASTAGEM ARBUSTIVA/ARBOREA",
                       "11" = "PASTAGEM HERBACEA",
                       "12" = "CULTURA AGRICOLA PERENE",
                       "13" = "CULTURA AGRICOLA SEMIPERENE",
                       "16" = "MINERACAO",
                       "17" = "URBANIZADA",
                       "20" = "OUTROS USOS",
                       "22" = "DESFLORESTAMENTO NO ANO",
                       "23" = "CORPO DAGUA",
                       "25" = "NAO OBSERVADO",
                       "50" = "NAO FLORESTA",
                       "52" = "CULTURA AGRICOLA TEMPORARIA"
            )
        )

        saveRDS(terraclass, terraclass_rds)
    }
    terraclass
}

#' @export
load_terraclass_2018 <- function(multicores = 32, memsize = 120) {
    terraclass_dir <- "data/raw/masks/terraclass-mask/2018/"
    terraclass_rds <- fs::path(terraclass_dir) / "terraclass.rds"

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = terraclass_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("satellite", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("1" = "VEGETACAO NATURAL FLORESTAL PRIMARIA",
                       "2" = "VEGETACAO NATURAL FLORESTAL SECUNDARIA",
                       "9" = "SILVICULTURA",
                       "10" = "PASTAGEM ARBUSTIVA/ARBOREA",
                       "11" = "PASTAGEM HERBACEA",
                       "12" = "CULTURA AGRICOLA PERENE",
                       "13" = "CULTURA AGRICOLA SEMIPERENE",
                       "14" = "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                       "15" = "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO",
                       "16" = "MINERACAO",
                       "17" = "URBANIZADA",
                       "20" = "OUTROS USOS",
                       "22" = "DESFLORESTAMENTO NO ANO",
                       "23" = "CORPO DAGUA",
                       "25" = "NAO OBSERVADO",
                       "51" = "NATURAL NAO FLORESTAL"
            )
        )

        saveRDS(terraclass, terraclass_rds)
    }

    terraclass
}

#' @export
load_terraclass_2020 <- function(multicores = 32, memsize = 120) {
    terraclass_dir_2020 <- "data/raw/masks/terraclass-mask/2020/"
    terraclass_rds_2020 <- fs::path(terraclass_dir_2020) / "terraclass.rds"

    if (fs::file_exists(terraclass_rds_2020)) {

        terraclass_2020 <- readRDS(terraclass_rds_2020)

    } else {

        terraclass_2020 <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = terraclass_dir_2020,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("satellite", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("1" = "VEGETACAO NATURAL FLORESTAL PRIMARIA",
                       "2" = "VEGETACAO NATURAL FLORESTAL SECUNDARIA",
                       "9" = "SILVICULTURA",
                       "10" = "PASTAGEM ARBUSTIVA/ARBOREA",
                       "11" = "PASTAGEM HERBACEA",
                       "12" = "CULTURA AGRICOLA PERENE",
                       "13" = "CULTURA AGRICOLA SEMIPERENE",
                       "14" = "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                       "15" = "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO",
                       "16" = "MINERACAO",
                       "17" = "URBANIZADA",
                       "20" = "OUTROS USOS",
                       "22" = "DESFLORESTAMENTO NO ANO",
                       "23" = "CORPO DAGUA",
                       "25" = "NAO OBSERVADO",
                       "51" = "NATURAL NAO FLORESTAL"
            )
        )

        saveRDS(terraclass_2020, terraclass_rds_2020)
    }
    terraclass_2020
}

#' @export
load_terraclass_2022 <- function(multicores = 32, memsize = 120) {
    terraclass_dir <- "data/raw/masks/terraclass-mask/2022/"
    terraclass_rds <- fs::path(terraclass_dir) / "terraclass.rds"

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = terraclass_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("satellite", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("1" = "VEGETACAO NATURAL FLORESTAL PRIMARIA",
                       "2" = "VEGETACAO NATURAL FLORESTAL SECUNDARIA",
                       "9" = "SILVICULTURA",
                       "10" = "PASTAGEM ARBUSTIVA/ARBOREA",
                       "11" = "PASTAGEM HERBACEA",
                       "12" = "CULTURA AGRICOLA PERENE",
                       "13" = "CULTURA AGRICOLA SEMIPERENE",
                       "14" = "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                       "15" = "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO",
                       "16" = "MINERACAO",
                       "17" = "URBANIZADA",
                       "20" = "OUTROS USOS",
                       "22" = "DESFLORESTAMENTO NO ANO",
                       "23" = "CORPO DAGUA",
                       "25" = "NAO OBSERVADO",
                       "51" = "NATURAL NAO FLORESTAL"
            )
        )
        saveRDS(terraclass, terraclass_rds)
    }
    terraclass
}
