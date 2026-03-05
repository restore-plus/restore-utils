.terraclass_dir <- function(version, year) {
    tc_base_dir <- "data/derived/masks/base/terraclass"
    tc_base_dir <- .project_env_variable("MASK_TERRACLASS_BASE_DIR", tc_base_dir)

    fs::path(tc_base_dir) / version / year
}

.terraclass_rds <- function(terraclass_dir) {
    fs::path(terraclass_dir) / "terraclass.rds"
}

#' @export
load_terraclass_2004 <- function(version = "v1", multicores = 32, memsize = 120) {
    terraclass_dir <- .terraclass_dir(version, 2004)
    terraclass_rds <- .terraclass_rds(terraclass_dir)

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {
        # We decided to maintain the original labels name because we only
        # used MINERACAO and URBANIZADA classes, so, we just harmonized both.
        terraclass <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = terraclass_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("satellite", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("84" = "AREA_SEM_INFORMACAO_EM_2004",
                       "85" = "AGRICULTURA_ANUAL",
                       "86" = "MINERACAO",
                       "87" = "VEGETACAO_SECUNDARIA",
                       "88" = "REGENERACAO_COM_PASTO",
                       "89" = "PASTO_SUJO",
                       "90" = "PASTO_LIMPO",
                       "91" = "PASTO_COM_SOLO_EXPOSTO",
                       "92" = "OUTROS",
                       "93" = "NAO_FLORESTA",
                       "94" = "MOSAICO_DE_OCUPACOES",
                       "95" = "HIDROGRAFIA",
                       "96" = "FLORESTA",
                       "97" = "DESFLORESTAMENTO_2004",
                       "98" = "URBANIZADA", # original label was AREA_URBANA
                       "99" = "AREA_NAO_OBSERVADA"
            )
        )

        saveRDS(terraclass, terraclass_rds)
    }
    terraclass
}

#' @export
load_terraclass_2008 <- function(version = "v1", multicores = 32, memsize = 120) {
    terraclass_dir <- .terraclass_dir(version, 2008)
    terraclass_rds <- .terraclass_rds(terraclass_dir)

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits::sits_cube(
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
                       "52" = "CULTURA AGRICOLA TEMPORARIA",
                       "53" = "NAO-URBANO"
            )
        )

        saveRDS(terraclass, terraclass_rds)
    }
    terraclass
}

#' @export
load_terraclass_2012 <- function(version = "v1", multicores = 32, memsize = 120) {
    terraclass_dir <- .terraclass_dir(version, 2012)
    terraclass_rds <- .terraclass_rds(terraclass_dir)

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits::sits_cube(
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
                       "52" = "CULTURA AGRICOLA TEMPORARIA",
                       "53" = "NAO-URBANO"
            )
        )

        saveRDS(terraclass, terraclass_rds)
    }
    terraclass
}

#' @export
load_terraclass_2010 <- function(version = "v1", multicores = 32, memsize = 120) {
    terraclass_dir <- .terraclass_dir(version, 2010)
    terraclass_rds <- .terraclass_rds(terraclass_dir)

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits::sits_cube(
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
                       "52" = "CULTURA AGRICOLA TEMPORARIA",
                       "53" = "NAO-URBANO"
            )
        )

        saveRDS(terraclass, terraclass_rds)
    }
    terraclass
}

#' @export
load_terraclass_2014 <- function(version = "v1", multicores = 32, memsize = 120, use_original = FALSE) {
    # Define default directory
    terraclass_dir <- .terraclass_dir(version, 2014)

    # Define default labels
    labels_default <- c(
        "1" = "VEGETACAO NATURAL FLORESTAL PRIMARIA",
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
        "52" = "CULTURA AGRICOLA TEMPORARIA",
        "53" = "NAO-URBANO"
    )

    if (use_original) {
        # Define original directory
        terraclass_dir <- .terraclass_dir("original", 2014)

        # Define default labels
        labels_default <- c(
            "1" = "VEGETACAO NATURAL FLORESTAL PRIMARIA",
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
    }

    # Define RDS file
    terraclass_rds <- .terraclass_rds(terraclass_dir)


    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = terraclass_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("satellite", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands  = "class",
            labels = labels_default
        )

        saveRDS(terraclass, terraclass_rds)
    }
    terraclass
}

#' @export
load_terraclass_2016 <- function(version = "v1", multicores = 32, memsize = 120) {
    terraclass_dir <- .terraclass_dir(version, 2016)
    terraclass_rds <- .terraclass_rds(terraclass_dir)

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits::sits_cube(
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
                       "51" = "NAO FLORESTA"
            )
        )

        saveRDS(terraclass, terraclass_rds)
    }

    terraclass
}

#' @export
load_terraclass_2018 <- function(version = "v1", multicores = 32, memsize = 120) {
    terraclass_dir <- .terraclass_dir(version, 2018)
    terraclass_rds <- .terraclass_rds(terraclass_dir)

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits::sits_cube(
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
                       "51" = "NAO FLORESTA",
                       "52" = "NAO-URBANO"
            )
        )

        saveRDS(terraclass, terraclass_rds)
    }

    terraclass
}

#' @export
load_terraclass_cerrado_2018 <- function(version = "v1", multicores = 32, memsize = 120) {
    terraclass_dir <- .terraclass_dir(version, 2018)
    terraclass_rds <- .terraclass_rds(terraclass_dir)

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits::sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = terraclass_dir,
            multicores = multicores,
            memsize = memsize,
            parse_info = c("satellite", "sensor",
                           "tile", "start_date", "end_date",
                           "band", "version"),
            bands = "class",
            labels = c("1" = "VEGETACAO NATURAL PRIMARIA",
                       "2" = "VEGETACAO NATURAL SECUNDARIA",
                       "9" = "SILVICULTURA",
                       "11" = "PASTAGEM",
                       "12" = "CULTURA AGRICOLA PERENE",
                       "13" = "CULTURA AGRICOLA SEMIPERENE",
                       "14" = "CULTURA AGRICOLA TEMPORARIA DE 1 CICLO",
                       "15" = "CULTURA AGRICOLA TEMPORARIA DE MAIS DE 1 CICLO",
                       "16" = "MINERACAO",
                       "17" = "URBANIZADA",
                       "20" = "OUTROS USOS",
                       "21" = "OUTRAS ÁREAS EDIFICADAS",
                       "22" = "DESFLORESTAMENTO NO ANO",
                       "23" = "CORPO DAGUA",
                       "25" = "NAO OBSERVADO"
            )
        )

        saveRDS(terraclass, terraclass_rds)
    }
    terraclass
}

#' @export
load_terraclass_2020 <- function(version = "v1", multicores = 32, memsize = 120) {
    terraclass_dir_2020 <- .terraclass_dir(version, 2020)
    terraclass_rds_2020 <- .terraclass_rds(terraclass_dir_2020)

    if (fs::file_exists(terraclass_rds_2020)) {

        terraclass_2020 <- readRDS(terraclass_rds_2020)

    } else {

        terraclass_2020 <- sits::sits_cube(
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
                       "51" = "NAO FLORESTA",
                       "52" = "NAO-URBANO"
            )
        )

        saveRDS(terraclass_2020, terraclass_rds_2020)
    }
    terraclass_2020
}

#' @export
load_terraclass_2022 <- function(version = "v1", multicores = 32, memsize = 120) {
    terraclass_dir <- .terraclass_dir(version, 2022)
    terraclass_rds <- .terraclass_rds(terraclass_dir)

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits::sits_cube(
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
                       "51" = "NAO FLORESTA"
            )
        )
        saveRDS(terraclass, terraclass_rds)
    }
    terraclass
}

#' @export
load_terraclass_2024 <- function(version = "v1", multicores = 32, memsize = 120) {
    terraclass_dir <- .terraclass_dir(version, 2024)
    terraclass_rds <- .terraclass_rds(terraclass_dir)

    if (fs::file_exists(terraclass_rds)) {

        terraclass <- readRDS(terraclass_rds)

    } else {

        terraclass <- sits::sits_cube(
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
                       "26" = "NAO FLORESTA"
            )
        )
        saveRDS(terraclass, terraclass_rds)
    }
    terraclass
}
