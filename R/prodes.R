#' @export
load_prodes_2014 <- function(multicores = 32, memsize = 120) {
    prodes_dir <- "data/raw/masks/prodes-mask-forest/2014/"
    prodes_rds <- fs::path(prodes_dir) / "prodes.rds"

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
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
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "91" = "Hidrografia",
                       "99" = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "Não Floresta")
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}


#' @export
load_prodes_2015 <- function(multicores = 32, memsize = 120) {
    prodes_dir <- "data/raw/masks/prodes-mask-forest/2015/"
    prodes_rds <- fs::path(prodes_dir) / "prodes.rds"

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
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
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "91" = "Hidrografia",
                       "99" = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "Não Floresta")
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2016 <- function(multicores = 32, memsize = 120) {
    prodes_dir <- "data/raw/masks/prodes-mask-forest/2016/"
    prodes_rds <- fs::path(prodes_dir) / "prodes.rds"

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
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
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "91" = "Hidrografia",
                       "99" = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "Não Floresta")
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2017 <- function(multicores = 32, memsize = 120) {
    prodes_dir <- "data/raw/masks/prodes-mask-forest/2017/"
    prodes_rds <- fs::path(prodes_dir) / "prodes.rds"

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
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
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "91" = "Hidrografia",
                       "99" = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "Não Floresta")
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2018 <- function(multicores = 32, memsize = 120) {
    prodes_dir <- "data/raw/masks/prodes-mask-forest/2018/"
    prodes_rds <- fs::path(prodes_dir) / "prodes.rds"

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
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
                       "50" = "r2010",
                       "51" = "r2011",
                       "52" = "r2012",
                       "53" = "r2013",
                       "54" = "r2014",
                       "55" = "r2015",
                       "56" = "r2016",
                       "57" = "r2017",
                       "58" = "r2018",
                       "91" = "Hidrografia",
                       "99" = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "Não Floresta")
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2019 <- function(multicores = 32, memsize = 120) {
    prodes_dir <- "data/raw/masks/prodes-mask-forest/2019/"
    prodes_rds <- fs::path(prodes_dir) / "prodes.rds"

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
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
                       "91" = "Hidrografia",
                       "99" = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "Não Floresta")
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2020 <- function(multicores = 32, memsize = 120) {
    prodes_dir <- "data/raw/masks/prodes-mask-forest/2020/"
    prodes_rds <- fs::path(prodes_dir) / "prodes.rds"

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
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
                       "91" = "Hidrografia",
                       "99" = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "Não Floresta")
        )

        saveRDS(prodes, prodes_rds)
    }

    prodes
}

#' @export
load_prodes_2021 <- function(multicores = 32, memsize = 120) {
    prodes_dir <- "data/raw/masks/prodes-mask-forest/2021/"
    prodes_rds <- fs::path(prodes_dir) / "prodes.rds"

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
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
                       "91" = "Hidrografia",
                       "99" = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "Não Floresta")
        )
        saveRDS(prodes, prodes_rds)
    }

    prodes
}


#' @export
load_prodes_2022 <- function(multicores = 32, memsize = 120) {
    prodes_dir <- "data/raw/masks/prodes-mask-forest/2022"
    prodes_rds <- fs::path(prodes_dir) / "prodes.rds"

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
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
                       "91" = "Hidrografia",
                       "99" = "Nuvem",
                       "100" = "Vegetação Nativa",
                       "101" = "Não Floresta")
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2023 <- function(multicores = 32, memsize = 120) {
    prodes_dir <- "data/raw/masks/prodes-mask-forest/2023"
    prodes_rds <- fs::path(prodes_dir) / "prodes.rds"

    if (fs::file_exists(prodes_rds)) {
        prodes <- readRDS(prodes_rds)
    } else {
        # Recover the PRODES classified cube
        prodes <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
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
                       "101" = "Não Floresta")
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
}

#' @export
load_prodes_2024 <- function(multicores = 32, memsize = 120) {
    prodes_dir <- "data/raw/masks/prodes-mask-forest/2024"
    prodes_rds <- fs::path(prodes_dir) / "prodes.rds"

    if (fs::file_exists(prodes_rds)) {

        prodes <- readRDS(prodes_rds)

    } else {
        # Recover the PRODES classified cube
        prodes <- sits_cube(
            source = "MPC",
            collection = "LANDSAT-C2-L2",
            data_dir = prodes_dir,
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
                       "101" = "Não Floresta")
        )

        saveRDS(prodes, prodes_rds)
    }

    prodes
}
