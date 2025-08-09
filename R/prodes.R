.prodes_dir <- function(version, year) {
    prodes_base_dir <- "data/derived/masks/base/prodes"
    prodes_base_dir <- .project_env_variable("MASK_PRODES_BASE_DIR", prodes_base_dir)

    fs::path(prodes_base_dir) / version / year
}

.prodes_rds <- function(prodes_dir) {
    fs::path(prodes_dir) / "prodes.rds"
}

#' @export
load_prodes_2014 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2014)
    prodes_rds <- .prodes_rds(prodes_dir)

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


#' @export
load_prodes_2015 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2015)
    prodes_rds <- .prodes_rds(prodes_dir)

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

#' @export
load_prodes_2016 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2016)
    prodes_rds <- .prodes_rds(prodes_dir)

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

#' @export
load_prodes_2017 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2017)
    prodes_rds <- .prodes_rds(prodes_dir)

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

#' @export
load_prodes_2018 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2018)
    prodes_rds <- .prodes_rds(prodes_dir)

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

#' @export
load_prodes_2019 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2019)
    prodes_rds <- .prodes_rds(prodes_dir)

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

#' @export
load_prodes_2020 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2020)
    prodes_rds <- .prodes_rds(prodes_dir)

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

#' @export
load_prodes_2021 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2021)
    prodes_rds <- .prodes_rds(prodes_dir)

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


#' @export
load_prodes_2022 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2022)
    prodes_rds <- .prodes_rds(prodes_dir)

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

#' @export
load_prodes_2023 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2023)
    prodes_rds <- .prodes_rds(prodes_dir)

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

#' @export
load_prodes_2024 <- function(version = "v2", multicores = 32, memsize = 120) {
    prodes_dir <- .prodes_dir(version = version, year = 2024)
    prodes_rds <- .prodes_rds(prodes_dir)

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
