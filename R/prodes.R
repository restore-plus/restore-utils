.prodes_dir <- function(version, year) {
    prodes_base_dir <- "data/derived/masks/base/prodes"
    prodes_base_dir <- .project_env_variable("MASK_PRODES_BASE_DIR", prodes_base_dir)

    fs::path(prodes_base_dir) / version / year
}

.prodes_rds <- function(prodes_dir) {
    fs::path(prodes_dir) / "prodes.rds"
}

#' @export
prodes_generate_forest_mask <- function(target_year, version = "v2", multicores = 32, memsize = 120) {
    if (target_year >= 2024) {
        cli::cli_abort(
            "Nothing to do: 2024 is the most recent year; the forest in this year represents the actual available forest"
        )
    }

    # Define deforestation years (we move from the future to the past).
    # The goal of this function is to "regenerate the forest area".
    # The idea is as follows:
    # If an area is known to be deforested in 2024, by definition, it was forest in 2023.
    # Generalizing: if an area is deforested in year `t`, it was forest in year `t - 1`.
    #
    # Therefore, to generate PRODES with a forest mask, we proceed as follows:
    # Example:
    #   target year = 2016
    #   deforestation years = 2024 – 2017
    # All of these deforestation years correspond to forest in 2016.
    deforestation_years <- paste0("d", (target_year + 1):2024)

    # Load PRODES 2024 cube
    # Why 2024 ? This is the current version of PRODES available. We always go
    # from the latest PRODES year available.
    prodes_cube <- load_prodes_2024(
        version    = version,
        multicores = multicores,
        memsize    = memsize
    )

    # Define output dir
    output_dir <- .prodes_dir(version = version, year = target_year)

    # Create output dir
    fs::dir_create(output_dir)

    # build expression
    rules_expression <- bquote(
        list(
            "Vegetação Nativa" = cube == "Vegetação Nativa" |
                                 cube %in% .(deforestation_years)
        )
    )

    # reclassify!
    eval(bquote(
        sits_reclassify(
            cube = prodes_cube,
            mask = prodes_cube,
            rules = .(rules_expression),
            multicores = multicores,
            memsize = memsize,
            output_dir = output_dir,
            version = "v1"
        )
    ))
}

#' @export
load_prodes_2000 <- function(version = "v2", multicores = 32, memsize = 120) {
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
            labels = c("0"   = "d2000",
                       "91"  = "Hidrografia",
                       "100" = "Vegetação Nativa",
                       "101" = "Não Floresta")
        )

        saveRDS(prodes, prodes_rds)
    }
    prodes
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
