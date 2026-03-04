#' @title Rasterize vector segments using GDAL
#'
#' @description
#' Converts vector segments to raster format using \code{gdalUtilities::gdal_rasterize}.
#' The function reads a vector file, converts class attributes to numeric values,
#' and rasterizes them at the specified resolution.
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param file Input vector file path (e.g., .gpkg, .shp)
#' @param res Target resolution for the output raster (in map units)
#' @param output_dir Directory where the output raster will be saved
#' @param crs Output CRS
#' @param style Optional data frame with columns 'name', 'index', and 'color'
#'   for class mapping. If NULL, classes are automatically numbered.
#' @param type GDAL data type for output raster (default: "Int16")
#' @param gdal_config Named list of additional configuration options to pass
#'   to gdalUtilities::gdal_rasterize. These will override or extend default
#'   GDAL options. Example: list(co = c("COMPRESS=LZW"), init = 0)
#'
#' @returns Path to the generated raster file
#'
#' @examples
#' \dontrun{
#' gdal_rasterize_segments(
#'   file = "segments.gpkg",
#'   res = 30,
#'   output_dir = "output/"
#' )
#' }
#'
#' @export
gdal_rasterize_segments <- function(file, res, output_dir, crs = crs_bdc(),
                                    style = NULL, type = "Int16", gdal_config = list()) {
    stopifnot(!is.null(res))
    stopifnot(!is.null(file))
    stopifnot(!is.null(output_dir))

    # create output dir
    fs::dir_create(output_dir, recurse = TRUE)

    # expand paths
    file <- fs::path_expand(file)
    output_dir <- fs::path_expand(output_dir)

    # define output files
    output_file_base <- fs::path(output_dir) / fs::path_file(file) |>
                        fs::path_ext_remove()

    output_file <- stringr::str_c(output_file_base, ".tif")
    output_style <- stringr::str_c(output_file_base, ".qml")

    if (fs::file_exists(output_file)) {{
        return(output_file)
    }}

    file_sf <- sf::st_read(file, quiet = TRUE)

    if (is.null(style)) {
        cli::cli_alert_warning(
            paste0(
                "Style not defined. Using random colors, ",
                "which is not recommended"
            )
        )

        file_sf <- file_sf |>
            dplyr::mutate(class_num = .data[["class"]] |>
                              as.factor() |>
                              as.numeric())

        style <- file_sf |>
            tibble::as_tibble() |>
            dplyr::select(dplyr::all_of(c("class", "class_num"))) |>
            dplyr::distinct(.data[["class"]], .data[["class_num"]]) |>
            dplyr::mutate(color = pals::brewer.piyg(n = dplyr::n())) |>
            dplyr::rename("index" = "class_num",
                          "color" = "color",
                          "name" = "class") |>
            dplyr::arrange(.data[["index"]])

    } else {
        file_sf <- file_sf |>
            dplyr::rename("name" = "class") |>
            dplyr::left_join(style |> dplyr::select(dplyr::all_of(c(
                "name", "index"
            )))) |>
            dplyr::mutate(
                class_num = .data[["index"]]
            )
    }

    file_bbox <- sf::st_bbox(file_sf)

    # Create vector file with `class` converted
    file_gpkg <- fs::file_temp(ext = ".gpkg")

    sf::st_write(obj = file_sf, dsn = file_gpkg)

    # Prepare default GDAL rasterize options
    default_config <- list(
        src_datasource = file_gpkg,
        dst_filename   = output_file,
        a              = "class_num",
        at             = TRUE,
        q              = TRUE,
        tr             = c(res, res),
        te             = file_bbox,
        ot             = type,
        init           = 255,
        a_nodata       = 255,
        co             = c(
            "COMPRESS=ZSTD",
            "PREDICTOR=2",
            "ZSTD_LEVEL=1",
            "BIGTIFF=YES"
        ),
        a_srs          = crs_bdc()
    )

    # Merge user config with defaults (user config takes precedence)
    final_config <- utils::modifyList(default_config, gdal_config)

    # Rasterize using merged configuration
    do.call(gdalUtilities::gdal_rasterize, final_config)

    # Save style
    sits:::.colors_qml(color_table = style, file = output_style)

    # Return!
    output_file
}
