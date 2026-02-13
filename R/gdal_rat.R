#' @title Configure QML style in a raster as RAT
#' 
#' @description Configures the QML style in a raster using RAT.
#' 
#' @param map Path to raster file
#' @param qml Path to QML file
#' @param band Band number
#' 
#' @returns Path to raster file
#' 
#' @keywords internal
#' @export
rat_set_style <- function(map, qml, band = 1) {
    # Check if map exist
    if (!fs::file_exists(map)) {
        cli::cli_abort(glue::glue("{map} doesn't exist"))
    }

    # Check if file exist
    if (!fs::file_exists(qml)) {
        cli::cli_abort(glue::glue("{qml} doesn't exist"))
    }

    # Load map
    qml_style <- .qml_to_tibble(qml)

    # Prepare pixel style
    pixel_style <- qml_style |>
                    dplyr::select(
                        .data[["pixel"]],
                        .data[["r"]],
                        .data[["g"]],
                        .data[["b"]]
                    )

    # Prepare pixel labels
    pixel_labels <- qml_style |>
                    dplyr::transmute(
                        VALUE = .data[["pixel"]],
                        Name  = .data[["label"]],
                        R     = .data[["r"]],
                        G     = .data[["g"]],
                        B     = .data[["b"]]
                    )

    # Load raster
    raster <- new(gdalraster::GDALRaster, map, read_only = FALSE)

    # Set color table
    raster$setColorTable(band, pixel_style, "RGB")

    # Create RAT
    rat <- gdalraster::buildRAT(
        raster     = raster,
        band       = band,
        table_type = "thematic",
        na_value   = NULL,
        join_df    = pixel_labels
    )

    # Set raster
    raster$setDefaultRAT(band, rat)

    # Close raster
    raster$flushCache()
    raster$close()

    # Return!
    return(map)
}
