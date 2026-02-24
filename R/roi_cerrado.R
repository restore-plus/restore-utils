#' @title Get the data file for the Cerrado ecoregions
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description This function returns the data file for the Cerrado ecoregions.
#'
#' @note The function by default uses the data file in the package extdata directory. The user
#'      can override this by setting the environment variable \code{RESTORE_PLUS_CERRADO_REGION_ROI_FILE}
#'      to the desired file path.
#'
#' @return Character path to the data file
#'
#' @keywords internal
#' @noRd
.roi_cerrado_ecoregion_data_file <- function() {
    default_file <- system.file("extdata/cerrado/cerrado-regions-bdc-md.gpkg", package = "restoreutils")

    fs::path(Sys.getenv("RESTORE_PLUS_AMAZON_REGION_ROI_FILE", default_file))
}

#' @title Get the name of the Cerrado ecoregion
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param region_id Integer with the region id (1 to 4).
#'
#' @description This function returns the name of the Cerrado ecoregion
#'
#' @return Character name of the Cerrado ecoregion
#' @keywords internal
.roi_cerrado_ecoregion_name <- function(region_id) {
    paste0("Q", region_id)
}

#' @title Get the sf object for the Cerrado ecoregions
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param crs Character with the CRS to use. If not provided, the CRS of the data file is used.
#' @param region_id Integer with the region id (1 to 4). If not provided, all regions are returned.
#'
#' @description This function returns the sf object for the Cerrado ecoregions.
#'
#' @return sf object for the Cerrado ecoregions
#' @keywords internal
.roi_cerrado_ecoregion_sf <- function(crs, region_id = NULL) {
    # Define region name
    region_name <- NULL

    # If region id is provided, use it to generate the region name
    if (!is.null(region_id)) {
        region_name <- .roi_cerrado_ecoregion_name(region_id)
    }

    # Load ecoregion roi file
    eco_region <- sf::st_read(.roi_cerrado_ecoregion_data_file(), quiet = TRUE)

    # Transform / Filter region
    if (!is.null(crs)) {
        eco_region <- suppressWarnings(sf::st_transform(eco_region, crs = crs))
    }

    # Filter region if region name is defined
    if (!is.null(region_name)) {
        eco_region <- dplyr::filter(eco_region, part == !!region_name)
    }

    # Select columns
    eco_region |>
            dplyr::mutate(tile_id = .data[["tile"]]) |>
            dplyr::select(-name_biome, -code_biome, -year, -tile)
}

#' @title Get the sf object for the Cerrado ecoregions
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description This function returns the sf object for the Cerrado ecoregions.
#'
#' @param region_id Integer with the region id (1 to 4).
#' @param crs Character with the CRS to use. If not provided, the CRS of the data file is used.
#' @param as_file Logical with whether to return the \code{sf} object as a file path.
#' @param as_union Logical with whether to union the \code{sf} objects.
#' @param as_convex Logical with whether to return the \code{sf} object as a convex hull.
#'
#' @return \code{sf} or file path object for the Cerrado ecoregions
#' @keywords internal
#' @export
roi_cerrado_regions <- function(region_id, crs = NULL, as_file = FALSE, as_union = FALSE, as_convex = FALSE) {
    # Check region id
    if (is.null(region_id)) {
        cli::cli_abort("Region id is required. To load all regions, use {.code roi_cerrado_biome()}.")
    }

    # Validate region id value
    if (region_id < 1 || region_id > 4) {
        cli::cli_abort("Region id must be between 1 and 4. To load all regions, use {.code roi_cerrado_biome()}.")
    }

    # generate eco region geometry
    eco_region_geom <- .roi_cerrado_ecoregion_sf(
        region_id = region_id,
        crs = crs
    )

    # Union
    if (as_union) {
        if (use_buffer) {
            eco_region_geom <- sf::st_buffer(eco_region_geom, 0.001)
        }

        eco_region_geom <- eco_region_geom |>
            sf::st_union() |>
            sf::st_make_valid()
    }

    # Transform convex hull
    if (as_convex) {
        eco_region_geom <- sf::st_union(eco_region_geom) |>
            sf::st_convex_hull()
    }

    if (as_file) {
        # Create temp file
        eco_region_file <- fs::file_temp(ext = "gpkg")

        # Save sf
        sf::st_write(eco_region_geom, eco_region_file, quiet = TRUE)

        # Update result variable
        eco_region_geom <- eco_region_file
    }

    # return!
    return(eco_region_geom)
}

#' @title Get the sf object for the Cerrado biome
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @param crs Character with the CRS to use. If not provided, the CRS of the data file is used.
#' @param as_file Logical with whether to return the \code{sf} object as a file path.
#' @param as_convex Logical with whether to return the sf object as a convex hull.
#' @param as_union Logical with whether to union the \code{sf} objects.
#' @param use_buffer Logical with whether to apply a buffer to the \code{sf} object.
#'
#' @description This function returns the \code{sf} object for the Cerrado biome.
#'
#' @return \code{sf} or file path object for the Cerrado biome
#' @keywords internal
#' @export
roi_cerrado_biome <- function(crs = NULL, as_file = FALSE, as_convex = FALSE, as_union = FALSE, use_buffer = FALSE) {
    # generate eco region geometry
    cerrado_geom <- .roi_cerrado_ecoregion_sf(crs = crs) |>
                        sf::st_make_valid()

    # Union
    if (as_union) {
        if (use_buffer) {
            cerrado_geom <- sf::st_buffer(cerrado_geom, 0.001)
        }

        cerrado_geom <- cerrado_geom |>
            sf::st_union() |>
            sf::st_make_valid()
    }

    # Transform convex hull
    if (as_convex) {
        cerrado_geom <- sf::st_union(cerrado_geom) |>
            sf::st_convex_hull()
    }

    # sf as file
    if (as_file) {
        # create temp file
        cerrado_file <- fs::file_temp(ext = "gpkg")

        # Save sf
        sf::st_write(cerrado_geom, cerrado_file, quiet = TRUE)

        # Update result variable
        cerrado_geom <- cerrado_file
    }

    # Return!
    return(cerrado_geom)
}

#' @title Get the tiles for the Cerrado biome
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description This function returns the tiles for the Cerrado biome.
#'
#' @param grid_system Character with the grid system to use.
#'
#' @return \code{sf} object with the tiles for the Cerrado biome
#' @keywords internal
#' @export
tiles_cerrado_biome <- function(grid_system = "BDC_MD_V2") {
    # Generate eco region geometry
    cerrado_geom <- roi_cerrado_biome(as_union = TRUE)

    # Convert to tiles and return
    suppressWarnings(
        sits::sits_roi_to_tiles(cerrado_geom, grid_system = grid_system)
    )
}
