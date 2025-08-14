.roi_ecoregion_data_file <- function() {
    default_file <- system.file("extdata/amazon/amazon-regions-bdc-md.gpkg", package = "restoreutils")

    fs::path(Sys.getenv("RESTORE_PLUS_REG3_ROI_FILE", default_file))
}

.roi_ecoregion_name <- function(region_id) {
    paste0("eco_", region_id)
}

.roi_ecoregion_sf <- function(region_id, crs) {
    # Region name
    eco_name <- .roi_ecoregion_name(region_id)

    # Load ecoregion roi file
    eco_region <- suppressMessages(sf::st_read(.roi_ecoregion_data_file()))

    # Transform / Filter region (region 3)
    eco_region <- sf::st_transform(eco_region, crs = crs)
    eco_region <- dplyr::filter(eco_region, layer == eco_name) |>
        dplyr::select(-gid, -id, -grs_schema)
}

#' @export
roi_ecoregions <- function(region_id, crs, as_file = FALSE, as_union = FALSE, as_convex = FALSE) {
    # generate eco region geometry
    eco_region_geom <- .roi_ecoregion_sf(
        region_id = region_id,
        crs = crs
    )

    # Union
    if (as_union) {
        eco_region_geom <- sf::st_union(eco_region_geom) |>
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
        sf::st_write(eco_region_geom, eco_region_file)

        # Update result variable
        eco_region_geom <- eco_region_file
    }

    # return!
    return(eco_region_geom)
}


#' @export
crop_to_roi <- function(cube, tiles, multicores, output_dir, grid_system = "BDC_MD_V2") {
    purrr::map_chr(tiles, function(tile) {
        tile_bbox <- sits_tiles_to_roi(tile, grid_system = grid_system)
        tile_bbox <- sf::st_as_sfc(tile_bbox)

        tile_cube <- sits_cube_copy(
            cube = cube,
            roi = tile_bbox,
            multicores = multicores,
            output_dir = output_dir
        )

        tile_filename_original <- tile_cube[["file_info"]][[1]][["path"]]
        tile_filename_new <- stringr::str_replace(tile_filename_original,
                                                  "_MOSAIC_",
                                                  paste0("_", tile, "_"))

        # rename
        fs::file_move(tile_filename_original, tile_filename_new)

        # return!
        tile_filename_new
    })
}
