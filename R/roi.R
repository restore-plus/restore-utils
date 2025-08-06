

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
