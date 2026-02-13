#' @title Crop a cube to a set of tiles
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Crops a cube to a set of tiles.
#' 
#' @param cube A \code{sits} cube.
#' @param tiles A character vector of tile names.
#' @param output_dir Character path for intermediate files.
#' @param multicores Integer specifying the number of cores for parallel
#'                   processing (default: 10).
#' @param grid_system Character specifying the grid system to use.
#'                     (default: "BDC_MD_V2").
#'
#' @returns A character vector with the paths to the cropped tile files.
#'
#' @export
crop_to_tiles <- function(cube, tiles, output_dir, multicores = 10, grid_system = "BDC_MD_V2") {
    purrr::map_chr(tiles, function(tile) {
        tile_bbox <- sits::sits_tiles_to_roi(tile, grid_system = grid_system)
        tile_bbox <- sf::st_as_sfc(tile_bbox)

        tile_cube <- sits::sits_cube_copy(
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
