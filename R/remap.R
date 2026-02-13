.build_remap_table <- function(cube, truth_table) {
    cube_labels <- sits::sits_labels(cube)

    cube_labels_remap <- purrr::map_dfr(seq_len(length(cube_labels)), function(idx) {
        cube_label <- cube_labels[idx]

        cube_label_name <- as.character(cube_label)
        cube_label_value <- as.numeric(names(cube_label))

        cube_label_target <- truth_table[truth_table[["source"]] == cube_label_name, "target"][["target"]]

        tibble::tibble(
            source = cube_label_value,
            target = cube_label_target
        )
    })

    if (nrow(cube_labels_remap) != length(cube_labels)) {
        cli::cli_abort("Error: There is an error in the mapping. Please, check it")
    }

    # Return!
    return(cube_labels_remap)
}

#' @title Remap cube labels
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Remaps the labels of a cube.
#'
#' @param cube          A \code{sits} cube.
#' @param output_dir    Character specifying the output directory.
#' @param multicores    Integer specifying the number of cores for parallel
#'                      processing (default: 10).
#' @param memsize       Integer specifying the memory size for parallel
#'                      processing (default: 16).
#' @param mapping_reference  A mapping reference object.
#' @param rules         A rules object.
#'
#' @returns A \code{sits} cube with the remapped labels.
#'
#' @export
cube_remap <- function(cube, output_dir, multicores = 10, memsize = 16, mapping_reference, rules = NULL) {
    # Define output dir
    output_dir <- fs::path(output_dir)

    # Create directory
    fs::dir_create(output_dir)

    # For tile remap
    purrr::map_dfr(seq_len(nrow(cube)), function(idx) {
        # Define tile
        tile <- cube[idx, ]

        # 1. Get files
        file_path <- tile[["file_info"]][[1]][["path"]]

        # Define output file
        file_out <- output_dir / fs::path_file(file_path)

        # Update tile label
        tile[["file_info"]][[1]][["path"]] <- file_out

        # If file exists, return it
        if (fs::file_exists(file_out)) {
            return(tile)
        }

        # Define file rules
        if (is.null(rules)) {
            rules <- .build_remap_table(tile, mapping_reference)
        }

        # Remap!
        file_out <- reclassify_remap_pixels(
            file       = file_path,
            file_out   = file_out,
            rules      = rules,
            multicores = multicores,
            memsize    = memsize,
            output_dir = output_dir
        )

        # Create overviews
        sf::gdal_addo(file_out)

        # Return!
        tile
    })
}
