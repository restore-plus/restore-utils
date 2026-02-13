#' @title Get the STAC address
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Gets the STAC address from the environment variables.
#' 
#' @returns Character representing the STAC address.
#' 
#' @keywords internal
.cube_stac_address <- function() {
    Sys.getenv("RESTORE_PLUS_STAC_ADDRESS")
}

#' @title Generate indices for a BDC cube
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Generates indices for a BDC cube.
#' 
#' @returns A sits cube with the generated indices.
#' 
#' @keywords internal
#' @export
cube_generate_indices_bdc <- function(cube, output_dir, multicores, memsize) {
    # Generate NDVI
    cube <- sits::sits_apply(
        data       = cube,
        NDVI       = (NIR08 - RED) / (NIR08 + RED),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate EVI (https://www.usgs.gov/landsat-missions/landsat-enhanced-vegetation-index)
    cube <- sits::sits_apply(
        data       = cube,
        EVI        = 2.5 * ((NIR08 - RED) / (NIR08 + 6 * RED - 7.5 * BLUE + 1)),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate MNDWI
    cube <- sits::sits_apply(
        data       = cube,
        MNDWI      = (GREEN - SWIR16) / (GREEN + SWIR16),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate NBR (https://www.usgs.gov/landsat-missions/landsat-normalized-burn-ratio)
    cube <- sits::sits_apply(data       = cube,
                       NBR        = (NIR08 - SWIR22) / (NIR08 + SWIR22),
                       output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    return(cube)
}

#' @title Generate indices for a GLAD cube
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Generates indices for a GLAD cube.
#' 
#' @returns A sits cube with the generated indices.
#' 
#' @export
cube_generate_indices_glad <- function(cube, output_dir, multicores, memsize) {
    # Generate NDVI
    cube <- sits::sits_apply(
        data       = cube,
        NDVI       = (NIR - RED) / (NIR + RED),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate EVI (https://www.usgs.gov/landsat-missions/landsat-enhanced-vegetation-index)
    cube <- sits::sits_apply(
        data       = cube,
        EVI        = 2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1)),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # Generate MNDWI
    cube <- sits::sits_apply(
        data       = cube,
        MNDWI      = (GREEN - SWIR1) / (GREEN + SWIR1),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    # 1.6. Generate NBR (https://www.usgs.gov/landsat-missions/landsat-normalized-burn-ratio)
    cube <- sits::sits_apply(
        data       = cube,
        NBR        = (NIR - SWIR2) / (NIR + SWIR2),
        output_dir = output_dir,
        multicores = multicores,
        memsize    = memsize,
        progress   = TRUE
    )

    return(cube)
}

#' @title Load a cube (patching STAC address)
#' 
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#' 
#' @description Loads a cube and patches the STAC address if the STAC 
#'              address (\code{RESTORE_PLUS_STAC_ADDRESS}) environment 
#'              variable is set.
#' 
#' @param ... Arguments to pass to \code{\link[sits]{sits_cube}}.
#' 
#' @returns A sits cube.
#' 
#' @keywords internal
#' @export
cube_load <- function(...) {
    # Load the cube
    cube <- sits::sits_cube(...)

    # Get the STAC address
    stac_address <- .cube_stac_address()

    # Check if the STAC address is set
    has_stac_address <- stringr::str_length(stac_address) > 0

    # If the STAC address is set, patch the cube
    if (has_stac_address) {
        # Patch cube file info
        cube <- slider::slide_dfr(cube, function(cube_row) {
            cube_row[["file_info"]][[1]] <- cube_row[["file_info"]][[1]] |>
                dplyr::rowwise() |>
                dplyr::mutate(url_hostname = httr::parse_url(stringr::str_replace(.data[["path"]], "/vsicurl/", ""))[["hostname"]]) |>
                dplyr::mutate(path = stringr::str_replace(.data[["path"]], .data[["url_hostname"]], !!stac_address)) |>
                dplyr::mutate(
                    path = stringr::str_replace(
                        .data[["path"]],
                        "/vsicurl/",
                        "/vsicurl?unsafessl=yes&max_retry=10&url="
                    )
                ) |>
                dplyr::select(-dplyr::any_of("url_hostname")) |>
                dplyr::as_tibble()

            # Return the patched cube row
            cube_row
        })
    }

    # Return the patched cube
    return(cube)
}
