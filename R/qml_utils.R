#' @title Read QML file
#'
#' @author Felipe Carlos, \email{efelipecarlos@@gmail.com}
#' @author Felipe Carvalho, \email{felipe.carvalho@@inpe.br}
#'
#' @description Reads a QML file and returns a tibble with the pixel values,
#'              colors, and labels.
#'
#' @param qml_file Path to QML file
#'
#' @returns A tibble with the pixel values, colors, and labels.
#'
#' @keywords internal
#' @export
.qml_to_tibble <- function(qml_file) {
    # Reads the ".qml" file
    qml <- xml2::read_xml(qml_file)

    # Finds all "<paletteEntry>" elements inside "<colorPalette>"
    nodes <- xml2::xml_find_all(qml, ".//colorPalette/paletteEntry")
    if (length(nodes) == 0) {
        stop("The QML file does not contain any <paletteEntry> tags.")
    }

    # Extracts the "value", "color" and "label" attributes
    values <- xml2::xml_attr(nodes, "value")
    colors <- xml2::xml_attr(nodes, "color")
    labels <- xml2::xml_attr(nodes, "label")

    rgb_mat <- grDevices::col2rgb(toupper(colors))

    tibble::tibble(
        pixel = as.integer(values),
        r     = as.integer(rgb_mat["red", ]),
        g     = as.integer(rgb_mat["green", ]),
        b     = as.integer(rgb_mat["blue", ]),
        label = labels
    ) |>
    dplyr::arrange(.data[["pixel"]])
}

#' @title Resolve color palette from multiple input formats
#'
#' @param colors A color specification: \code{NULL} (default) generates a random
#'               palette, a path to a \code{.qml} file, a tibble with
#'               \code{label} and \code{color} columns, or a named character
#'               vector.
#' @param classes Character vector of unique class names in the data.
#'
#' @returns A named character vector mapping class names to hex colors.
#' @noRd
.qml_to_palette <- function(colors, classes) {
    # If it is NULL, generate a random palette
    if (is.null(colors)) {
        # Number of classes
        n <- length(classes)

        # Generate a random palette
        palette <- grDevices::hcl.colors(n, palette = "Dynamic")

        # Set the names of the palette
        names(palette) <- classes

        # Return the palette
        return(palette)
    }

    # If it is a file, assume it is a QML file
    if (is.character(colors) && length(colors) == 1 && fs::file_exists(colors)) {
        # Read the QML file
        qml <- .qml_to_tibble(colors)

        # Prepare a palette from the RGB values
        palette <- grDevices::rgb(
            qml[["r"]], qml[["g"]], qml[["b"]], maxColorValue = 255
        )

        # Set the names of the palette
        names(palette) <- qml[["label"]]

        # Return the palette
        return(palette)
    }

    # If it is a tibble with label + color columns
    if (inherits(colors, "data.frame")) {
        # Check if the tibble has the required columns
        if (!all(c("label", "color") %in% names(colors))) {
            # Throw an error if the tibble does not have the required columns
            cli::cli_abort(c(
                "Color tibble must have {.field label} and {.field color} columns.",
                "i" = "Got columns: {.val {names(colors)}}."
            ))
        }

        # Prepare a palette from the color values
        palette <- colors[["color"]]

        # Set the names of the palette
        names(palette) <- colors[["label"]]

        # Return the palette
        return(palette)
    }

    # If it is a named character vector
    if (is.character(colors) && !is.null(names(colors))) {
        # Return the named character vector
        return(colors)
    }

    # Throw an error if the colors format is invalid
    cli::cli_abort(c(
        "Invalid {.arg colors} format.",
        "i" = "Use {.code NULL} (random), a QML file path, a tibble
               with {.field label}/{.field color} columns, or a named
               character vector."
    ))
}
