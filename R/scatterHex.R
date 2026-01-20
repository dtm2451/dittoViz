#' scatter plot where observations are grouped into hexagonal bins and then summarized
#'
#' @inheritParams scatterPlot
#' @param bins Numeric or numeric vector giving the number of hexagonal bins in the x and y directions. Set to 30 by default.
#' @param color.by Single string denoting the name of a column of \code{data_frame} to use, instead of point density, for setting the color of plotted hexagons.
#' Alternatively, a string vector naming multiple such columns of data to plot at once.
#' @param color.method Single string that specifies how \code{color.by} data should be summarized per each hexagonal bin.
#' Options, and the default, depend on whether the \code{color.by}-data is continuous versus discrete:
#'
#' \strong{Continuous}: String naming a function for how target data should be summarized for each bin.
#' Can be any function that inputs (summarizes) a numeric vector and outputs a single numeric value.
#' Default is \code{median}.
#' Other useful options are \code{sum}, \code{mean}, \code{sd}, or \code{max}.
#' You can also use a custom function as long as you give it a name; e.g. first run \code{logsum <- function(x) \{ log(sum(x)) \}} externally, then give \code{color.method = "logsum"}.
#'
#' \strong{Discrete}: A string signifying whether the color should (default) be simply based on the "max" grouping of the bin,
#' based on "prop.<value>" the proportion of a specific value (e.g. "prop.A" or "prop.TRUE"),
#' or based on the "max.prop"ortion of observations belonging to any grouping.
#' @param legend.density.title,legend.color.title Strings which set the title for the legends.
#' @param legend.density.breaks,legend.color.breaks Numeric vector which sets the discrete values to label in the density and color.by legends.
#' @param legend.density.breaks.labels,legend.color.breaks.labels String vector, with same length as \code{legend.*.breaks}, which sets the labels for the tick marks or hex icons of the associated legend.
#' @param min.opacity,max.opacity Scalar between [0,1] which sets the minimum or maximum opacity used for the density legend (when color is used for \code{color.by} data and density is shown via opacity).
#' @param min.density,max.density Number which sets the min/max values used for the density scale.
#' Used no matter whether density is represented through opacity or color.
#' @param min.color,max.color color for the min/max values of the color scale.
#' @param mid.color NULL (default), "ryb", "rwb", "rgb", or a color to use for the midpoint of a three-color color scale.
#' \emph{This parameter acts a switch between using a 2-color scale or a 3-color scale}:\itemize{
#' \item When left NULL, the 2-color scale runs from \code{min.color} to \code{max.color}, using \code{\link[ggplot2]{scale_fill_gradient}}.
#' \item When given a color, the 3-color scale runs from \code{min.color} to \code{mid.color} to \code{max.color}, using \code{\link[ggplot2]{scale_fill_gradientn}}.
#' \item{
#' When given \emph{\code{"ryb"}, \code{"rwb"}, or \code{"rgb"} serves as a \strong{single-point, quick switch to a "standard" 3-color scale}} by also updating the \code{min.color} and \code{max.color}.
#' Doing so sets:\itemize{
#'     \item \code{max.color} to a red,
#'     \item \code{min.color} to a blue,
#'     \item and \code{mid.color} to either a yellow ("r\emph{y}b"), "white" ("r\emph{w}b"), or "gray97" ("r\emph{g}b", gray not green).
#'     \item Actual colors used are inspired by \href{http://www.colorbrewer.org}{ColorBrewer} "RdYlBu" and "RdBu" palettes.
#' }
#' Thus, the 3-color scale runs from a blue to one of a yellow, "white", or "gray97" to a red, using \code{\link[ggplot2]{scale_fill_gradientn}}.
#' }
#' }
#' @param min,max Number which sets the values associated with the minimum or maximum color for \code{color.by} data.
#' @param mid Number which sets the value (approximately, see note below) associated with the \code{mid.color} of the three-color scale.
#' Ignored when \code{mid.color} is left as NULL.
#'
#' Takes precedence over \code{mid.location} if both are provided.
#'
#' Note: When \code{color.by} is not given and point density is plotted, \strong{ignored} if \code{min.density} and \code{max.density} are not also set.
#'
#' Note: When \code{color.by} is given but \code{min} and \code{max} are not, the mid-color may not be exactly at \code{mid} because translation of \code{c(min, mid, max)} into the \code{c(0, mid.location, 1)} form used by \code{\link[ggplot2]{scale_fill_gradientn}} is performed based on the full range of \code{color.by}-data rather than on the range of binned \code{color.method} calculations.
#' @param mid.location Number between 0 and 1 which sets the relative location of the mid-color in the three-color scale.
#' Ignored when \code{mid} is provided.
#' Default is 0.5, the true midpoint between the min and max values.
#'
#' This can be useful for adjusting the midpoint when transformations are applied such that providing the absolute midpoint value is not possible.
#' @param main String, sets the plot title. The default title is either "Density", \code{color.by}, or NULL, depending on the identity of \code{color.by}.
#' To remove, set to \code{NULL}.
#' @param data.out Logical. When set to \code{TRUE}, changes the output from the plot alone to a list containing the plot ("plot"),
#' and data.frame of the underlying data for target observations ("data"),
#' and the ultimately used mapping of columns to given aesthetic sets, because modification of newly made columns is required for many features ("cols_used").
#'
#' @details
#' This function first makes any requested adjustments to data in the given \code{data_frame}, internally only, such as scaling the \code{color.by}-column if \code{color.adjustment} was given \code{"z-score"}.
#'
#' Next, data_frame is then subset to only target rows based on the \code{rows.use} input.
#'
#' Finally, a hex plot is created using this dataframe:
#'
#' If \code{color.by} is not rovided, coloring is based on the density of observations within each hex bin.
#' When \code{color.by} is provided, density is represented through opacity while coloring is based on a summarization, chosen with the \code{color.method} input, of the target \code{color.by} data.
#'
#' If \code{split.by} was used, the plot will be split into a matrix of panels based on the associated groupings.
#'
#' @return A ggplot object where colored hexagonal bins are used to summarize observations in a scatter plot.
#'
#' Alternatively, if \code{data.out=TRUE}, a list containing three slots is output:
#' the plot (named 'plot'),
#' a data.table containing the updated underlying data for target rows (named 'data'),
#' and a list providing mappings of final column names in 'data' to given plot aesthetics (named 'cols_used'), because modification of newly made columns is required for many features.
#'
#' @section Many characteristics of the plot can be adjusted using discrete inputs:
#' \itemize{
#' \item Colors: \code{min.color} and \code{max.color} adjust the colors for continuous data.
#' \item For discrete \code{color.by} plotting with \code{color.method = "max"}, colors are instead adjusted with \code{color.panel} and/or \code{colors} & the labels of the groupings can be changed using \code{rename.color.groups}.
#' \item Titles and axes labels can be adjusted with \code{main}, \code{sub}, \code{xlab}, \code{ylab}, and \code{legend.color.title} and \code{legend.density.title} arguments.
#' \item Legends can also be adjusted in other ways, using variables that all start with "\code{legend.}" for easy tab completion lookup.
#' }
#'
#' @section Additional Features:
#' Other tweaks and features can be added as well.
#' Each is accessible through 'tab' autocompletion starting with "\code{do.}"\code{---} or "\code{add.}"\code{---},
#' and if additional inputs are involved in implementing or tweaking these, the associated inputs will start with the "\code{---.}":
#' \itemize{
#' \item If \code{do.contour} is provided, density gradient contour lines will be overlaid with color and linetype adjustable via \code{contour.color} and \code{contour.linetype}.
#' \item If \code{add.trajectory.by.groups} is provided a list of vectors (each vector being group names from start-group-name to end-group-name), and a column name pointing to the relevant grouping information is provided to \code{trajectory.group.by},
#' then median centers of the groups will be calculated and arrows will be overlayed to show trajectory inference paths.
#' \item If \code{add.trajectory.curves} is provided a list of matrices (each matrix containing x, y coordinates from start to end), paths and arrows will be overlayed to show trajectory inference curves.
#' Arrow size is controlled with the \code{trajectory.arrow.size} input.
#' }
#'
#' @seealso
#' \code{\link{scatterPlot}} for making non-hex-binned scatter plots showing each individual data point.
#' It is often best to investigate your data with both the individual and hex-bin methods, then pick whichever is the best representation for your particular goal.
#'
#' @author Daniel Bunis, Jared Andrews with some code adapted from Giuseppe D'Agostino
#' @examples
#' example("dittoExampleData", echo = FALSE)
#'
#' # The minimal inputs for scatterHex are the 'data_frame', and 2 column names,
#' #   given to 'x.by' and 'y.by', indicating which data to use for the x and y
#' #   axes, respectively.
#' scatterHex(
#'     example_df, x.by = "PC1", y.by = "PC2")
#'
#' # 'color.by' can also be given a column name in order to represent that
#' #   column's data in the color of the hexes.
#' # Note: This capability requires the suggested package 'ggplot.multistats'.
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(
#'         example_df, x.by = "PC1", y.by = "PC2",
#'         color.by = "groups")
#' }
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(
#'         example_df, x.by = "PC1", y.by = "PC2",
#'         color.by = "gene1")
#' }
#'
#' # 'color.method' is then used to adjust how the target data is summarized
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(example_df, x.by = "PC1", y.by = "PC2",
#'         color.by = "groups",
#'         color.method = "max.prop")
#' }
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(example_df, x.by = "PC1", y.by = "PC2",
#'         color.by = "gene1",
#'         color.method = "mean")
#' }
#' # One particularly useful 'color.method' for discrete 'color.by'-data is
#' #   to use 'prop.<value>' to color by the proportion of a particular value
#' #   within each bin:
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(example_df, x.by = "PC1", y.by = "PC2",
#'         color.by = "groups",
#'         color.method = "prop.A")
#' }
#'
#' # 'max.density' adjustment can be extremely useful for saying "I only care for
#' #   opacity to be decreased in regions with too low a number of observations
#' #   to trust."
#' # (A good value for this in "real" data might be 10 or 50 or higher, but for
#' #   our sparse example data, we'll use just 2.)
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(
#'         example_df, x.by = "PC1", y.by = "PC2",
#'         color.by = "gene1", bins = 10,
#'         sub = "Default density scale")
#' }
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(
#'         example_df, x.by = "PC1", y.by = "PC2",
#'         color.by = "gene1", bins = 10,
#'         sub = "Density capped low for ignoring sparse regions",
#'         max.density = 2)
#' }
#'
#' # Additionally, like with other dittoViz functions:
#' # - intuitive inputs can be used to modify the look of your plot
#' # - 'data.out = TRUE' can be used to have not just the plot returned, but also
#' #   the underlying data
#' # - the 'split.by' input allows plotting to be "split" or faceted based on a
#' #   groupings of a discrete data column
#' # - the 'rows.use' input can be used to plot from only to certain observations
#' # - 'color.by' can be given names of multiple columns containing continuous data
#' #   to have results of each plotted side-by-side.  This can be combined with
#' #   up to one 'split.by' variable with 'multivar.split.dir' then deciding which
#' #   faceting direction is used for the 'color.by' identities.  E.g. 'color.by =
#' #   c("gene1", "gene2"), split.by = "groups", multivar.split.dir = "row"'
#' # - many extra features are available via inputs starting with 'do.FEATURE*'
#' #   or 'add.FEATURE*', with tweaks for those features having inputs starting
#' #   with  with 'FEATURE*'. E.g. 'do.label = TRUE' and 'labels.repel = FALSE'
#'
#' @export
scatterHex <- function(
        data_frame,
        x.by,
        y.by,
        color.by = NULL,
        bins = 30,
        color.method = NULL,
        split.by = NULL,
        rows.use = NULL,
        color.panel = dittoColors(),
        colors = seq_along(color.panel),
        x.adjustment = NULL,
        y.adjustment = NULL,
        color.adjustment = NULL,
        x.adj.fxn = NULL,
        y.adj.fxn = NULL,
        color.adj.fxn = NULL,
        multivar.split.dir = c("col", "row"),
        split.nrow = NULL,
        split.ncol = NULL,
        split.adjust = list(),
        min.density = NA,
        max.density = NA,
        min.color = "#F0E442",
        mid.color = NULL,
        max.color = "#0072B2",
        min.opacity = 0.2,
        max.opacity = 1,
        min = NA,
        mid = NA,
        max = NA,
        mid.location = 0.5,
        rename.color.groups = NULL,
        xlab = x.by,
        ylab = y.by,
        main = "make",
        sub = NULL,
        theme = theme_bw(),
        do.contour = FALSE,
        contour.color = "black",
        contour.linetype = 1,
        do.ellipse = FALSE,
        do.label = FALSE,
        labels.size = 5,
        labels.highlight = TRUE,
        labels.use.numbers = FALSE,
        labels.numbers.spacer = ": ",
        labels.repel = TRUE,
        labels.split.by = split.by,
        labels.repel.adjust = list(),
        add.trajectory.by.groups = NULL,
        add.trajectory.curves = NULL,
        trajectory.group.by,
        trajectory.arrow.size = 0.15,
        add.xline = NULL,
        xline.linetype = "dashed",
        xline.color = "black",
        xline.linewidth = 0.5,
        xline.opacity = 1,
        add.yline = NULL,
        yline.linetype = "dashed",
        yline.color = "black",
        yline.linewidth = 0.5,
        yline.opacity = 1,
        add.abline = NULL,
        abline.slope = 1,
        abline.linetype = "solid",
        abline.color = "black",
        abline.linewidth = 0.5,
        abline.opacity = 1,
        legend.show = TRUE,
        legend.color.title = "make",
        legend.color.breaks = waiver(),
        legend.color.breaks.labels = waiver(),
        legend.density.title = "Observations",
        legend.density.breaks = waiver(),
        legend.density.breaks.labels = waiver(),
        show.grid.lines = TRUE,
        data.out = FALSE) {

    # Standardize rows.use vector
    rows.use <- .which_rows(rows.use, data_frame)
    multivar.split.dir <- match.arg(multivar.split.dir)

    ### Make dataframe edits
    edit_outs <- .data_adjust_scatter(
        data_frame, x.by, y.by, color.by, NA, split.by,
        x.adjustment, y.adjustment, color.adjustment,
        x.adj.fxn, y.adj.fxn, color.adj.fxn,
        rename.color.groups, NULL,
        multivar.split.dir, rows.use, FALSE, NULL
    )
    data <- edit_outs$data_use
    cols_use <- edit_outs$cols_use

    # Parse coloring methods
    color_by_var <- FALSE
    discrete_disp <- FALSE
    discrete_data <- FALSE

    if (!is.null(cols_use$color.by)) {
        color_by_var <- TRUE

        if (!is.numeric(data[,cols_use$color.by])) {
            discrete_data <- TRUE

            if (!any(c("max.prop", paste0("prop.", unique(data[,cols_use$color.by]))) %in% color.method)) {
                discrete_disp <- TRUE
            }
        }

        if (is.null(color.method)) {
            color.method <- ifelse(discrete_data, "max", "median")
        }

        .check_color.method(color.method, discrete_disp)
    }

    # Set titles if "make"
    main <- .leave_default_or_null(
        main,
        default =
            if (!color_by_var) {
                "Density"
            } else if (length(color.by)==1) {
                color.by
            } else {
                NULL
            }
    )
    legend.color.title <- .leave_default_or_null(
        legend.color.title,
        default = ifelse(
            length(color.by)==1,
            paste(color.by, color.method, sep = ",\n"),
            color.method),
        null.if = is.null(color.by)
    )

    # Make the plot
    p <- .scatter_hex(
        data, cols_use$x.by, cols_use$y.by, cols_use$color.by,
        bins, color_by_var, discrete_disp, color.method, color.panel, colors,
        min.density, max.density, min.color, mid.color, max.color,
        min.opacity, max.opacity, min, mid, max, mid.location,
        xlab, ylab, main, sub, theme, legend.show,
        legend.color.title, legend.color.breaks, legend.color.breaks.labels,
        legend.density.title, legend.density.breaks, legend.density.breaks.labels,
        show.grid.lines)

    ### Add extra features
    if (!is.null(cols_use$split.by)) {
        p <- .add_splitting(
            p, cols_use$split.by, split.nrow, split.ncol, split.adjust)
    }

    # Get number of panels so that replicates of aesthetics can be generated if supplied for each line.
    pp <- ggplot_build(p)
    num.panels <- length(levels(pp$data[[1]]$PANEL))

    if (!is.null(add.xline)) {
        p <- .add_xline(p, add.xline, xline.linetype, xline.color, xline.linewidth, xline.opacity, num.panels)
    }

    if (!is.null(add.yline)) {
        p <- .add_yline(p, add.yline, yline.linetype, yline.color, yline.linewidth, yline.opacity, num.panels)
    }

    if (!is.null(add.abline)) {
        p <- .add_abline(p, add.abline, abline.slope, abline.linetype, abline.color, abline.linewidth, abline.opacity, num.panels)
    }

    if (do.contour) {
        p <- .add_contours(p, data, cols_use$x.by, cols_use$y.by, contour.color,  contour.linetype)
    }

    p <- .add_letters_ellipses_labels_if_discrete(
        p, data, cols_use$x.by, cols_use$y.by, cols_use$color.by,
        FALSE, do.ellipse, do.label,
        labels.highlight, labels.size, labels.repel, labels.split.by,
        labels.repel.adjust,
        labels.use.numbers, labels.numbers.spacer, legend.color.title)

    if (is.list(add.trajectory.by.groups)) {
        p <- .add_trajectories_by_groups(
            p, data_frame, cols_use$x.by, cols_use$y.by, add.trajectory.by.groups,
            trajectory.group.by, trajectory.arrow.size)
    }

    if (is.list(add.trajectory.curves)) {
        p <- .add_trajectory_curves(
            p, add.trajectory.curves, trajectory.arrow.size)
    }

    ### RETURN the PLOT ###
    if (data.out) {
        list(plot = p, data = data, cols_used = edit_outs$cols_use)
    } else{
        p
    }
}

.scatter_hex <- function(
        data,
        x.by,
        y.by,
        color.by,
        bins,
        color_by_var,
        discrete,
        color.method,
        color.panel,
        colors,
        min.density,
        max.density,
        min.color,
        mid.color,
        max.color,
        min.opacity,
        max.opacity,
        min,
        mid,
        max,
        mid.location,
        xlab,
        ylab,
        main,
        sub,
        theme,
        legend.show,
        legend.color.title,
        legend.color.breaks,
        legend.color.breaks.labels,
        legend.density.title,
        legend.density.breaks,
        legend.density.breaks.labels,
        show.grid.lines
) {

    if (!show.grid.lines) {
        theme <- theme + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    }

    # Predefined color schemes
    if (!identical(mid.color, NULL)) {
        if (mid.color == "ryb") {
            min.color <- "#4575B4"
            mid.color <- "#FFFFBF"
            max.color <- "#D73027"
        }
        if (mid.color == "rgb") {
            min.color <- "#2166AC"
            mid.color <- "gray97"
            max.color <- "#B2182B"
        }
        if (mid.color == "rwb") {
            min.color <- "#2166AC"
            mid.color <- "white"
            max.color <- "#B2182B"
        }
    }

    ### Set up plotting
    p <- ggplot() + ylab(ylab) + xlab(xlab) + ggtitle(main,sub) + theme

    ### Determine how to add data while adding proper theming
    aes.use <- aes(x = .data[[x.by]], y = .data[[y.by]])
    geom.args <- list(
        data = data, bins = bins, na.rm = TRUE)

    if (!color_by_var) {
        ## Set color scale based on density for stat_bin_hex
        if (identical(mid.color, NULL)) {
            p <- p + scale_fill_gradient(
                name = legend.density.title,
                low= min.color,
                high = max.color,
                limits = c(min.density, max.density),
                breaks = legend.density.breaks,
                labels = legend.density.breaks.labels)
        } else {
            if (!identical(mid, NA)) {
                # Substitute 'mid.location' with what 'mid' represents.
                if (identical(min.density, NA) || identical(max.density, NA)) {
                    warning("Ignoring 'mid' and defaulting to 'mid.location = 0.5': 'min.density' and 'max.density' must also be provided in order to use the 'mid' parameter with no 'color.by' parameter set.")
                    mid.location <- 0.5
                } else if (mid <= min.density || mid >= max.density) {
                    warning("Ignoring 'mid' and defaulting to 'mid.location = 0.5': 'mid' must be between 'min.density' and 'max.density'")
                    mid.location <- 0.5
                } else {
                    mid.location <- (mid - min.density) / (max.density - min.density)
                }
            } else if (mid.location <= 0 || mid.location >= 1) {
                warning("Ignoring given 'mid.location' and defaulting to '0.5': 'mid.location' must be greater than 0 and less than 1.")
                mid.location <- 0.5
            }

            p <- p + scale_fill_gradientn(
                name = legend.density.title,
                values = c(0, mid.location, 1),
                colors = c(min.color, mid.color, max.color),
                limits = c(min.density, max.density),
                breaks = legend.density.breaks,
                labels = legend.density.breaks.labels)
        }

    } else {
        ## Setup for ggplot.multistats::stat_summaries_hex
        .error_if_no_ggplot.multistats()

        # Set alpha scale based on density
        p <- p + scale_alpha_continuous(
            name = legend.density.title,
            range = c(min.opacity, max.opacity),
            limits = c(min.density, max.density),
            breaks = legend.density.breaks,
            labels = legend.density.breaks.labels)

        # Prep aesthetics
        aes.use <- modifyList(aes.use, aes(
            z = .data[[color.by]],
            fill = after_stat(.data$fxn_c),
            alpha = after_stat(.data$fxn_d),
            # Fix for when color is a factor
            group = 1))

        # Determine how 'c' and 'd' should be calculated &
        # set fill based on color.method
        if (discrete) {

            geom.args$funs <- c(
                fxn_c = if (color.method == "max") {
                    function(x) names(which.max(table(x)))
                }, fxn_d = length)

            p <- p + scale_fill_manual(
                name = legend.color.title,
                values = color.panel[colors])

        } else {

            geom.args$funs <- c(
                fxn_c = if (color.method == "max.prop") {
                    function(x) max(table(x)/length(x))
                } else if (grepl("^prop.", color.method)) {
                    function(x) {
                        lev <- substr(color.method, 6, nchar(color.method))
                        sum(x==lev)/length(x)
                    }
                } else {
                    color.method
                }, fxn_d = length)

            # Using do.call here with color.args list does not work, not entirely sure why
            if (identical(mid.color, NULL)) {
                p <- p + scale_fill_gradient(
                    name = legend.color.title,
                    low = min.color,
                    high = max.color,
                    limits = c(min, max),
                    breaks = legend.color.breaks,
                    labels = legend.color.breaks.labels)
            } else {
                if (is.numeric(data[[color.by]]) && !identical(mid, NA)) {
                    max.calc <- ifelse(identical(max, NA), max(data[[color.by]]), max)
                    min.calc <- ifelse(identical(min, NA), min(data[[color.by]]), min)
                    mid.location <- (mid - min.calc) / (max.calc - min.calc)
                    if (identical(min, NA) || identical(max, NA)) {
                        if (mid.location <= 0 || mid.location >= 1) {
                            # This warning adds hint of setting min/max if color.method brings color range outside of the original range of color.by data
                            warning("Ignoring 'mid' and defaulting to 'mid.location = 0.5': 'mid' must be within the range of 'color.by'-data unless 'min' and 'max' are also given.")
                            mid.location <- 0.5
                        } else {
                            message("Approximation of 'mid' value into the color scale \"midpoint\" is not gauranteed to be exact without also setting 'min' and 'max'.")
                        }
                    } else if (mid.location <= 0 || mid.location >= 1) {
                        warning("Ignoring 'mid' and defaulting to 'mid.location = 0.5': 'mid' must be greater than 'min' and less than 'max'.")
                        mid.location <- 0.5
                    }
                }
                if (mid.location <= 0 || mid.location >= 1) {
                    warning("Ignoring given 'mid.location' and defaulting to '0.5': 'mid.location' must be greater than 0 and less than 1.")
                    mid.location <- 0.5
                }
                p <- p + scale_fill_gradientn(
                    name = legend.color.title,
                    values = c(0, mid.location, 1),
                    colors = c(min.color, mid.color, max.color),
                    limits = c(min, max),
                    breaks = legend.color.breaks,
                    labels = legend.color.breaks.labels)
            }
        }
    }

    ### Add data
    geom.args$mapping <- aes.use
    if (!is.null(color.by)) {
        p <- p + do.call(ggplot.multistats::stat_summaries_hex, geom.args)
    } else {
        p <- p + do.call(stat_bin_hex, geom.args)
    }

    if (!legend.show) {
        p <- .remove_legend(p)
    }

    p
}

.check_color.method <- function(color.method, discrete) {
    valid <- FALSE
    if (discrete) {
        valid <- color.method == "max"
    } else {
        # Discrete at this point is based on the style of data displayed
        # Thus originally discrete data with color.method = 'max.prop' gets 'discrete = FALSE' to this function
        valid <- color.method == "max.prop" || grepl("^prop.", color.method) || exists(color.method, mode='function')
    }

    if (!valid) {
        stop("'color.method' not valid. Must be \"max\", \"max.prop\", or \"prop.<data-level>\" (discrete data) or the name of a function (continuous data)")
    }
}
