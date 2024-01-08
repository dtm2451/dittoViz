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
#' You can also use a custom function as long as you give it a name; e.g. first run \code{logsum <- function(x) \{ log(sum(x)) \}} externally, then give \code{color.method = "logsum"}
#'
#' \strong{Discrete}: A string signifying whether the color should (default) be simply based on the "max" grouping of the bin,
#' or based on the "max.prop"ortion of observations belonging to any grouping.
#' @param legend.density.title,legend.color.title Strings which set the title for the legends.
#' @param legend.density.breaks,legend.color.breaks Numeric vector which sets the discrete values to label in the density and color.by legends.
#' @param legend.density.breaks.labels,legend.color.breaks.labels String vector, with same length as \code{legend.*.breaks}, which sets the labels for the tick marks or hex icons of the associated legend.
#' @param min.opacity,max.opacity Scalar between [0,1] which sets the minimum or maximum opacity used for the density legend (when color is used for \code{color.by} data and density is shown via opacity).
#' @param min.density,max.density Number which sets the min/max values used for the density scale.
#' Used no matter whether density is represented through opacity or color.
#' @param min.color,max.color color for the min/max values of the color scale.
#' @param min,max Number which sets the values associated with the minimum or maximum color for \code{color.by} data.
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
#' @author Daniel Bunis with some code adapted from Giuseppe D'Agostino
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
#' # Data can be "split" or faceted by a discrete variable as well.
#' scatterHex(example_df, x.by = "PC1", y.by = "PC2",
#'     split.by = "timepoint") # single split.by element
#' scatterHex(example_df, x.by = "PC1", y.by = "PC2",
#'     split.by = c("groups","SNP")) # row and col split.by elements
#'
#' # Modify the look with intuitive inputs
#' scatterHex(example_df, x.by = "PC1", y.by = "PC2",
#'     show.grid.lines = FALSE,
#'     ylab = NULL, xlab = "PC2 by PC1",
#'     main = "Plot Title",
#'     sub = "subtitle",
#'     legend.density.title = "Items")
#' # 'max.density' is one of these intuitively named inputs that can be
#' #   extremely useful for saying "I only can for opacity to be decreased
#' #   in regions with exceptionally low observation numbers."
#' # (A good value for this in "real" data might be 10 or 50 or higher, but for
#' #   our sparse example data, we need to do a lot to show this off at all!)
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
#' # You can restrict to only certain data points using the 'rows.use' input.
#' #   The input can be given rownames, indexes, or a logical vector
#' scatterHex(example_df, x.by = "PC1", y.by = "PC2",
#'     sub = "show only first 40 observations, by index",
#'     rows.use = 1:40)
#' scatterHex(example_df, x.by = "PC1", y.by = "PC2",
#'     sub = "show only 3 obs, by name (plotting gets a bit wonky for few points)",
#'     rows.use = c("obs1", "obs2", "obs25"))
#' scatterHex(example_df, x.by = "PC1", y.by = "PC2",
#'     sub = "show groups A,B,D only, by logical",
#'     rows.use = example_df$groups!="C")
#'
#' # Many extra features are easy to add as well:
#' #   Each is started via an input starting with 'do.FEATURE*' or 'add.FEATURE*'
#' #   And when tweaks for that feature are possible, those inputs will start be
#' #   named starting with 'FEATURE*'. For example, color.by groups can be labeled
#' #   with 'do.label = TRUE' and the tweaks for this feature are given with inputs
#' #   'labels.size', 'labels.highlight', and 'labels.repel':
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(example_df, x.by = "PC1", y.by = "PC2", color.by = "groups",
#'         sub = "default labeling",
#'         do.label = TRUE)          # Turns on the labeling feature
#' }
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(example_df, x.by = "PC1", y.by = "PC2", color.by = "groups",
#'         sub = "tweaked labeling",
#'         do.label = TRUE,          # Turns on the labeling feature
#'         labels.size = 8,          # Adjust the text size of labels
#'         labels.highlight = FALSE, # Removes white background behind labels
#'         labels.repel = FALSE)     # Turns off anti-overlap location adjustments
#' }
#'
#' # Faceting can also be used to show multiple continuous variables side-by-side
#' #   by giving a vector of column names to 'color.by'.
#' #   This can also be combined with 1 'split.by' variable, with direction then
#' #   controlled via 'multivar.split.dir':
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(example_df, x.by = "PC1", y.by = "PC2", bins = 10,
#'         color.by = c("gene1", "gene2"))
#' }
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(example_df, x.by = "PC1", y.by = "PC2", bins = 10,
#'         color.by = c("gene1", "gene2"),
#'         split.by = "groups")
#' }
#' if (requireNamespace("ggplot.multistats", quietly = TRUE)) {
#'     scatterHex(example_df, x.by = "PC1", y.by = "PC2", bins = 10,
#'         color.by = c("gene1", "gene2"),
#'         split.by = "groups",
#'         multivar.split.dir = "row")
#' }
#'
#' # Sometimes, it can be useful for external editing or troubleshooting purposes
#' #   to see the underlying data that was directly used for plotting.
#' # 'data.out = TRUE' can be provided in order to obtain not just plot ("plot"),
#' #   but also the "data" and "cols_used" returned as a list.
#' out <- scatterHex(example_df, x.by = "PC1", y.by = "PC2",
#'     rows.use = 1:40,
#'     data.out = TRUE)
#' out$plot
#' summary(out$data)
#' out$cols_use
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
        max.color = "#0072B2",
        min.opacity = 0.2,
        max.opacity = 1,
        min = NA,
        max = NA,
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
        labels.repel = TRUE,
        labels.split.by = split.by,
        labels.repel.adjust = list(),
        add.trajectory.by.groups = NULL,
        add.trajectory.curves = NULL,
        trajectory.group.by,
        trajectory.arrow.size = 0.15,
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

            if (!("max.prop" %in% color.method)) {
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
        min.density, max.density, min.color, max.color,
        min.opacity, max.opacity, min, max,
        xlab, ylab, main, sub, theme, legend.show,
        legend.color.title, legend.color.breaks, legend.color.breaks.labels,
        legend.density.title, legend.density.breaks, legend.density.breaks.labels,
        show.grid.lines)

    ### Add extra features
    if (!is.null(cols_use$split.by)) {
        p <- .add_splitting(
            p, cols_use$split.by, split.nrow, split.ncol, split.adjust)
    }

    if (do.contour) {
        p <- .add_contours(p, data, cols_use$x.by, cols_use$y.by, contour.color,  contour.linetype)
    }

    p <- .add_letters_ellipses_labels_if_discrete(
        p, data, cols_use$x.by, cols_use$y.by, cols_use$color.by,
        FALSE, do.ellipse, do.label,
        labels.highlight, labels.size, labels.repel, labels.split.by,
        labels.repel.adjust)

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
        max.color,
        min.opacity,
        max.opacity,
        min,
        max,
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

    ### Set up plotting
    p <- ggplot() + ylab(ylab) + xlab(xlab) + ggtitle(main,sub) + theme

    ### Determine how to add data while adding proper theming
    aes.args <- list(x = x.by, y = y.by)
    geom.args <- list(
        data = data, bins = bins, na.rm = TRUE)

    if (!color_by_var) {
        ## Set color scale based on density for stat_bin_hex
        p <- p + scale_fill_gradient(
            name = legend.density.title,
            low= min.color,
            high = max.color,
            limits = c(min.density, max.density),
            breaks = legend.density.breaks,
            labels = legend.density.breaks.labels)

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
        aes.args$z <- color.by
        aes.args$fill <- "stat(c)"
        aes.args$alpha <- "stat(d)"
        # Fix for when color is a factor
        aes.args$group <- 1

        # Determine how 'c' and 'd' should be calculated &
        # set fill based on color.method
        if (discrete) {

            geom.args$funs <- c(
                c = if (color.method == "max") {
                    function(x) names(which.max(table(x)))
                }, d = length)

            p <- p + scale_fill_manual(
                name = legend.color.title,
                values = color.panel[colors])

        } else {

            geom.args$funs <- c(
                c = if (color.method == "max.prop") {
                    function(x) max(table(x)/length(x))
                } else {
                    color.method
                }, d = length)

            p <- p + scale_fill_gradient(
                name = legend.color.title,
                low= min.color,
                high = max.color,
                limits = c(min,max),
                breaks = legend.color.breaks,
                labels = legend.color.breaks.labels)

        }
    }

    ### Add data
    geom.args$mapping <- do.call(aes_string, aes.args)
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
        valid <- color.method == "max.prop" || exists(color.method, mode='function')
    }

    if (!valid) {
        stop("'color.method' not valid. Must be \"max\" or \"max.prop\" (discrete data) or the name of a function (continuous data)")
    }
}
