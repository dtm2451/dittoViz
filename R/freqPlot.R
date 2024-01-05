#' Plot discrete observation frequencies per sample and per grouping
#'
#' @inheritParams yPlot
#' @inheritParams barPlot
#' @param var Single string representing the name of a column of \code{data_frame} that contains the discrete data you wish to quantify as frequencies.
#' @param sample.by Single string representing the name of a column of \code{data_frame} that contains an indicator of which sample each observation belongs to.
#'
#' Note that when this is not provided, there will only be one data point per grouping.
#' A warning can be expected then for all \code{plots} options except \code{"jitter"}.
#' @param vars.use String or string vector naming a subset of the values of \code{var}-data which should be shown.
#' If left as \code{NULL}, all values are shown.
#'
#' Hint: use \code{\link{colLevels}} or \code{unique(data_frame[,var])} to assess options.
#'
#' Note: When \code{var.labels.rename} is jointly utilized to update how the \code{var}-values are shown, the \strong{updated} values must be used.
#' @param var.labels.reorder Integer vector. A sequence of numbers, from 1 to the number of distinct \code{var}-value identities, for rearranging the order of facets within the plot space.
#'
#' Method: Make a first plot without this input.
#' Then, treating the top-left-most grouping as index 1, and the bottom-right-most as index n.
#' Values of \code{var.labels.reorder} should be these indices, but in the order that you would like them rearranged to be.
#' @param max.normalize Logical which sets whether the data for each \code{var}-data value (each facet) should be normalized to have the same maximum value.
#'
#' When set to \code{TRUE}, lower frequency \code{var}-values will make use of just as much plot space as higher frequency vars.
#'
#' Note: Similarly equal plot space utilization can be achieved by using \code{split.adjust = list(scales = "free_y")}, and that alternative route retains original values of the data.
#' @param split.nrow,split.ncol Integers which set the dimensions of the facet grid.
#' @param split.adjust A named list which allows extra parameters to be pushed through to the faceting function call.
#' List elements should be valid inputs to the faceting function \code{\link[ggplot2]{facet_wrap}}, e.g. `list(scales = "free_y")`.
#'
#' See \code{\link[ggplot2]{facet_wrap}} for options.
#' @param ylab String, sets the continuous-axis label (=y-axis for box and violin plots, x-axis for ridgeplots).
#' Default = "make" and if left as make, this title will be automatically generated.
#' @param data.out Logical. When set to \code{TRUE}, changes the output, from the plot alone, to a list containing the plot (\code{p}), its underlying data (\code{data}).
#' @return A ggplot plot where frequencies of discrete \code{var}-data per sample, grouped by condition, timepoint, etc., is shown on the y-axis by a violin plot, boxplot, and/or jittered points, or on the x-axis by a ridgeplot with or without jittered points.
#'
#' Alternatively, if \code{data.out = TRUE}, a list containing the plot ("p") and a dataframe of the underlying data ("data").
#'
#' Alternatively, if \code{do.hover = TRUE}, a plotly conversion of the ggplot output in which underlying data can be retrieved upon hovering the cursor over the plot.
#' @details
#' The function creates a dataframe containing counts and percent makeup of \code{var} identities per sample if \code{sample.by} is given, or per group if only \code{group.by} is given.
#' \code{color.by} can optionally be used to add subgroupings to calculations and ultimate plots, or to convey super-groups of \code{group.by} groupings.
#'
#' Typically, \code{var} might target clustering or observation-type annotations, but in truth it can be given any discrete data.
#'
#' If a set of rows to use was indicated with the \code{rows.use} input, only the targeted rows are used for counts and percent makeup calculations.
#' In other words, the \code{row.use} input adjusts the universe that frequencies are calculated within.
#'
#' If a set of \code{var}-values to show is indicated with the \code{vars.use} input, the data.frame is trimmed at the end to include only the corresponding rows.
#' Thus, this input does not affect the universe for frequency calculation.
#'
#' If \code{max.normalized} is set to \code{TRUE}, counts and percent data are transformed to a 0-1 scale, which is one method for making better use of white space for lower frequency \code{var}-values.
#' Alternatively, \code{split.adjust = list(scales = "free_y")} can be used to achieve the same white-space utilization while retaining original data values.
#'
#' Either percent of total (\code{scale = "percent"}), which is the default, or counts (if \code{scale = "count"})
#' data is then (gg)plotted with the data representation types in \code{plots} by utilizing the same machinery as \code{\link{yPlot}}.
#' Faceting by \code{var}-data values is utilized to achieve per \code{var}-value (e.g. cluster) granularity.
#'
#' See below for additional customization options!
#'
#' @section Calculation Details:
#' The function is restricted in that each samples' observations, indicated by the unique values of \code{sample.by}-data, must exist within single \code{group.by} and \code{color.by} groupings.
#' Thus, in order to ensure all valid \code{var}-data composition data points are generated, prior to calculations... \itemize{
#' \item \code{var}-data are ensured to be a factor, which ensures a calculation will be run for every \code{var}-value (a.k.a. cluster)
#' \item \code{group.by}-data and \code{color-by}-data are treated as non-factor data, which ensures that calculations are run only for the groupings that each sample is associated with.
#' }
#'
#' @section Plot Customization:
#' The \code{plots} argument determines the types of \strong{data representation} that will be generated, as well as their order from back to front.
#' Options are \code{"jitter"}, \code{"boxplot"}, \code{"vlnplot"}, and \code{"ridgeplot"}.
#'
#' Each plot type has specific associated options which are controlled by variables that start with their associated string.
#' For example, all jitter adjustments start with "\code{jitter.}", such as \code{jitter.size} and \code{jitter.width}.
#'
#' Inclusion of \code{"ridgeplot"} overrides \code{"boxplot"} and \code{"vlnplot"} presence and changes the plot to be horizontal.
#'
#' Additionally:
#'
#' \itemize{
#' \item \strong{Colors can be adjusted} with \code{color.panel}.
#' \item \strong{Subgroupings:} \code{color.by} can be utilized to split major \code{group.by} groupings into subgroups.
#' When this is done in y-axis plotting, dittoViz automatically ensures the centers of all geoms will align,
#' but users will need to manually adjust \code{jitter.width} to less than 0.5/num_subgroups to avoid overlaps.
#' There are also three inputs through which one can use to control geom-center placement, but the easiest way to do all at once so is to just adjust \code{vlnplot.width}!
#' The other two: \code{boxplot.position.dodge}, and \code{jitter.position.dodge}.
#' \item \strong{Line(s) can be added} at single or multiple value(s) by providing these values to \code{add.line}.
#' Linetype and color are set with \code{line.linetype}, which is "dashed" by default, and \code{line.color}, which is "black" by default.
#' \item \strong{Titles and axes labels} can be adjusted with \code{main}, \code{sub}, \code{xlab}, \code{ylab}, and \code{legend.title} arguments.
#' \item The \strong{legend can be hidden} by setting \code{legend.show = FALSE}.
#' \item \strong{y-axis zoom and tick marks} can be adjusted using \code{min}, \code{max}, and \code{y.breaks}.
#' \item \strong{x-axis labels and groupings} can be changed / reordered using \code{x.labels} and \code{x.reorder}, and rotation of these labels can be turned on/off with \code{x.labels.rotate = TRUE/FALSE}.
#' }
#'
#' @seealso
#' \code{\link{barPlot}} for a data representation that emphasizes total makeup of samples/groups rather than focusing on the \code{var}-data values individually.
#'
#' @examples
#' example("dittoExampleData", echo = FALSE)
#'
#' # There are three main inputs for this function, in addition to 'data_frame'.
#' #  var = typically this will be observation-type annotations or clustering
#' #    This is the set of observations for which we will calculate frequencies
#' #    (per each unique value of this data) within each sample
#' #  sample.by = the name of a column containing sample assignments
#' #    We'll treat all observations with the same value in this column as part
#' #    of the same sample.
#' #  group.by = how to group samples together
#' freqPlot(example_df,
#'     var = "clustering",
#'     sample.by = "sample",
#'     group.by = "category")
#'
#' # 'color.by' can also be set differently from 'group.by' to have the effect
#' #  of highlighting supersets or subgroupings:
#' freqPlot(example_df, "clustering",
#'     group.by = "category",
#'     sample.by = "sample",
#'     color.by = "subcategory")
#'
#' # The var-values shown can be subset with 'vars.use'
#' freqPlot(example_df, "clustering",
#'     group.by = "category", sample.by = "sample", color.by = "subcategory",
#'     vars.use = 1:2)
#'
#' # Particular observations can be ignored from calculations and plotting using
#' #   the 'rows.use' input. Note that doing so adjusts the universe in which
#' #   frequencies are calculated; all frequencies will now be in terms of freq.
#' #   out of the rows.use cells.
#' #   This can be useful for quantifying subtypes within a given supertype,
#' #     rather than per all observations.
#' #   For our example, we'll calculate among clusters 1 and 2, treating clusters 3
#' #     and 4 observations as part of an unwanted other group of data. You'll
#' #     notice that frequencies are higher here than when we used 'vars.use' in
#' #     the previous example.
#' freqPlot(example_df, "clustering",
#'     group.by = "category", sample.by = "sample", color.by = "subcategory",
#'     rows.use = example_df$clustering %in% 1:2)
#'
#' # Lower frequency targets can be expanded to use the entire y-axis by:
#' #  turning on 'max.normalize'-ation:
#' freqPlot(example_df, "clustering",
#'     group.by = "category", sample.by = "sample", color.by = "subcategory",
#'     max.normalize = TRUE)
#' #  or by setting y-scale limits to be set by the contents of facets:
#' freqPlot(example_df, "clustering",
#'     group.by = "category", sample.by = "sample", color.by = "subcategory",
#'     split.adjust = list(scales = "free_y"))
#'
#' # Data representations can also be selected and reordered with the 'plots'
#' #  input, and further adjusted with inputs applying to each representation.
#' freqPlot(example_df,
#'     var = "clustering", sample.by = "sample", group.by = "category",
#'     plots = c("vlnplot", "boxplot", "jitter"),
#'     vlnplot.lineweight = 0.2,
#'     boxplot.fill = FALSE,
#'     boxplot.lineweight = 0.2)
#'
#' # Finally, 'sample.by' is not technically required. When not given, a
#' #  single data point of overall composition stats will be shown for each
#' #  grouping.
#' #  Just note, all data representation other than "jitter" will complain
#' #  due to there only being the one datapoint per group unless you set
#' #  plots to "jitter".
#' freqPlot(example_df,
#'     var = "clustering", group.by = "category", color.by = "subcategory",
#'     plots = "jitter")
#'
#' @author Daniel Bunis
#' @export

freqPlot <- function(
    data_frame,
    var,
    sample.by = NULL,
    group.by,
    color.by = group.by,
    vars.use = NULL,
    scale = c("percent", "count"),
    max.normalize = FALSE,
    plots = c("boxplot","jitter"),
    split.nrow = NULL,
    split.ncol = NULL,
    split.adjust = list(),
    rows.use = NULL,
    data.out = FALSE,
    data.only = FALSE,
    do.hover = FALSE,
    color.panel = dittoColors(),
    colors = seq_along(color.panel),
    y.breaks = NULL,
    min = 0,
    max = NA,
    var.labels.rename = NULL,
    var.labels.reorder = NULL,
    x.labels = NULL,
    x.labels.rotate = TRUE,
    x.reorder = NULL,
    theme = theme_classic(),
    xlab = group.by,
    ylab = "make",
    main = "make",
    sub = NULL,
    jitter.size = 1,
    jitter.width = 0.2,
    jitter.color = "black",
    jitter.position.dodge = boxplot.position.dodge,
    do.raster = FALSE,
    raster.dpi = 300,
    boxplot.width = 0.4,
    boxplot.color = "black",
    boxplot.show.outliers = NA,
    boxplot.outlier.size = 1.5,
    boxplot.fill = TRUE,
    boxplot.position.dodge = vlnplot.width,
    boxplot.lineweight = 1,
    vlnplot.lineweight = 1,
    vlnplot.width = 1,
    vlnplot.scaling = "area",
    vlnplot.quantiles = NULL,
    ridgeplot.lineweight = 1,
    ridgeplot.scale = 1.25,
    ridgeplot.ymax.expansion = NA,
    ridgeplot.shape = c("smooth", "hist"),
    ridgeplot.bins = 30,
    ridgeplot.binwidth = NULL,
    add.line = NULL,
    line.linetype = "dashed",
    line.color = "black",
    legend.show = TRUE,
    legend.title = color.by) {

    scale <- match.arg(scale)
    ridgeplot.shape <- match.arg(ridgeplot.shape)

    # Check that sample definitions are 1:1 with groupings/colorings
    if (!is.null(sample.by)) {
        samps <- ._col(sample.by, data_frame, add.names = FALSE)
        .check_1value_per_group(samps, group.by, "group.by", data_frame)
        if (color.by != group.by) {
            .check_1value_per_group(samps, color.by, "color.by", data_frame)
        }
    }

    # Gather data (use split.by to ensure per- color.by & sample.by calculation)
    data <- .make_composition_summary_df(
        data_frame, var, group.by, split.by = c(sample.by, color.by),
        rows.use, x.reorder, x.labels,
        var.labels.reorder, var.labels.rename, do.hover, max.normalize,
        TRUE, FALSE, TRUE, TRUE)
    if (data.only) {
        return(data)
    }

    # Decide titles
    ylab <- .leave_default_or_null(ylab, scale)
    main <- .leave_default_or_null(main, var)

    # Subset to vars.use
    if (!is.null(vars.use)) {
        data <- data[data$label %in% vars.use,]
    }

    # Adjust BarPlot-ready data for dittoPlot plotter expectation
    if (max.normalize) {
        scale <- paste0(scale, ".norm")
        y.breaks <- NULL
        ylab <- paste("Normalized", ylab)
    }

    #Build Plot
    yPlot(
        data, scale, group.by = "grouping", color.by = color.by,
        shape.by = NULL, split.by = "label", plots = plots,
        var.adjustment = NULL, var.adj.fxn = NULL,
        do.hover = do.hover, color.panel = color.panel, colors = colors,
        theme = theme, main = main, sub = sub, ylab = ylab, y.breaks = y.breaks,
        min = min, max = max, xlab = xlab, x.labels = x.labels,
        x.labels.rotate = x.labels.rotate, x.reorder = x.reorder,
        split.nrow = split.nrow, split.ncol = split.ncol,
        split.adjust = split.adjust, do.raster = do.raster,
        raster.dpi = raster.dpi, jitter.size = jitter.size,
        jitter.color = jitter.color, jitter.width = jitter.width,
        jitter.position.dodge = jitter.position.dodge,
        boxplot.width = boxplot.width,
        boxplot.color = boxplot.color,
        boxplot.show.outliers = boxplot.show.outliers,
        boxplot.outlier.size = boxplot.outlier.size,
        boxplot.fill = boxplot.fill,
        boxplot.position.dodge = boxplot.position.dodge,
        boxplot.lineweight = boxplot.lineweight,
        vlnplot.lineweight = vlnplot.lineweight,
        vlnplot.width = vlnplot.width,
        vlnplot.scaling = vlnplot.scaling,
        vlnplot.quantiles = vlnplot.quantiles,
        ridgeplot.lineweight = ridgeplot.lineweight,
        ridgeplot.scale = ridgeplot.scale,
        ridgeplot.ymax.expansion = ridgeplot.ymax.expansion,
        ridgeplot.shape = ridgeplot.shape,
        ridgeplot.bins = ridgeplot.bins,
        ridgeplot.binwidth = ridgeplot.binwidth,
        add.line = add.line,
        line.linetype = line.linetype,
        line.color = line.color,
        legend.show = legend.show,
        legend.title = legend.title,
        data.out = data.out)
}

.check_1value_per_group <- function(groupings, check, input.name, data_frame) {

    values <- data_frame[,check, drop = TRUE]
    pairs <- paste(groupings, values)
    if (length(unique(groupings)) != length(unique(pairs))) {
        stop("Unable to interpret '", input.name,"' with 'sample.by'. '",
             check, "' data does not map 1 per sample.")
    }
}
