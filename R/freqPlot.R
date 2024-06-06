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
#' @param add.pvalues NULL (off), "all", or a list of length 2 string vectors which each name a pairwise set of 2 \code{group.by}-values to compare between.
#' Giving "all" will determine, and run comparisons for, all possible pairwise combinations of \code{group.by}-values.
#' @param pvalues.round.digits = 3,
#' @param pvalues.test.adjust named list providing any desired additional inputs for the p-value calculation with \code{\link[stats]{wilcox.test}}.
#' \code{x} and \code{y} inputs are filled internally, but all others can be adjusted if desired.
#' @param pvalues.adjust Logical stating whether to perform multiple hypothesis test correction and plot the corrected p-values.
#' Highly recommended, but if you are performing multiple iterations of this function,
#' proper correction requires running this correction once on all p-values.
#' See \code{\link[stats]{p.adjust}}.
#' @param pvalues.adjust.method String used only when \code{pvalues.adjust = TRUE}, and "fdr" by default.
#' Passed along to the \code{method} input of \code{\link[stats]{p.adjust}}.
#' Any valid option for that input will work.
#' @param pvalues.offset.first,pvalues.offset.between,pvalues.offset.above Numbers which set the heights at which pvalue brackets will be plotted, in fractions of y-data values:
#' \itemize{
#' \item \code{pvalues.offset.first}: the height between the highest data point and the first p-value bracket plotted.
#' \item \code{pvalues.offset.between}: if multiple comparisons are to be run, the additional offset to add between them.
#' \item \code{pvalues.offset.above}: the additional height, above all brackets, to add to the plot in order to ensure p-values are visible. (This is accomplished using a geom_text layer of empty strings, and only required because the ggpubr package does not ensure visibility on its own!)
#' }
#' @param pvalues.do.fc Logical stating whether to calculate medians and the fold-changes between them, alongside of p-value calculations.
#' Only helpful when also using \code{data.out = TRUE}.
#' @param pvalues.fc.pseudocount Number, zero by default. A value to add within fold_change calculations only, to both \code{group.1} and \code{group.2} median frequencies in order to avoid division by zero errors.
#' When needed, we recommend something small relative to the lowest expected cell frequencies of the data, 0.000001 perhaps.
#' Although a relatively small value like this can lead to heavily inflated log fold change values in the extreme cases where \code{group.1} or \code{group.2} frequencies are 0 or near 0, a tiny pseudocount leaves all other fold change values only minimally affected.
#' @param do.hover Logical which sets whether the ggplot output should be converted to a ggplotly object with data about individual bars displayed when you hover your cursor over them.
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
#' @importFrom utils combn
#' @export

freqPlot <- function(
    data_frame,
    var,
    sample.by = NULL,
    group.by,
    color.by = group.by,
    vars.use = NULL,
    add.pvalues = NULL,
    pvalues.round.digits = 4,
    pvalues.test.adjust = list(),
    pvalues.adjust = TRUE,
    pvalues.adjust.method = "fdr",
    pvalues.offset.first = 0.1,
    pvalues.offset.between = 0.2,
    pvalues.offset.above = 0.1,
    pvalues.do.fc = FALSE,
    pvalues.fc.pseudocount = 0,
    scale = c("percent", "count"),
    max.normalize = FALSE,
    plots = c("boxplot","jitter"),
    split.nrow = NULL,
    split.ncol = NULL,
    split.adjust = list(scale = "free_y"),
    rows.use = NULL,
    data.out = FALSE,
    data.only = FALSE,
    do.hover = FALSE,
    hover.round.digits = 5,
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
        var.labels.reorder, var.labels.rename, FALSE, hover.round.digits,
        max.normalize, TRUE, FALSE, TRUE, TRUE)
    if (data.only) {
        return(data)
    }

    # Decide titles
    ylab <- .leave_default_or_null(ylab, scale)
    main <- .leave_default_or_null(main, var)

    # Subset to vars.use
    if (!is.null(vars.use)) {
        data <- data[data$Y %in% vars.use,]
    }

    # Adjust BarPlot-ready data for dittoPlot plotter expectation
    if (max.normalize) {
        scale <- paste0(scale, ".norm")
        y.breaks <- NULL
        ylab <- paste("Normalized", ylab)
    }

    #Build Plot
    yPlot(
        data, scale, group.by = group.by, color.by = color.by,
        shape.by = NULL, split.by = "Y", rows.use = NULL, plots = plots,
        var.adjustment = NULL, var.adj.fxn = NULL,
        do.hover = do.hover, hover.round.digits = hover.round.digits,
        hover.data = unique(c(group.by, "Y", sample.by, color.by, "count", "percent")),
        color.panel = color.panel, colors = colors,
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
        add.pvalues = add.pvalues,
        pvalues.round.digits = pvalues.round.digits,
        pvalues.test.adjust = pvalues.test.adjust,
        pvalues.adjust = pvalues.adjust,
        pvalues.adjust.method = pvalues.adjust.method,
        pvalues.offset.first = pvalues.offset.first,
        pvalues.offset.between = pvalues.offset.between,
        pvalues.offset.above = pvalues.offset.above,
        pvalues.do.fc = pvalues.do.fc,
        pvalues.fc.pseudocount = pvalues.fc.pseudocount,
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

#' Calculate per-sample frequencies of clusters or cell annotations, and compare them across group.
#' @inheritParams freqPlot
#' @param freq.by Single string representing the name of a column of \code{data_frame} that contains the cluster or cell identities to quantify and assess.
#' @param sample.by Single string representing the name of a column of \code{data_frame} that denotes which sample each observation belongs to.
#' @param group.by Single string representing the name of a column of \code{data_frame} that contains group identities.
#' @param group.1,group.2 Single strings naming the 2 sets of \code{group.by}-data to compare.
#' @param freq.targs Single string or a string vector naming which cell/cluster/\code{freq.by}-data identities to target.
#' When not provided, the function will loop through all identities in the \code{freq.by} column.
#' @param wilcox.adjust named list providing any desired additional inputs for the p-value calculation with \code{\link[stats]{wilcox.test}}.
#' \code{x} and \code{y} inputs are filled in by this function, but all others can be adjusted if desired.
#' @param do.adjust Logical stating whether to perform p-value adjustment per multiple hypothesis testing.
#' Highly recommended, but if you are performing multiple iterations of this function,
#' proper correction requires running this correction once on all p-values.
#' See \code{\link[stats]{p.adjust}}.
#' @param p.adjust.method String, "fdr" by default, passed along to the \code{method} input of \code{\link[stats]{p.adjust}}, any valid option for that input will work.
#' @param do.fc Logical stating whether to calculate \code{group.1} and \code{group.2} medians and the fold-changes between them.
#' @param fc.pseudocount Number, zero by default. A value to add within fold_change calculations only, to both \code{group.1} and \code{group.2} median frequencies in order to avoid division by zero errors.
#' When needed, we recommend something small relative to the lowest expected cell frequencies of the data, 0.000001 perhaps.
#' Although a relatively small value like this can lead to heavily inflated log fold change values in the extreme cases where \code{group.1} or \code{group.2} frequencies are 0 or near 0, a tiny pseudocount leaves all other fold change values only minimally affected.
#' @param comp.data.out Logical. When set to \code{TRUE}, changes the output from the stats data.frame alone to a named list containing both the stats ("stats") and the underlying per-sample frequency calculations ("data").
#' @param data.direct Logical, primarily for internal use. Tells the function that the provided \code{data_frame} is already the output of dittoViz's composition calculation, so that does not need to be re-run.
#' @return a data.frame. Or if \code{data.out} was set to \code{TRUE}, a named list containing 2 data.frames: \itemize{
#' \item 'stats' = the standard output
#' \item 'data' = the composition summary statistics are based upon.
#' }
#' @details The function starts by utilizing the same code behind \code{\link{freqPlot}} and \code{\link{barPlot}} composition calculations for
#' \code{freq.by}-identity frequency calculation within \code{sample.by}-samples.
#' Observations are first trimmed based on any given \code{rows.use} selection,
#' then frequencies are calculated and percent normalized,
#' and which \code{sample.by}-samples belong to which \code{group.by}-groups are marked.
#' (Set \code{comp.data.out = TRUE} to output this composition data.frame as well!)
#'
#' Afterwards, it loops through all \code{freq.targs}, building a row of the eventual stats return for each.
#' P values are calculated viz the \code{\link[stats]{wilcox.test}} function.
#'
#' If \code{do.fc} is left as \code{TRUE}, group medians and fold change between them are calculated.
#' Of note, a \code{pseudocount} can be introduced in median fold change calculation to prevent errors from division by zero. The use of a \code{pseudocount} has no effect on p-values.
#'
#' If \code{do.adjust} is left as \code{TRUE}, p-values will be adjusted based on the \code{p.adjust.method} which default to the false discovery rate method.
#'
#' @section The stats data.frame return:
#' Each row holds statistics for an individual comparison.
#' The columns represent:
#' \itemize{
#' \item Y: cell/cluster/\code{freq.by}-data identity
#' \item group1: \code{group.1},
#' (for compatibility with running the function multiple times, each targeting distinct groups, and then concatenating all outputs together!)
#' \item group2: \code{group.2}, "
#' \item max_freq: The maximum frequency of either group, which can be helpful to know for plotting purposes
#' \item p: The p-value associated with comparison of percent frequencies of group.1 samples versus group.2 samples using a Mann Whitney U Test / wilcoxon rank sum test (\code{\link[stats]{wilcox.test}}).
#' \item padj (\code{do.adjust = TRUE}): p-values corrected by the chosen \code{p.adjust.method}, FDR by default, built from running \code{p.adjust(stats$p, method = p.adjust.method)} per all hypotheses tested in this call to the \code{freq_stats} function.
#' \item median_g1 (\code{do.fc = TRUE}): the median frequency within samples from \code{group.1}
#' \item median_g2 (\code{do.fc = TRUE}): the median frequency within samples from \code{group.2}
#' \item median_fold_change (\code{do.fc = TRUE}): \code{(median_g1 + pseudocount) / (median_g2 + pseudocount)}.
#' \item median_log2_fold_change (\code{do.fc = TRUE}): \code{log2(median_fold_change)}
#' \item positive_fc_means_up_in (\code{do.fc = TRUE}): Always \code{group.1}. A minor note to help remember the directionality of these fold changes!
#' }
#' @author Daniel Bunis
#' @export
#' @importFrom stats wilcox.test
#' @importFrom stats p.adjust
#' @importFrom stats median
freq_stats <- function(
    data_frame,
    freq.by,
    sample.by,
    group.by, group.1, group.2,
    freq.targs = NULL,
    rows.use = NULL,
    wilcox.adjust = list(),
    do.adjust = TRUE,
    p.adjust.method = "fdr",
    do.fc = TRUE,
    fc.pseudocount = 0,
    comp.data.out = FALSE,
    data.direct = FALSE
) {

    # Collect stats
    if (data.direct) {
        data <- data_frame
    } else {
        data <- .make_composition_summary_df(
            data_frame, freq.by, group.by, split.by = c(sample.by),
            rows.use, NULL, NULL,
            NULL, NULL, FALSE, 1,
            FALSE, TRUE, FALSE, TRUE, TRUE)
    }

    # Trim to selected targets
    if (!is.null(freq.targs)) {
        data <- data[data$Y %in% freq.targs,]
    }

    # Here, we loop through all the cell_groups being targeted, 1- calculating stats and 2- building a data.frame during each iteration.
    #  The lapply call performs the iteration, and gathers the data.frames output by each iteration into a list.
    #  That list of data.frames created in our lapply is then 'rbind'ed into a single data.frame.
    stats <- list()
    for (cell in unique(data$Y)) {
        data_use <- data[data$Y==cell,]
        g1s <- as.vector(data_use[[group.by]]==group.1)
        g2s <- as.vector(data_use[[group.by]]==group.2)

        if (length(g1s)==0 || length(g2s)==0) {
            warning("No data")
            next
        }

        new <- data.frame(
            Y = cell,
            group1 = group.1,
            group2 = group.2,
            max_freq = max(data_use$percent),
            stringsAsFactors = FALSE
        )

        if (do.fc) {
            new$median_g1 <- median(data_use$percent[g1s], na.rm = TRUE)
            new$median_g2 <- median(data_use$percent[g2s], na.rm = TRUE)
            if (new$median_g2==0 && fc.pseudocount==0) {
                warning("Looks like a pseudocount will be needed to avoid division by zero errors. Try adding 'fc.pseudocount = 0.000001' to your call and see the '?freq_stats' documentation for details.")
            }
            new$median_fold_change <- (new$median_g1 + fc.pseudocount) / (new$median_g2 + fc.pseudocount)
            new$median_log2_fold_change <- log2(new$median_fold_change)
            new$positive_fc_means_up_in <- group.1
        }

        wilcox.adjust_this <- wilcox.adjust
        wilcox.adjust_this$x <- data_use$percent[g1s]
        wilcox.adjust_this$y <- data_use$percent[g2s]
        new$p <- do.call(wilcox.test, wilcox.adjust_this)$p.value

        stats[[length(stats)+1]] <- new
    }
    stats <- do.call(rbind, stats)

    # Apply FDR correction
    if (do.adjust) {
        stats$padj <- p.adjust(stats$p, method = p.adjust.method)
    }

    # Output
    if (comp.data.out) {
        list(stats = stats, data = data)
    } else {
        stats
    }
}
