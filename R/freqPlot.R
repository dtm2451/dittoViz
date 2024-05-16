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
#' @export

freqPlot <- function(
    data_frame,
    var,
    sample.by = NULL,
    group.by,
    color.by = group.by,
    vars.use = NULL,
    add.pvalues = NULL,
    pvalues.pseudocount = 0,
    pvalues.adjust.method = "fdr",
    pvalues.adjust.n = NULL,
    pvalues.offset.first = 0.1,
    pvalues.offset.between = 0.2,
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
        data <- data[data$label %in% vars.use,]
    }

    # Adjust BarPlot-ready data for dittoPlot plotter expectation
    if (max.normalize) {
        scale <- paste0(scale, ".norm")
        y.breaks <- NULL
        ylab <- paste("Normalized", ylab)
    }

    #Build Plot
    yplot_out <- yPlot(
        data, scale, group.by = "grouping", color.by = color.by,
        shape.by = NULL, split.by = "Y", rows.use = NULL, plots = plots,
        var.adjustment = NULL, var.adj.fxn = NULL,
        do.hover = do.hover, hover.round.digits = hover.round.digits,
        hover.data = unique(c("grouping", "Y", sample.by, color.by, "count", "percent")),
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
        add.line = add.line,
        line.linetype = line.linetype,
        line.color = line.color,
        legend.show = legend.show,
        legend.title = legend.title,
        data.out = TRUE)
    p <- yplot_out$p
    cols_use <- yplot_out$cols_use

    ### Addition: p values
    stats_calcd <- FALSE
    if (!identical(add.pvalues, NULL) && is.list(add.pvalues)) {
        # ToDo error if no ggpubr
        stats <- list()
        y_offset <- 1 + pvalues.offset.first
        for (ind in seq_along(add.pvalues)) {
            # ToDo: Error if bad setup
            g1 <- add.pvalues[[ind]][1]
            g2 <- add.pvalues[[ind]][2]
            new_stats <- freq_stats(
                data_frame = data,
                data_direct = TRUE,
                sample.by = sample.by,
                cell.by = "Y",
                group.by = group.by,
                group.1 = g1,
                group.2 = g2,
                cell.targs = NULL,
                rows.use = NULL,
                pseudocount = pvalues.pseudocount,
                p.adjust.method = pvalues.adjust.method,
                n.for.correction = pvalues.adjust.n,
                n.multiply = length(add.pvalues),
                data.out = FALSE)
            # names(new_stats)[1] <- var
            new_stats[[group.by]] <- g2
            stats[[new_stats$comparison[1]]] <- new_stats

            p <- p + ggpubr::stat_pvalue_manual(
                data = new_stats, label = "padj",
                y.position = new_stats$maxy*y_offset
            ) + geom_text(data = new_stats, aes(
                x = .data[[group.by]],
                y = .data$maxy*(y_offset)),
                label = ""
            )
            y_offset <- y_offset + pvalues.offset.between
        }
        stats_calcd <- TRUE
    }

    # DONE. Return the plot +/- data
    if (data.out) {
        out <- list(
            p = p,
            data = data,
            cols_used = cols_use)
        if (stats_calcd) {
            out$stats <- stats
        }
        out
    } else {
        p
    }
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
#' @param data_frame A \code{\link[SingleCellExperiment]{SingleCellExperiment}} (or Seurat) object
#' @param cell.by String name of a per-cell metadata (a column of \code{colData(object)}) containing the cluster or cell annotation identities to quantify and assess.
#' @param group.by String name of a per-cell metadata (a column of \code{colData(object)}) containing sample-group identities.
#' @param group.1,group.2 Strings naming the 2 groups within the \code{group.by} metadata which you aim to compare.
#' @param sample.by String name of a per-cell metadata (a column of \code{colData(object)}) containing which sample each cell belongs to.
#' Recommendations for cyclone data, (standardiazed because they are required elements of the file_metadata input!): \itemize{
#' \item 'file_name': holds which original fcs file each cell came from.
#' \item 'donor_id': holds which patient/mouse each cell came from.
#' \item somthing else: sometimes, your data might both break up samples' data acquisition accross multiple .fcs files (so 'file_name' would then be too specific) & contain multiple timepoints or conditions per sample (so 'donor_id' is not specific enough).
#' In such a case, the burden lies on the user to create a viable metadata (\code{<object>$<metadata-name> <- <properly-uniqued-values>}, and then use \code{sample.by = <metadata-name>})
#' \item 'donor_id' & data subsetting with `cells.use`: As an alternative to creating a new metadata to use for \code{sample.by}, subsetting to only cells from a specific timepoint or condition might serve a dual purpose of achieving your specific analysis goal && allowing 'donor_id' to properly scope to individual samples.
#' See \code{cells.use} input description for further details.
#' }
#' @param cell.targs (Optional) Single string or a string vector naming which cell groups of the \code{cell.by} metadata which should be targetted.
#' When not provided, the function will loop through all cell groups in the \code{cell.by} metadata.
#' @param cells.use Logical vector, the same length as the number of cells in the object, which sets which cells to include (TRUE) versus ignore (FALSE).
#' @param pseudocount Number, zero by default. A value to add, within fold_change calculations only, to both \code{group.1} and \code{group.2} median frequencies in order to avoid division by zero errors.
#' When needed, we recommend something small relative to the lowest expected cell frequencies of the data, 0.000001 perhaps.
#' Although a relatively small value like this can lead to heavily inflated log fold change values in the extreme cases where \code{group.1} or \code{group.2} frequencies are 0 or near 0, a tiny pseudocount leaves all other fold change values only minimally affected.
#' @param p.adjust.method String, passed along to the \code{method} input of \code{\link[stats]{p.adjust}}, any valid option for that input will work. "fdr" by default.
#' @param data.out Logical. When set to \code{TRUE}, changes the output from the stats data.frame alone to a named list containing both the stats ("stats") and the underlying per-sample frequency calculations ("data").
#' @return a data.frame, or if \code{data.out} was set to \code{TRUE}, a named list containing 2 data.frames, 'stats' and the underlying 'data'.
#' @details The function starts by utilizing \code{\link[dittoSeq]{dittoFreqPlot}} for
#' \code{cell.by}-cell frequency calculation within \code{sample.by}-samples,
#' percent normalization,
#' marking which \code{sample.by}-samples belong to which \code{group.by}-groups,
#' and trimming to only: 1. requested \code{cell.targs}, 2. \code{group.by}-groups \code{group.1} and \code{group.2}, and 3. cells matching the \code{cells.use} requirements if any were given.
#' It then removes some unnecessary columns from the data.frame returned by \code{\link[dittoSeq]{dittoFreqPlot}}. (Set \code{data.out = TRUE} to see what this cleaned return looks like!)
#'
#' Afterwards, it loops through all \code{cell.targs}, building a row of the eventual stats return for each.
#' Of note, a \code{pseudocount} can be introduced in median fold change calculation to prevent errors from division by zero. The use of a \code{pseudocount} has no effect on p-values.
#' Lastly, \code{p.adjust.method} correction, FDR by default, is applied to the 'p' column and added as a 'padj' column before data is returned.
#' @section The stats data.frame return:
#' Each row holds statistics for an individual comparison.
#' The columns represent:
#' \itemize{
#' \item cell_group: this row's cluster or cell-annotation
#' \item comparison: this groups of \code{group.by} compared in this row, formatted \code{<group.1>_vs_<group.2>}.
#' (For compatibility with running the function multiple times, each targwtting distinct groups, and then concatenating all outputs together!)
#' \item median_g1: the median frequency for the given cell_group within samples from \code{group.1}
#' \item median_g2: the median frequency for the given cell_group within samples from \code{group.2}
#' \item median_fold_change: \code{(median_g1 + pseudocount) / (median_g2 + pseudocount)}. Although zero by default, a small \code{pseudocount} can be set used to prevent division by zero error cases while only nominally affecting the value of other cases.
#' \item median_log2_fold_change: \code{log2( median_fold_change )}
#' \item positive_fc_means_up_in: Value = \code{group.1}, just a minor note to help remember the directionality of these fold changes!
#' \item p: The p-value associated with comparison of cell_group percent frequencies of group.1 samples versus group.2 samples using a Mann Whitney U Test / wilcoxon rank sum test (\code{\link[stats]{wilcox.test}}).
#' \item padj: p-values corrected by the chosen \code{p.adjust.method}, FDR by default, built from running \code{p.adjust(stats$p, method = p.adjust.method)} per all hypotheses tested in this call to the \code{freq_stats} function.
#' }
#' @author Daniel Bunis
#' @export
#' @importFrom stats wilcox.test
#' @importFrom stats p.adjust
#' @importFrom stats median
freq_stats <- function(
        data_frame,
        data_direct = FALSE,
        sample.by,
        cell.by,
        group.by, group.1, group.2,
        cell.targs = NULL,
        rows.use = NULL,
        pseudocount = 0,
        p.adjust.method = "fdr",
        n.for.correction = NULL,
        n.multiply = 1,
        data.out = FALSE
) {

    if (is.null(cell.targs)) {
        cell.targs <- colLevels(cell.by, data_frame, rows.use, used.only = TRUE)
    }

    # Collect stats with dittoSeq
    if (data_direct) {
        data <- data_frame
    } else {
        data <- .make_composition_summary_df(
            data_frame, cell.by, group.by, split.by = c(sample.by),
            rows.use, NULL, NULL,
            NULL, NULL, FALSE, 1,
            FALSE, TRUE, FALSE, TRUE, TRUE)
    }

    # Here, we loop through all the cell_groups being targeted, 1- calculating stats and 2- building a data.frame during each iteration.
    #  The lapply call performs the iteration, and gathers the data.frames output by each iteration into a list.
    #  That list of data.frames created in our lapply is then 'rbind'ed into a single data.frame.
    stats <- do.call(
        rbind,
        lapply(
            unique(data$Y),
            function(cell) {
                data_use <- data[data$Y==cell,]
                g1s <- as.vector(data_use[[group.by]]==group.1)
                g2s <- as.vector(data_use[[group.by]]==group.2)
                new <- data.frame(
                    Y = cell,
                    comparison = paste0(group.1, "_vs_", group.2),
                    group1 = group.1,
                    group2 = group.2,
                    median_g1 = median(data_use$percent[g1s], na.rm = TRUE),
                    median_g2 = median(data_use$percent[g2s], na.rm = TRUE),
                    stringsAsFactors = FALSE
                )
                if (new$median_g2==0) {
                    warning("Looks like a 'pseudocount' will be needed to avoid division by zero errors. Try adding 'pseudocount = 0.000001' to your call and see the '?freq_stats' documentation for details.")
                }
                new$median_fold_change <- (new$median_g1 + pseudocount) / (new$median_g2 + pseudocount)
                new$median_log2_fold_change <- log2(new$median_fold_change)
                new$positive_fc_means_up_in <- group.1
                new$p <- wilcox.test(x=data_use$percent[g1s],
                                     y=data_use$percent[g2s])$p.value
                new$maxy <- max(data_use$percent)
                new
            })
    )

    # Apply FDR correction
    ntests <- if (!identical(n.for.correction, NULL)) {
        n.for.correction
    } else {
        nrow(stats) * n.multiply
    }
    stats$n <- ntests
    stats$padj <- p.adjust(stats$p, method = p.adjust.method, n = ntests)

    # Output
    if (data.out) {
        list(stats = stats, data = data)
    } else {
        stats
    }
}
