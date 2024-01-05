#' Outputs a stacked bar plot to show the percent composition of samples, groups, clusters, or other groupings
#' @import ggplot2
#'
#' @inheritParams yPlot
#' @param var Single string representing the name of a column of \code{data_frame} to quantify within x-axis groups.
#' @param group.by Single string representing the name of a column of \code{data_frame} to use for separating data across discrete x-axis groups.
#' @param scale "count" or "percent". Sets whether data should be shown as counts versus percentage.
#' @param do.hover Logical which sets whether the ggplot output should be converted to a ggplotly object with data about individual bars displayed when you hover your cursor over them.
#' @param theme A ggplot theme which will be applied before dittoViz adjustments.
#' Default = \code{theme_classic()}.
#' See \url{https://ggplot2.tidyverse.org/reference/ggtheme.html} for other options and ideas.
#' @param xlab String which sets the x-axis title.
#' Default is \code{group.by} so it defaults to the name of the grouping information.
#' Set to \code{NULL} to remove.
#' @param ylab String which sets the y-axis title.
#' Default = "make" and if left as make, a title will be automatically generated.
#' @param x.labels String vector which will replace the x-axis groupings' labels.
#' Regardless of \code{x.reorder}, the first component of \code{x.labels} sets the name for the left-most x-axis grouping.
#' @param x.labels.rotate Logical which sets whether the x-axis grouping labels should be rotated.
#' @param x.reorder Integer vector. A sequence of numbers, from 1 to the number of groupings, for rearranging the order of x-axis groupings.
#'
#' Method: Make a first plot without this input.
#' Then, treating the leftmost grouping as index 1, and the rightmost as index n.
#' Values of \code{x.reorder} should be these indices, but in the order that you would like them rearranged to be.
#'
#' Recommendation for advanced users: If you find yourself coming back to this input too many times, an alternative solution that can be easier long-term
#' is to make the target data into a factor, and to put its levels in the desired order: \code{factor(data, levels = c("level1", "level2", ...))}.
#' @param y.breaks Numeric vector which sets the plot's tick marks / major gridlines. c(break1,break2,break3,etc.)
#' @param min,max Scalars which control the zoom of the plot.
#' These inputs set the minimum / maximum values of the y-axis.
#' Default = set based on the limits of the data, 0 to 1 for \code{scale = "percent"}, or 0 to maximum count for 0 to 1 for \code{scale = "count"}.
#' @param main String, sets the plot title
#' @param sub String, sets the plot subtitle
#' @param var.labels.rename String vector for renaming the distinct identities of \code{var}-values.
#' This vector must be the same length as the number of levels or unique values in the \code{var}-data.
#'
#' Hint: use \code{\link{colLevels}} or \code{unique(data_frame[,var])} to original values.
#' @param var.labels.reorder Integer vector. A sequence of numbers, from 1 to the number of distinct \code{var}-value identities, for rearranging the order labels' groupings within the plot space.
#'
#' Method: Make a first plot without this input.
#' Then, treating the top-most grouping as index 1, and the bottom-most as index n.
#' Values of \code{var.labels.reorder} should be these indices, but in the order that you would like them rearranged to be.
#' @param legend.title String which adds a title to the legend.
#' @param data.out Logical. When set to \code{TRUE}, changes the output, from the plot alone, to a list containing the plot ("p") and a data.frame ("data") containing the underlying data.
#' @param retain.factor.levels Logical which controls whether factor identities of \code{var} and \code{group.by} data should be respected.
#' Set to TRUE to faithfully reflect ordering of groupings encoded in factor levels,
#' but Note that this will also force retention of groupings that could otherwise be removed via \code{rows.use}.
#' @param data.only Logical. When set to \code{TRUE}, the underlying data will be returned, but not the plot itself.
#'
#' @return A ggplot plot where discrete data, grouped by sample, condition, cluster, etc. on the x-axis, is shown on the y-axis as either counts or percent-of-total-per-grouping in a stacked barplot.
#'
#' Alternatively, if \code{data.out = TRUE}, a list containing the plot ("p") and a dataframe of the underlying data ("data").
#'
#' Alternatively, if \code{do.hover = TRUE}, a plotly conversion of the ggplot output in which underlying data can be retrieved upon hovering the cursor over the plot.
#' @details
#' The function creates a dataframe containing counts and percent makeup of \code{var} identities for each x-axis grouping (determined by the \code{group.by} input).
#' If a subset of data points to use is indicated with the \code{rows.use} input, only those rows of the \code{data_frame} are used for counts and percent makeup calculations.
#' In other words, the \code{row.use} input adjusts the universe that compositions are calculated within.
#' Then, a vertical bar plot is generated (\code{ggplot2::geom_col()}) showing either percent makeup if
#' \code{scale = "percent"}, which is the default, or raw counts if \code{scale = "count"}.
#'
#' @section Many characteristics of the plot can be adjusted using discrete inputs:
#' \itemize{
#' \item Colors can be adjusted with \code{color.panel} and/or \code{colors}.
#' \item y-axis zoom and tick marks can be adjusted using \code{min}, \code{max}, and \code{y.breaks}.
#' \item Titles can be adjusted with \code{main}, \code{sub}, \code{xlab}, \code{ylab}, and \code{legend.title} arguments.
#' \item The legend can be removed by setting \code{legend.show = FALSE}.
#' \item x-axis labels and groupings can be changed / reordered using \code{x.labels} and \code{x.reorder}, and rotation of these labels can be turned off with \code{x.labels.rotate = FALSE}.
#' \item y-axis \code{var}-group labels and their order can be changed / reordered using \code{var.labels} and \code{var.labels.reorder}.
#' }
#'
#' @examples
#' example("dittoExampleData", echo = FALSE)
#'
#' # There are two main inputs for this function, in addition to 'data_frame'.
#' #  var = typically this will be observation-type annotations or clustering
#' #    This is the set of observations for which we will calculate frequencies
#' #    (per each unique value of this data) within each group
#' #  group.by = how to group observations together
#' barPlot(
#'     data_frame = example_df,
#'     var = "clustering",
#'     group.by = "groups")
#'
#' # 'scale' then allows choice of scaling by 'percent' (default) or 'count'
#' barPlot(example_df, "clustering", group.by = "groups",
#'     scale = "count")
#'
#' # Particular observations can be ignored from calculations and plotting using
#' #   the 'rows.use' input.
#' #   Here, we'll remove an entire "cluster" from consideration, but notice the
#' #     fractions will still sum to 1.
#' barPlot(example_df, "clustering", group.by = "groups",
#'     rows.use = example_df$clustering!="1")
#'
#' ### Accessing underlying data:
#' # as data.frame, with plot returned too
#' barPlot(example_df, "clustering", group.by = "groups",
#'     data.out = TRUE)
#' # as data.frame, no plot
#' barPlot(example_df, "clustering", group.by = "groups",
#'     data.out = TRUE,
#'     data.only = TRUE)
#' # through hovering the cursor over the relevant parts of the plot
#' if (requireNamespace("plotly", quietly = TRUE)) {
#'     barPlot(example_df, "clustering", group.by = "groups",
#'         do.hover = TRUE)
#'     }
#'
#' @author Daniel Bunis
#' @export

barPlot <- function(
    data_frame,
    var,
    group.by,
    scale = c("percent", "count"),
    split.by = NULL,
    rows.use = NULL,
    retain.factor.levels = TRUE,
    data.out = FALSE,
    data.only = FALSE,
    do.hover = FALSE,
    color.panel = dittoColors(),
    colors = seq_along(color.panel),
    split.nrow = NULL,
    split.ncol = NULL,
    split.adjust = list(),
    y.breaks = NA,
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
    legend.show = TRUE,
    legend.title = NULL) {

    scale <- match.arg(scale)

    # Gather data
    data <- .make_composition_summary_df(
        data_frame, var, group.by, split.by, rows.use, x.reorder, x.labels,
        var.labels.reorder, var.labels.rename, do.hover, FALSE,
        retain.factor.levels, retain.factor.levels)
    if (data.only) {
        return(data)
    }

    # Decide titles
    ylab <- .leave_default_or_null(ylab, scale)
    main <- .leave_default_or_null(main, var)

    #Build Plot
    p <- ggplot(
        data=data,
        aes_string(x = "grouping", y= scale, fill = "label")) +
        theme + xlab(xlab) + ylab(ylab) + ggtitle(main, subtitle = sub) +
        scale_fill_manual(name = legend.title, values = color.panel[colors]) +
        if (x.labels.rotate) {
            theme(axis.text.x= element_text(
                angle=45, hjust = 1, vjust = 1))
        }
        #Add the bars.
        if(do.hover){
            p <- p + suppressWarnings(geom_col(
                aes_string(text = "hover.string")))
        } else {
            p <- p + geom_col()
        }
    # Set y-axis ticks & scaling
    if (is.na(y.breaks[1]) && scale == "percent") {
        y.breaks <- c(0,0.5,1)
    }
    if (!is.na(y.breaks[1])) {
        p <- p + scale_y_continuous(breaks = y.breaks)
    }
    if (is.null(max)) {
        max <- ifelse(scale == "percent", 1, max(data$label.count.total))
    }
    p <- p + coord_cartesian(ylim=c(min,max))

    ### Add extra features
    if (!is.null(split.by)) {
        p <- .add_splitting(
            p, split.by, split.nrow, split.ncol, split.adjust)
    }

    if (!legend.show) {
        p <- .remove_legend(p)
    }

    if (do.hover) {
        .error_if_no_plotly()
        p <- plotly::ggplotly(p, tooltip = "text")
    }

    #DONE. Return the plot
    if (data.out) {
        list(
            p = p,
            data = data)
    } else {
        p
    }
}

.make_composition_summary_df <- function(
    data_frame, var, group.by, split.by, rows.use,
    x.reorder, x.labels,
    var.labels.reorder, var.labels.rename,
    do.hover, max.normalize = FALSE,
    retain.factor.levels.var, retain.factor.levels.group,
    make.factor.var = FALSE, keep.level.order.group = FALSE
) {

    rows.use <- .which_rows(rows.use, data_frame)

    data_frame_use <- data_frame[rows.use, , drop = FALSE]

    # Extract x.grouping and y.labels data
    y.var <- ._col(var, data_frame_use, add.names = FALSE)
    x.var <- ._col(group.by, data_frame_use, add.names = FALSE)
    if (any(is.na(x.var))) {
        stop('Cannot calculate composition among grouping data containing NAs. Offending column: ', group.by)
    }

    # Factor editting
    if(!retain.factor.levels.var) {
        y.var <- as.character(y.var)
    }
    if(make.factor.var) {
        y.var <- as.factor(y.var)
    }
    x.levs <- levels(as.factor(x.var))
    if(!retain.factor.levels.group) {
        x.var <- as.character(x.var)
    }

    # Extract or negate-away split.by data
    facet <- "filler"
    split.data <- list()
    if (!is.null(split.by)) {
        for (by in seq_along(split.by)) {
            split.data[[by]] <- data_frame_use[, split.by[by]]
            if (any(is.na(split.data[[by]]))) {
                stop('Cannot calculate composition among sub-grouping data containing NAs. Offending column: ', split.by[by])
            }
        }
        facet <- do.call(paste, split.data)
    }

    # Create dataframe (per split.by group)
    data <- do.call(
        rbind,
        lapply(
            unique(facet),
            function(this_facet) {

                # Subset data per facet
                use <- facet==this_facet
                use_first <- which(use)[1]
                y.var <- y.var[use]
                x.var <- x.var[use]

                # Create data frame
                new <- data.frame(table(y.var, x.var))
                names(new) <- c("label", "grouping", "count")

                new$label.count.total.per.facet <- rep(
                    as.vector(table(x.var)),
                    each = length(levels(as.factor(y.var))))
                new$percent <- new$count / new$label.count.total.per.facet

                # Catch 0/0
                new$percent[is.nan(new$percent)] <- 0

                # Add facet info
                for (by in seq_along(split.by)) {
                    new[[split.by[by]]] <- split.data[[by]][use_first]
                }

                new
            }
        )
    )

    # max.normalization per var-label
    if (max.normalize) {
        data$count.norm <- 0
        data$percent.norm <- 0

        for (i in unique(data$label)) {
            this_lab <- data$label == i
            data$count.norm[this_lab] <-
                data$count[this_lab]/max(data$count[this_lab])
            data$percent.norm[this_lab] <-
                data$percent[this_lab]/max(data$percent[this_lab])
        }
    }

    # Rename/reorder
    if(keep.level.order.group){
        data$grouping <- factor(data$grouping, levels = x.levs)
    }
    data$grouping <- .rename_and_or_reorder(data$grouping, x.reorder, x.labels)
    data$label <- .rename_and_or_reorder(
        data$label, var.labels.reorder, var.labels.rename)

    # Add hover info
    if (do.hover) {
        hover.data <- data[,names(data) %in% c("label", "count", "percent")]
        names(hover.data)[1] <- var
        # Make hover strings, "data.type: data" \n "data.type: data"
        data$hover.string <- .make_hover_strings_from_df(hover.data)
    }

    data
}
