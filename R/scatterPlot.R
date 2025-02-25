#' Show RNAseq data overlayed on a scatter plot
#' @import ggplot2
#'
#' @param data_frame A data_frame where columns are features and rows are observations you might wish to visualize.
#' @param x.by,y.by Single strings denoting the name of a column of \code{data_frame} containing numeric data to use for the x- and y-axis of the scatterplot.
#' @param color.by Single string denoting the name of a column of \code{data_frame} to use for setting the color of plotted points.
#' Alternatively, a string vector naming multiple such columns of data to plot at once.
#' @param shape.by Single string denoting the name of a column of \code{data_frame} containing discrete data to use for setting the shape of plotted points.
#' @param multivar.split.dir "row" or "col", sets the direction of faceting used for 'var' values when: \itemize{
#' \item \code{var} is given multiple column names
#' \item AND \code{split.by} is used to provide an additional feature to facet by
#' }
#' @param split.by 1 or 2 strings denoting the name(s) of column(s) of \code{data_frame} containing discrete data to use for faceting / separating data points into separate plots.
#'
#' When 2 columns are named, c(row,col), the first is used as rows and the second is used for columns of the resulting facet grid.
#'
#' When 1 column is named, shape control can be achieved with \code{split.nrow} and \code{split.ncol}
#'
#' @param split.nrow,split.ncol Integers which set the dimensions of faceting/splitting when faceting by a single feature.
#' @param split.show.all.others Logical which sets whether gray "others" points of facets should include all points of other facets (\code{TRUE}) versus just points left out by \code{rows.use} which would exist in the current facet (\code{FALSE}).
#' @param split.adjust A named list which allows extra parameters to be pushed through to the faceting function call.
#' List elements should be valid inputs to the faceting functions, e.g. `list(scales = "free")`.
#'
#' For options, when giving 1 column to \code{split.by}, see \code{\link[ggplot2]{facet_wrap}},
#' OR when giving 2 columns to \code{split.by}, see \code{\link[ggplot2]{facet_grid}}.
#' @param rows.use String vector of rownames of \code{data_frame} OR an integer vector specifying the row-indices of data points which should be plotted.
#'
#' Alternatively, a Logical vector, the same length as the number of rows in \code{data_frame}, where \code{TRUE} values indicate which rows to plot.
#' @param show.others Logical. TRUE by default, whether rows not targeted by \code{rows.use} should be shown in the background in light gray.
#' @param size Number which sets the size of data points. Default = 1.
#' @param color.panel String vector which sets the colors to draw from when \code{color.by} indicates discrete data.
#' \code{dittoColors()} by default, see \code{\link{dittoColors}} for contents.
#'
#' A named vector can be used if names are matched to the distinct values of the \code{color.by} data.
#' @param colors Integer vector, the indexes / order, of colors from \code{color.panel} to actually use.
#'
#' Useful for quickly swapping around colors of the default set (when not using names for color matching).
#' @param x.adjustment,y.adjustment,color.adjustment A recognized string indicating whether numeric \code{x.by}, \code{y.by}, and \code{color.by} data should be used directly (default) or should be adjusted to be
#' \itemize{
#' \item{"z-score": scaled with the scale() function to produce a relative-to-mean z-score representation}
#' \item{"relative.to.max": divided by the maximum value to give percent of max values between [0,1]}
#' }
#'
#' Ignored if the target data is not numeric as these known adjustments target numeric data only.
#'
#' In order to leave the unedited data available for use in other features, the adjusted data are put in a new column and that new column is used for plotting.
#' @param x.adj.fxn,y.adj.fxn,color.adj.fxn If you wish to apply a function to edit the \code{x.by}, \code{y.by}, or \code{color.by} data before use, in a way not possible with the \code{color.adjustment} input,
#' this input can be given a function which takes in a vector of values as input and returns a vector of values of the same length as output.
#'
#' For example, \code{function(x) \{log2(x)\}} or \code{as.factor}.
#'
#' In order to leave the unedited data available for use in other features, the adjusted data are put in a new column and that new column is used for plotting.
#' @param do.hover Logical which controls whether the ggplot output will be converted to a plotly object so that data about individual points can be displayed when you hover your cursor over them.
#' The \code{hover.data} argument is used to determine what data to show upon hover.
#' @param hover.data String vector which denotes what data to show for each data point, upon hover, when \code{do.hover} is set to \code{TRUE}.
#' Defaults to all data expected to be useful.
#' Only values present in the plotting data are actually used.
#' These can be column names of \code{data_frame} and any column names which will be created to accommodate multivar and data adjustment functionality.
#' You can run the function with \code{data.out = TRUE} and inspect the \code{$Target_data} output's columns to view your available options.
#' @param hover.round.digits Integer number specifying the number of decimal digits to round displayed numeric values to, when \code{do.hover} is set to \code{TRUE}.
#' @param shape.panel Vector of integers, corresponding to ggplot shapes, which sets what shapes to use in conjunction with \code{shape.by}.
#' When nothing is supplied to \code{shape.by}, only the first value is used.
#' Default is a set of 6, \code{c(16,15,17,23,25,8)}, the first being a simple, solid, circle.
#' @param size Number which sets the size of data points. Default = 1.
#' @param opacity Number between 0 and 1.
#' 1 = opaque. 0 = invisible. Default = 1.
#' (In terms of typical ggplot variables, = alpha)
#' @param do.ellipse Logical. Whether \code{color.by} groups should be surrounded by median-centered ellipses.
#' @param do.label  Logical. Whether to add text labels near the center (median) of \code{color.by} groups.
#' @param labels.size Number which sets the size of labels text when \code{do.label = TRUE}.
#' @param labels.highlight Logical. Whether labels should have a box behind them when \code{do.label = TRUE}.
#' @param labels.repel Logical, that sets whether the labels' placements will be adjusted with \link[ggrepel]{ggrepel} to avoid intersections between labels and plot bounds when \code{do.label = TRUE}.
#' TRUE by default.
#' @param labels.split.by String of one or two column names which controls the facet-split calculations for label placements.
#' Defaults to \code{split.by}, so generally there is no need to adjust this except when if you plan to apply faceting externally.
#' @param labels.repel.adjust A named list which allows extra parameters to be pushed through to ggrepel function calls.
#' List elements should be valid inputs to the \code{\link[ggrepel]{geom_label_repel}} by default, or \code{\link[ggrepel]{geom_text_repel}} when \code{labels.highlight = FALSE}.
#' @param labels.use.numbers Logical which controls whether numbers will be used in place of original data-values.
#' When turned on, so number to value mapping can be known, these numbers are added to the legend.
#' @param labels.numbers.spacer String. When \code{do.label = TRUE} and \code{labels.use.numbers}, this string is used in the legend between the numbers and their associated data values.
#' @param rename.color.groups String vector which sets new names for the identities of \code{color.by} groups.
#' @param rename.shape.groups String vector which sets new names for the identities of \code{shape.by} groups.
#' @param legend.show Logical. Whether any legend should be displayed. Default = \code{TRUE}.
#' @param legend.color.title,legend.shape.title Strings which set the title for the color or shape legends.
#' @param legend.color.size,legend.shape.size Numbers representing the size of shapes in the color and shape legends (for discrete variable plotting).
#' Default = 5. *Enlarging the icons in the colors legend is incredibly helpful for making colors more distinguishable by color blind individuals.
#' @param min.color color for \code{min} value of numeric \code{color.by}-data. Default = yellow
#' @param max.color color for \code{max} value of numeric \code{color.by}-data. Default = blue
#' @param min.value,max.value Number which sets the \code{color.by}-data value associated with the minimum or maximum colors.
#' @param legend.color.breaks Numeric vector which sets the discrete values to label in the color-scale legend for \code{color.by}-data.
#' @param legend.color.breaks.labels String vector, with same length as \code{legend.color.breaks}, which sets the labels for the tick marks of the color-scale.
#' @param main String, sets the plot title.
#' A default title is automatically generated based on \code{color.by} and \code{shape.by} when either are provided.
#' To remove, set to \code{NULL}.
#' @param sub String, sets the plot subtitle.
#' @param xlab,ylab Strings which set the labels for the axes. To remove, set to \code{NULL}.
#' @param do.letter Logical which sets whether letters should be added on top of the colored dots.
#' For extended colorblindness compatibility.
#' NOTE: \code{do.letter} is ignored if \code{do.hover = TRUE} or \code{shape.by} is used because lettering is incompatible with plotly and with changing the dots' to be different shapes.
#' @param do.contour Logical. Whether density-based contours should be displayed.
#' @param contour.color String that sets the color of the \code{do.contour} contours.
#' @param contour.linetype String or numeric which sets the type of line used for \code{do.contour} contours.
#' Defaults to "solid", but see \code{\link[ggplot2]{linetype}} for other options.
#' @param add.trajectory.by.groups List of vectors representing trajectory paths, each from start-group to end-group, where vector contents are the group-names indicated by the \code{trajectory.group.by} column of \code{data_frame}.
#' @param trajectory.group.by String denoting the name of a column of \code{data_frame} to use for generating trajectories from data point groups.
#' @param trajectory.arrow.size Number representing the size of trajectory arrows, in inches.  Default = 0.15.
#' @param add.trajectory.curves List of matrices, each representing coordinates for a trajectory path, from start to end, where matrix columns represent x and y coordinates of the paths.
#' @param add.xline Numeric value(s), denoting x-axis value(s), where one or more vertical line(s) should be added.
#' @param xline.linetype String which sets the type of line for \code{add.xline}.
#' Defaults to "dashed", but any ggplot linetype will work.
#' @param xline.color String that sets the color(s) of the \code{add.xline} line(s). Default = "black".
#' Alternatively, a vector of strings of the same length as \code{add.xline} can be given to set the color of each line individually.
#' @param xline.linewidth Number that sets the linewidth of the \code{add.xline} line(s). Default = 0.5.
#' Alternatively, a vector of numbers of the same length as \code{add.xline} can be given to set the linewidth of each line individually.
#' @param xline.opacity Number that sets the opacity of the \code{add.xline} line(s). Default = 1.
#' Alternatively, a vector of numbers of the same length as \code{add.xline} can be given to set the opacity of each line individually.
#' @param add.yline Numeric value(s), denoting y-axis value(s), where one or multiple horizonal line(s) should be added.
#' @param yline.linetype String which sets the type of line for \code{add.yline}.
#' Defaults to "dashed", but any ggplot linetype will work.
#' @param yline.color String that sets the color(s) of the \code{add.yline} line(s). Default = "black".
#' Alternatively, a vector of strings of the same length as \code{add.yline} can be given to set the color of each line individually.
#' @param yline.linewidth Number that sets the linewidth of the \code{add.yline} line(s). Default = 0.5.
#' Alternatively, a vector of numbers of the same length as \code{add.yline} can be given to set the linewidth of each line individually.
#' @param yline.opacity Number that sets the opacity of the \code{add.yline} line(s). Default = 1.
#' Alternatively, a vector of numbers of the same length as \code{add.yline} can be given to set the opacity of each line individually.
#' @param add.abline Numeric value(s), denoting y-axis intercept(s), where one or multiple diagonal line(s) should be added.
#' Use \code{abline.slope} to set slope(s).
#' @param abline.slope Number that sets the slope of the \code{add.abline} line(s). Default = 1.
#' Alternatively, a vector of numbers of the same length as \code{add.abline} can be given to set the slope of each line individually.
#' @param abline.linetype String which sets the type of line for \code{add.abline}.
#' Defaults to "dashed", but any ggplot linetype will work.
#' @param abline.color String that sets the color(s) of the \code{add.abline} line(s). Default = "black".
#' Alternatively, a vector of strings of the same length as \code{add.abline} can be given to set the color of each line individually.
#' @param abline.linewidth Number that sets the linewidth of the \code{add.abline} line(s). Default = 0.5.
#' Alternatively, a vector of numbers of the same length as \code{add.abline} can be given to set the linewidth of each line individually.
#' @param abline.opacity Number that sets the opacity of the \code{add.abline} line(s). Default = 1.
#' Alternatively, a vector of numbers of the same length as \code{add.abline} can be given to set the opacity of each line individually.
#' @param theme A ggplot theme which will be applied before internal adjustments.
#' Default = \code{theme_bw()}.
#' See \url{https://ggplot2.tidyverse.org/reference/ggtheme.html} for other options and ideas.
#' @param plot.order String. If the data should be plotted based on the order of the color data, sets whether to plot in "increasing", "decreasing", or "randomize"d order.
#' @param show.grid.lines Logical which sets whether grid lines should be shown within the plot space.
#' @param do.raster Logical. When set to \code{TRUE}, rasterizes the internal plot layer, changing it from individually encoded points to a flattened set of pixels.
#' This can be useful for editing in external programs (e.g. Illustrator) when there are many thousands of data points.
#' @param raster.dpi Number indicating dots/pixels per inch (dpi) to use for rasterization. Default = 300.
#' @param data.out Logical. When set to \code{TRUE}, changes the output, from the plot alone, to a list containing the plot ("p"),
#' a data.frame containing the underlying data for target rows ("Target_data"),
#' a data.frame containing the underlying data for non-target rows ("Others_data"),
#' and the ultimately used mapping of columns to given aesthetic sets ("cols_used"), because modification of newly made columns is required for many features.
#'
#' @return a ggplot scatterplot where colored dots and/or shapes represent individual rows of the given \code{data_frame}.
#'
#' Alternatively, if \code{data.out=TRUE}, a list containing four slots is output:
#' the plot (named 'p'),
#' a data.frame containing the underlying data for target rows (named 'Target_data'),
#' a data.frame containing the underlying data for non-target rows (named 'Others_data'),
#' and a list providing mappings of final column names in 'Target_data' to given plot aesthetics (named 'cols_used') because modification of newly made columns is required for many features.
#'
#' Alternatively, if \code{do.hover} is set to \code{TRUE}, the plot is coverted from ggplot to plotly &
#' additional information about each data point, determined by the \code{hover.data} input, is displayed upon hovering the cursor over the plot.
#'
#' @details
#' This function first makes any requested adjustments to data in the given \code{data_frame}, internally only, such as scaling the \code{color.by}-column if \code{color.adjustment} was given \code{"z-score"}.
#'
#' Next, if a set of rows to target was indicated with the \code{rows.use} input, then the data_frame is split into \code{Target_data} and \code{Others_data}.
#'
#' Then, rows are reordered to match with the requested \code{plot.order} behavior.
#'
#' Finally, a scatter plot is created from the resultant data.frames.
#' Non-target data points are colored in gray if \code{show.others=TRUE},
#' and target data points are displayed on top, colored and shaped based on the \code{color.by}- and \code{shape.by}-associated data.
#' If \code{split.by} was used, the plot will be split into a matrix of panels based on the associated groupings.
#'
#' @section Many characteristics of the plot can be adjusted using discrete inputs:
#' \itemize{
#' \item \code{size} and \code{opacity} can be used to adjust the size and transparency of the data points. \strong{\code{size}} can be given a number, or a column name of \code{data_frame}.
#' \item Colors used can be adjusted with \code{color.panel} and/or \code{colors} for discrete data, or \code{min}, \code{max}, \code{min.color}, and \code{max.color} for continuous data.
#' \item Shapes used can be adjusted with \code{shape.panel}.
#' \item Color and shape labels can be changed using \code{rename.color.groups} and \code{rename.shape.groups}.
#' \item Titles and axes labels can be adjusted with \code{main}, \code{sub}, \code{xlab}, \code{ylab}, and \code{legend.title} arguments.
#' \item Legends can also be adjusted in other ways, using variables that all start with "\code{legend.}" for easy tab completion lookup.
#' }
#'
#' @seealso
#' \code{\link{scatterHex}} for a hex-binned version that can be useful when points are very dense.
#'
#' @author Daniel Bunis, Jared Andrews
#' @export
#' @examples
#' example("dittoExampleData", echo = FALSE)
#'
#' # The minimal inputs for scatterPlot are the 'data_frame', and 2 column names,
#' #   given to 'x.by' and 'y.by', indicating which data to use for the x and y
#' #   axes, respectively.
#' scatterPlot(
#'     example_df, x.by = "PC1", y.by = "PC2")
#'
#' # 'color.by' and/or 'shape.by' can also be given column names in order to
#' #   show represent that columns data in the color or shape of the data points.
#' #   'shape.by' must be pointed to discrete data, but 'color.by' can be given
#' #   discrete or numeric data.
#' scatterPlot(
#'     example_df, x.by = "PC1", y.by = "PC2",
#'     color.by = "groups",
#'     shape.by = "SNP",
#'     size = 3)
#' scatterPlot(
#'     example_df, x.by = "PC1", y.by = "PC2",
#'     color.by = "gene1",
#'     size = 3)
#'
#' # Data can be "split" or faceted by a discrete variable as well.
#' scatterPlot(example_df, x.by = "PC1", y.by = "PC2", color.by = "gene1",
#'     split.by = "timepoint") # single split.by element
#' scatterPlot(example_df, x.by = "PC1", y.by = "PC2", color.by = "gene1",
#'     split.by = c("groups","SNP")) # row and col split.by elements
#'
#' # Modify the look with intuitive inputs
#' scatterPlot(example_df, x.by = "PC1", y.by = "PC2", color.by = "groups",
#'     size = 5,
#'     opacity = 0.3,
#'     show.grid.lines = FALSE,
#'     ylab = NULL, xlab = "PC2 by PC1",
#'     main = "Plot Title",
#'     sub = "subtitle",
#'     legend.color.title = "Legend\nRetitle")
#'
#' # You can restrict to only certain data points using the 'rows.use' input.
#' #   The input can be given rownames, indexes, or a logical vector
#' #   All "other" points will now only be shown as a gray background, or will not
#' #   be shown add all if you also add 'show.others = FALSE'
#' scatterPlot(example_df, x.by = "PC1", y.by = "PC2", color.by = "groups",
#'     sub = "show only first 40 observations, by index",
#'     rows.use = 1:40)
#' scatterPlot(example_df, x.by = "PC1", y.by = "PC2", color.by = "groups",
#'     sub = "show only 3 observations, by name",
#'     rows.use = c("obs1", "obs2", "obs25"))
#' scatterPlot(example_df, x.by = "PC1", y.by = "PC2", color.by = "groups",
#'     sub = "show groups A,B,D only, by logical, without others as background",
#'     rows.use = example_df$groups!="C",
#'     show.others = FALSE)
#'
#' # Many extra features are easy to add as well:
#' #   Each is started via an input starting with 'do.FEATURE*' or 'add.FEATURE*'
#' #   And when tweaks for that feature are possible, those inputs will start be
#' #   named starting with 'FEATURE*'. For example, color.by groups can be labeled
#' #   with 'do.label = TRUE' and the tweaks for this feature are given with inputs
#' #   'labels.size', 'labels.highlight', and 'labels.repel':
#' scatterPlot(example_df, x.by = "PC1", y.by = "PC2", color.by = "groups",
#'     sub = "default labeling",
#'     do.label = TRUE)          # Turns on the labeling feature
#' scatterPlot(example_df, x.by = "PC1", y.by = "PC2", color.by = "groups",
#'     sub = "tweaked labeling",
#'     do.label = TRUE,            # Turns on the labeling feature
#'     labels.size = 8,            # Adjust the text size of labels
#'     labels.highlight = FALSE,   # Removes white background behind labels
#'     # labels.use.numbers = TRUE,# Swap to number placeholders
#'     labels.repel = FALSE)       # Turns off anti-overlap location adjustments
#'
#' # Faceting can also be used to show multiple continuous variables side-by-side
#' #   by giving a vector of column names to 'color.by'.
#' #   This can also be combined with 1 'split.by' variable, with direction then
#' #   controlled via 'multivar.split.dir':
#' scatterPlot(example_df, x.by = "PC1", y.by = "PC2",
#'     color.by = c("gene1", "gene2"))
#' scatterPlot(example_df, x.by = "PC1", y.by = "PC2",
#'     color.by = c("gene1", "gene2"),
#'     split.by = "groups")
#' scatterPlot(example_df, x.by = "PC1", y.by = "PC2",
#'     color.by = c("gene1", "gene2"),
#'     split.by = "groups",
#'     multivar.split.dir = "row")
#'
#' # Sometimes, it can be useful for external editing or troubleshooting purposes
#' #   to see the underlying data that was directly used for plotting.
#' # 'data.out = TRUE' can be provided in order to obtain not just plot ("plot"),
#' #   but also the "Target_data" and "Others_data" data.frames and "cols_used"
#' #   returned as a list.
#' out <- scatterPlot(example_df, x.by = "PC1", y.by = "PC2", color.by = "groups",
#'     rows.use = 1:40,
#'     data.out = TRUE)
#' out$plot
#' summary(out$Target_data)
#' summary(out$Others_data)
#' out$cols_used
#'
scatterPlot <- function(
    data_frame,
    x.by,
    y.by,
    color.by = NULL,
    shape.by = NULL,
    split.by = NULL,
    size = 1,
    rows.use = NULL,
    show.others = TRUE,
    x.adjustment = NULL,
    y.adjustment = NULL,
    color.adjustment = NULL,
    x.adj.fxn = NULL,
    y.adj.fxn = NULL,
    color.adj.fxn = NULL,
    split.show.all.others = TRUE,
    opacity = 1,
    color.panel = dittoColors(),
    colors = seq_along(color.panel),
    split.nrow = NULL,
    split.ncol = NULL,
    split.adjust = list(),
    multivar.split.dir = c("col", "row"),
    shape.panel = c(16,15,17,23,25,8),
    rename.color.groups = NULL,
    rename.shape.groups = NULL,
    min.color = "#F0E442",
    max.color = "#0072B2",
    min.value = NA,
    max.value = NA,
    plot.order = c("unordered", "increasing", "decreasing", "randomize"),
    xlab = x.by,
    ylab = y.by,
    main = "make",
    sub = NULL,
    theme = theme_bw(),
    do.hover = FALSE,
    hover.data = unique(c(
        color.by, paste0(color.by,".color.adj"), "color.multi", "color.which",
        x.by, paste0(x.by,".x.adj"),
        y.by, paste0(y.by,".y.adj"),
        shape.by, split.by
    )),
    hover.round.digits = 5,
    do.contour = FALSE,
    contour.color = "black",
    contour.linetype = 1,
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
    do.letter = FALSE,
    do.ellipse = FALSE,
    do.label = FALSE,
    labels.size = 5,
    labels.highlight = TRUE,
    labels.use.numbers = FALSE,
    labels.numbers.spacer = ": ",
    labels.repel = TRUE,
    labels.repel.adjust = list(),
    labels.split.by = split.by,
    legend.show = TRUE,
    legend.color.title = "make",
    legend.color.size = 5,
    legend.color.breaks = waiver(),
    legend.color.breaks.labels = waiver(),
    legend.shape.title = shape.by,
    legend.shape.size = 5,
    show.grid.lines = TRUE,
    do.raster = FALSE,
    raster.dpi = 300,
    data.out = FALSE) {

    plot.order <- match.arg(plot.order)
    multivar.split.dir <- match.arg(multivar.split.dir)

    # Standardize rows vectors.
    rows.use <- .which_rows(rows.use, data_frame)
    all.rows <- .all_rows(data_frame)

    # Set titles if "make"
    main <- .leave_default_or_null(
        main, paste0(unique(c(color.by, shape.by)), collapse = " and "))
    legend.color.title <- .leave_default_or_null(
        legend.color.title, color.by, null.if = length(color.by)>1)

    ### Make dataframe edits
    edit_outs <- .data_adjust_scatter(
        data_frame, x.by, y.by, color.by, shape.by, split.by,
        x.adjustment, y.adjustment, color.adjustment,
        x.adj.fxn, y.adj.fxn, color.adj.fxn,
        rename.color.groups, rename.shape.groups,
        multivar.split.dir, rows.use, do.hover, hover.data, hover.round.digits
    )
    Target_data <- edit_outs$data_use
    Others_data <- edit_outs$data_other
    cols_use <- edit_outs$cols_use

    if (plot.order %in% c("increasing", "decreasing")) {
        Target_data <- Target_data[order(Target_data[,cols_use$color.by], decreasing = plot.order=="decreasing"),]
    } else if (plot.order == "randomize") {
        Target_data <- Target_data[sample(nrow(Target_data)),]
    }

    # Make the plot
    p <- .scatter_plot(
        Target_data, Others_data, cols_use$x.by, cols_use$y.by,
        cols_use$color.by, cols_use$shape.by, show.others, size, opacity,
        color.panel, colors, do.hover, shape.panel,
        min.color, max.color, min.value, max.value,
        xlab, ylab, main, sub, theme,
        legend.show, legend.color.title, legend.color.size,
        legend.color.breaks, legend.color.breaks.labels, legend.shape.title,
        legend.shape.size, do.raster, raster.dpi,
        cols_use$split.by, split.show.all.others, show.grid.lines)

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
        p <- .add_contours(p, Target_data, cols_use$x.by, cols_use$y.by, contour.color, contour.linetype)
    }

    p <- .add_letters_ellipses_labels_if_discrete(
        p, Target_data, cols_use$x.by, cols_use$y.by, cols_use$color.by,
        do.letter, do.ellipse, do.label,
        labels.highlight, labels.size, labels.repel, labels.split.by,
        labels.repel.adjust, labels.use.numbers, labels.numbers.spacer,
        legend.color.title,
        size, opacity, legend.color.title, legend.color.size)

    if (is.list(add.trajectory.by.groups)) {
        p <- .add_trajectories_by_groups(
            p, data_frame, cols_use$x.by, cols_use$y.by, add.trajectory.by.groups,
            trajectory.group.by, trajectory.arrow.size)
    }

    if (is.list(add.trajectory.curves)) {
        p <- .add_trajectory_curves(
            p, add.trajectory.curves, arrow.size = trajectory.arrow.size)
    }

    if (do.hover) {
        .error_if_no_plotly()
        p <- plotly::ggplotly(p, tooltip = "text")
    }

    ### RETURN the PLOT ###
    if (data.out) {
        list(
            plot = p,
            Target_data = Target_data,
            Others_data = Others_data,
            cols_used = cols_use)
    } else{
        p
    }
}

.scatter_plot <- function(
    Target_data,
    Others_data,
    x.by,
    y.by,
    color.by,
    shape.by,
    show.others,
    size,
    opacity,
    color.panel,
    colors,
    do.hover,
    shape.panel,
    min.color,
    max.color,
    min.value,
    max.value,
    xlab,
    ylab,
    main,
    sub,
    theme,
    legend.show,
    legend.color.title,
    legend.color.size,
    legend.color.breaks,
    legend.color.breaks.labels,
    legend.shape.title,
    legend.shape.size,
    do.raster,
    raster.dpi,
    split.by,
    split.show.all.others,
    show.grid.lines
) {

    ### Set up plotting
    if (!show.grid.lines) {
        theme <- theme + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    }
    p <- ggplot() + ylab(ylab) + xlab(xlab) + ggtitle(main, sub) + theme

    # Determine how to add data while adding proper theming
    aes.use <- aes(x = .data[[x.by]], y = .data[[y.by]])
    geom.args <- list(data = Target_data, alpha = opacity)

    if (is.character(size)) {
        aes.use <- modifyList(aes.use, aes(size = .data[[size]]))
    } else {
        geom.args$size <- size
    }

    if (!is.null(color.by)) {

        aes.use <- modifyList(aes.use, aes(color = .data[[color.by]]))

        if (is.numeric(Target_data[,color.by])) {
            p <- p +
                scale_colour_gradient(
                    name = legend.color.title, low = min.color, high = max.color,
                    limits = c(min.value, max.value),
                    breaks = legend.color.breaks,
                    labels = legend.color.breaks.labels)
        } else {
            p <- p +
                scale_colour_manual(
                    name = legend.color.title,
                    values = color.panel[colors]) +
                guides(color = guide_legend(override.aes = list(size=legend.color.size)))
        }
    }

    if (!is.null(shape.by)) {

        aes.use <- modifyList(aes.use, aes(shape = .data[[shape.by]]))

        p <- p +
            scale_shape_manual(
                values = shape.panel[seq_along(levels(as.factor(Target_data[,shape.by])))],
                name = legend.shape.title) +
            guides(shape = guide_legend(override.aes = list(size=legend.shape.size)))

    } else {
        geom.args$shape <- shape.panel[1]
    }

    ### Add data
    # Others_data
    if (show.others) {
        if (!is.null(split.by) && split.show.all.others) {
            Others_data <- .rep_all_data_per_facet(
                Target_data, Others_data, split.by)
        }

        if (nrow(Others_data)>0) {
            if (do.raster) {
                .error_if_no_ggrastr()
                p <- p + ggrastr::geom_point_rast(
                    data = Others_data, aes(x = .data[[x.by]], y = .data[[y.by]]),
                    size=size, color = "gray90", raster.dpi = raster.dpi)
            } else {
                p <- p + geom_point(
                    data = Others_data, aes(x = .data[[x.by]], y = .data[[y.by]]),
                    size=size, color = "gray90")
            }
        }
    }
    # Target_data
    if (do.hover) {
        aes.use <- modifyList(aes.use, aes(text = .data$hover.string))
        geom.args$mapping <- aes.use
        p <- p + suppressWarnings(do.call(geom_point, geom.args))
    } else {
        geom.args$mapping <- aes.use
        if (do.raster) {
            .error_if_no_ggrastr()
            p <- p + do.call(ggrastr::geom_point_rast, geom.args)
        } else {
            p <- p + do.call(geom_point, geom.args)
        }
    }

    if (!legend.show) {
        p <- .remove_legend(p)
    }

    p
}

.rep_all_data_per_facet <- function(Target_data, Others_data, split.by) {

    all_data <- rbind(Target_data[, colnames(Others_data)], Others_data)

    facet <- if (is.null(split.by)) {
        "filler"
    } else {
        do.call(paste, all_data[,split.by, drop = FALSE])
    }

    Others_data <- data.frame(row.names = rownames(all_data))

    Others_data <- do.call(
        rbind,
        lapply(
            unique(facet),
            function(this_facet) {

                facet_data <- all_data[facet==this_facet, , drop = FALSE]

                new_data <- all_data
                # Add facet info
                if (!is.null(split.by)) {
                    for (by in split.by) {
                        new_data[[by]] <- facet_data[1,by]
                    }
                }

                new_data
            }
        )
    )
}
