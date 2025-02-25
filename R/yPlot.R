#' Plots continuous data per group on a y- (or x-) axis using customizable data representations
#' @import ggplot2
#'
#' @inheritParams scatterPlot
#'
#' @param var Single string representing the name of a column of \code{data_frame} to be used as the primary, y-axis, data.
#' Alternatively, a string vector naming multiple such columns of data to plot at once.
#' See the input \code{multivar.aes} to understand or tweak how multiple var-data will be shown.
#' @param group.by Single string representing the name of a column of \code{data_frame} containing discrete data to use for separating the data points into groups.
#' @param color.by Single string representing the name of a column of \code{data_frame} containing discrete data to use for setting data representation color fills.
#' This data does not need to be the same as \code{group.by}, which is great for highlighting supersets or subgroups when wanted, but it defaults to \code{group.by} so the input can often be skipped.
#' @param shape.by Single string representing the name of a column of \code{data_frame} containing discrete data to use for setting shapes of the jitter points.
#' When not provided, all jitter points will be dots.
#' @param multivar.aes "split", "group", or "color", the plot feature to utilize for displaying 'var' value when \code{var} is given multiple column names.
#' When set to "split" (the default), note that displaying the \code{var}-identity of the data will be prioritized so the \code{split.by} input becomes limited to receiving a single usable element.
#' @param multivar.split.dir "row" or "col", sets the direction of faceting used for 'var' values when: \itemize{
#' \item \code{var} is given multiple column names
#' \item \code{multivar.aes = "split"} (default)
#' \item AND \code{split.by} is used to provide an additional feature to facet by
#' }
#' @param plots String vector which sets the types of plots to include: possibilities = "jitter", "boxplot", "vlnplot", "ridgeplot".
#'
#' Order matters: c("vlnplot", "boxplot", "jitter") will put a violin plot in the back, boxplot in the middle, and then individual dots in the front.
#'
#' See details section for more info.
#' @param color.panel String vector which sets the colors to draw from for data representation fills.
#' Default = \code{dittoColors()}.
#'
#' A named vector can be used if names are matched to the distinct values of the \code{color.by} data.
#' @param shape.panel Vector of integers corresponding to ggplot shapes which sets what shapes to use.
#' When discrete groupings are supplied by \code{shape.by}, this sets the panel of shapes which will be used.
#' When nothing is supplied to \code{shape.by}, only the first value is used.
#' Default is a set of 6, \code{c(16,15,17,23,25,8)}, the first being a simple, solid, circle.
#' @param var.adjustment A recognized string indicating whether numeric \code{var} data should be used directly (default) or should be adjusted to be
#' \itemize{
#' \item{"z-score": scaled with the scale() function to produce a relative-to-mean z-score representation}
#' \item{"relative.to.max": divided by the maximum expression value to give percent of max values between [0,1]}
#' }
#'
#' Ignored if the \code{var} data is not numeric as these known adjustments target numeric data only.
#'
#' In order to leave the unedited data available for use in other features, the adjusted data are put in a new column and that new column is used for plotting.
#' @param var.adj.fxn If you wish to apply a function to edit the \code{var} data before use, in a way not possible with the \code{var.adjustment} input,
#' this input can be given a function which takes in a vector of values as input and returns a vector of values of the same length as output.
#'
#' For example, \code{function(x) \{log2(x)\}} or \code{as.factor}.
#'
#' In order to leave the unedited data available for use in other features, the adjusted data are put in a new column and that new column is used for plotting.
#' @param hover.data String vector which denotes what data to show for each jitter data point, upon hover, when \code{do.hover} is set to \code{TRUE}.
#' Defaults to all data expected to be useful.
#' Only values present in the plotting data are actually used.
#' These can be column names of \code{data_frame} and any column names which will be created to accommodate multivar and data adjustment functionality.
#' You can run the function with \code{data.out = TRUE} and inspect the \code{$data} output's columns to view your available options.
#' @param main String, sets the plot title. Default = "make" and if left as make, a title will be automatically generated.  To remove, set to \code{NULL}.
#' @param theme A ggplot theme which will be applied before internal adjustments.
#' Default = \code{theme_classic()}.
#' See \url{https://ggplot2.tidyverse.org/reference/ggtheme.html} for other options and ideas.
#' @param xlab String which sets the grouping-axis label (=x-axis for box and violin plots, y-axis for ridgeplots).
#' Set to \code{NULL} to remove.
#' @param ylab String, sets the continuous-axis label (=y-axis for box and violin plots, x-axis for ridgeplots).
#' Defaults to "\code{var}".
#' @param y.breaks Numeric vector, a set of breaks that should be used as major grid lines. c(break1,break2,break3,etc.).
#' @param min,max Scalars which control the zoom on the continuous axis of the plot.
#' @param x.labels String vector, c("label1","label2","label3",...) which overrides the names of groupings.
#' @param x.reorder Integer vector. A sequence of numbers, from 1 to the number of groupings, for rearranging the order of x-axis groupings.
#'
#' Method: Make a first plot without this input.
#' Then, treating the leftmost grouping as index 1, and the rightmost as index n.
#' Values of x.reorder should be these indices, but in the order that you would like them rearranged to be.
#'
#' Recommendation for advanced users: If you find yourself coming back to this input too many times, an alternative solution that can be easier long-term
#' is to make the target data into a factor, and to put its levels in the desired order: \code{factor(data, levels = c("level1", "level2", ...))}.
#' @param x.labels.rotate Logical which sets whether the labels should be rotated.
#' Default: \code{TRUE} for violin and box plots, but \code{FALSE} for ridgeplots.
#' @param add.line Numeric value(s), denoting y-axis value(s), where one or multiple horizonal line(s) should be added.
#' @param line.linetype String which sets the type of line for \code{add.line}.
#' Defaults to "dashed", but any ggplot linetype will work.
#' @param line.color String that sets the color(s) of the \code{add.line} line(s). Default = "black".
#' Alternatively, a vector of strings of the same length as \code{add.line} can be given to set the color of each line individually.
#' @param line.linewidth Number that sets the linewidth of the \code{add.line} line(s). Default = 0.5.
#' Alternatively, a vector of numbers of the same length as \code{add.line} can be given to set the linewidth of each line individually.
#' @param line.opacity Number that sets the opacity of the \code{add.line} line(s). Default = 1.
#' Alternatively, a vector of numbers of the same length as \code{add.line} can be given to set the opacity of each line individually.
#' @param jitter.size Scalar which sets the size of the jitter shapes.
#' @param jitter.width Scalar that sets the width/spread of the jitter in the x direction. Ignored in ridgeplots.
#'
#' Note for when \code{color.by} is used to split x-axis groupings into additional bins: ggplot does not shrink jitter widths accordingly, so be sure to do so yourself!
#' Ideally, needs to be 0.5/num_subgroups.
#' @param jitter.color String which sets the color of the jitter shapes
#' @param jitter.shape.legend.size Scalar which changes the size of the shape key in the legend.
#' If set to \code{NA}, \code{jitter.size} is used.
#' @param jitter.shape.legend.show Logical which sets whether the shapes legend will be shown when its shape is determined by \code{shape.by}.
#' @param jitter.position.dodge Scalar which adjusts the relative distance between jitter widths when multiple subgroups exist per \code{group.by} grouping (a.k.a. when \code{group.by} and \code{color.by} are not equal).
#' Similar to \code{boxplot.position.dodge} input & defaults to the value of that input so that BOTH will actually be adjusted when only, say, \code{boxplot.position.dodge = 0.3} is given.
#' @param do.raster Logical. When set to \code{TRUE}, rasterizes the jitter plot layer, changing it from individually encoded points to a flattened set of pixels.
#' This can be useful for editing in external programs (e.g. Illustrator) when there are many thousands of data points.
#' @param raster.dpi Number indicating dots/pixels per inch (dpi) to use for rasterization. Default = 300.
#' @param boxplot.width Scalar which sets the width/spread of the boxplot in the x direction
#' @param boxplot.color String which sets the color of the lines of the boxplot
#' @param boxplot.show.outliers Logical, whether outliers should by including in the boxplot.
#' Default is \code{FALSE} when there is a jitter plotted, \code{TRUE} if there is no jitter.
#' @param boxplot.outlier.size Scalar which adjusts the size of points used to mark outliers.
#' @param boxplot.fill Logical, whether the boxplot should be filled in or not.
#' Known bug: when boxplot fill is turned off, outliers do not render.
#' @param boxplot.position.dodge Scalar which adjusts the relative distance between boxplots when multiple are drawn per grouping (a.k.a. when \code{group.by} and \code{color.by} are not equal).
#' By default, this input actually controls the value of \code{jitter.position.dodge} unless the \code{jitter} version is provided separately.
#' @param boxplot.lineweight Scalar which adjusts the thickness of boxplot lines.
#' @param vlnplot.lineweight Scalar which sets the thickness of the line that outlines the violin plots.
#' @param vlnplot.width Scalar which sets the width/spread of violin plots in the x direction
#' @param vlnplot.scaling String which sets how the widths of the of violin plots are set in relation to each other.
#' Options are "area", "count", and "width". If the default is not right for your data, I recommend trying "width".
#' For an explanation of each, see \code{\link[ggplot2]{geom_violin}}.
#' @param vlnplot.quantiles Single number or numeric vector of values in [0,1] naming quantiles at which to draw a horizontal line within each violin plot. Example: \code{c(0.1, 0.5, 0.9)}
#' @param ridgeplot.lineweight Scalar which sets the thickness of the ridgeplot outline.
#' @param ridgeplot.scale Scalar which sets the distance/overlap between ridgeplots.
#' A value of 1 means the tallest density curve just touches the baseline of the next higher one.
#' Higher numbers lead to greater overlap.  Default = 1.25
#' @param ridgeplot.ymax.expansion Scalar which adjusts the minimal space between the topmost grouping and the top of the plot in order to ensure the curve is not cut off by the plotting grid.
#' The larger the value, the greater the space requested.
#' When left as NA, dittoViz will attempt to determine an ideal value itself based on the number of groups & linear interpolation between these goal posts: #groups of 3 or fewer: 0.6; #groups=12: 0.1; #groups or 34 or greater: 0.05.
#' @param ridgeplot.shape Either "smooth" or "hist", sets whether ridges will be smoothed (the typical, and default) versus rectangular like a histogram.
#' (Note: as of the time shape "hist" was added, combination of jittered points is not supported by the \code{\link[ggridges]{stat_binline}} that dittoViz relies on.)
#' @param ridgeplot.bins Integer which sets how many chunks to break the x-axis into when \code{ridgeplot.shape = "hist"}.
#' Overridden by \code{ridgeplot.binwidth} when that input is provided.
#' @param ridgeplot.binwidth Integer which sets the width of chunks to break the x-axis into when \code{ridgeplot.shape = "hist"}.
#' Takes precedence over \code{ridgeplot.bins} when provided.
#' @param legend.show Logical. Whether the legend should be displayed. Default = \code{TRUE}.
#' @param legend.title String or \code{NULL}, sets the title for the main legend which includes colors and data representations.
#' @param data.out Logical. When set to \code{TRUE}, changes the output, from the plot alone, to a list containing the plot (\code{p}), its underlying data (\code{data}),
#' and the ultimately used mapping of columns to given aesthetic sets, because modification of newly made columns is required for many features ("cols_used").
#' @param ... arguments passed to yPlot by ridgePlot, ridgeJitter, and boxPlot wrappers.
#' Options are all the ones above.
#'
#' @return a ggplot where continuous data, grouped by sample, age, cluster, etc., shown on either the y-axis by a violin plot, boxplot, and/or jittered points, or on the x-axis by a ridgeplot with or without jittered points.
#'
#' Alternatively when \code{data.out=TRUE}, a list containing
#' the plot ("p")
#' the underlying data as a dataframe ("data"),
#' and the ultimately used mapping of columns to given aesthetic sets ("cols_used"), because modification of newly made columns is required for many features.
#'
#' Alternatively when \code{do.hover = TRUE}, a plotly converted version of the ggplot where additional data will be displayed when the cursor is hovered over jitter points.
#' @details
#' The function plots the targeted \code{var} data of \code{data_frame}, grouped by the columns of data given to \code{group.by} and \code{color.by}, using data representations given by \code{plots}.
#' Data representations will also be colored (filled) based on \code{color.by}.
#' If a subset of data points to use is indicated with the \code{rows.use} input, the data_frame is internally subset to include only those indicated rows before plotting.
#'
#' The \code{plots} argument determines the types of data representation that will be generated, as well as their order from back to front.
#' Options are \code{"jitter"}, \code{"boxplot"}, \code{"vlnplot"}, and \code{"ridgeplot"}.
#' Inclusion of \code{"ridgeplot"} overrides \code{"boxplot"} and \code{"vlnplot"} presence and changes the plot to be horizontal.
#'
#' When \code{split.by} is provided a column name of \code{data_frame}, separate plots will be produced representing each of the distinct groupings of the split.by data using ggplots facetting functionality.
#'
#' \code{ridgePlot}, \code{ridgeJitter}, and \code{boxPlot} are included as wrappers of the basic \code{yPlot} function
#' that simply change the default for the \code{plots} input to be \code{"ridgeplot"}, \code{c("ridgeplot","jitter")}, or \code{c("boxplot","jitter")},
#' to make such plots even easier to produce.
#'
#' @section Many characteristics of the plot can be adjusted using discrete inputs:
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
#' \item \strong{Shapes used} in conjunction with \code{shape.by} can be adjusted with \code{shape.panel}.
#' This can be very useful for making manual additional alterations \emph{after} dittoViz plot generation.
#' }
#' @seealso
#' \code{\link{ridgePlot}}, \code{\link{ridgeJitter}}, and \code{\link{boxPlot}} for shortcuts to a few 'plots' input shortcuts
#'
#' @examples
#' example("dittoExampleData", echo = FALSE)
#'
#' # Basic yPlot, with jitter behind a vlnplot (looks better with more points)
#' yPlot(data_frame = example_df, var = "gene1", group.by = "timepoint")
#' yPlot(data_frame = example_df, var = c("gene1", "gene2"), group.by = "timepoint")
#'
#' # Color distinctly from the grouping variable using 'color.by'
#' yPlot(data_frame = example_df, var = "gene1", group.by = "timepoint",
#'     color.by = "conditions")
#'
#' # Update the 'plots' input to change / reorder the data representations
#' yPlot(example_df, "gene1", "timepoint",
#'     plots = c("vlnplot", "boxplot", "jitter"))
#' yPlot(example_df, "gene1", "timepoint",
#'     plots = c("ridgeplot", "jitter"))
#'
#' # Provided wrappers enable certain easy adjustments of the 'plots' parameter.
#' # Quickly make a Boxplot
#' boxPlot(example_df, "gene1", "timepoint")
#' # Quickly make a Ridgeplot, with or without jitter
#' ridgePlot(example_df, "gene1", "timepoint")
#' ridgeJitter(example_df, "gene1", "timepoint")
#'
#' # Modify the look with intuitive inputs
#' yPlot(example_df, "gene1", "timepoint",
#'     plots = c("vlnplot", "boxplot", "jitter"),
#'     boxplot.color = "white",
#'     main = "CD3E",
#'     legend.show = FALSE)
#'
#' \dontrun{
#' # (Due to unfortunate CRAN submission constraints)
#'
#' # Data can also be split in other ways with 'shape.by' or 'split.by'
#' yPlot(data_frame = example_df, var = "gene1", group.by = "timepoint",
#'     plots = c("vlnplot", "boxplot", "jitter"),
#'     shape.by = "clustering",
#'     split.by = "SNP") # single split.by element
#' yPlot(data_frame = example_df, var = "gene1", group.by = "timepoint",
#'     plots = c("vlnplot", "boxplot", "jitter"),
#'     split.by = c("groups","SNP")) # row and col split.by elements
#'
#' # Multiple features can also be plotted at once by giving them as a vector to
#' #   the 'var' input. One aesthetic of the plot will then be used to display the
#' #   'var'-info, and you can control which (faceting / "split", x-axis grouping
#' #   / "group", or color / "color") with 'multivar.aes':
#' yPlot(data_frame = example_df, group.by = "timepoint",
#'     var = c("gene1", "gene2"))
#' yPlot(data_frame = example_df, group.by = "timepoint",
#'     var = c("gene1", "gene2"),
#'     multivar.aes = "group")
#' yPlot(data_frame = example_df, group.by = "timepoint",
#'     var = c("gene1", "gene2"),
#'     multivar.aes = "color")
#'
#' }
#'
#' @author Daniel Bunis, Jared Andrews
#' @export

yPlot <- function(
    data_frame,
    var,
    group.by,
    color.by = group.by,
    shape.by = NULL,
    split.by = NULL,
    rows.use = NULL,
    plots = c("vlnplot","boxplot","jitter"),
    multivar.aes = c("split", "group", "color"),
    multivar.split.dir = c("col", "row"),
    var.adjustment = NULL,
    var.adj.fxn = NULL,
    do.hover = FALSE,
    hover.data = unique(c(
        var, paste0(var,".adj"), "var.multi", "var.which",
        group.by, color.by, shape.by, split.by
    )),
    hover.round.digits = 5,
    color.panel = dittoColors(),
    colors = seq_along(color.panel),
    shape.panel = c(16,15,17,23,25,8),
    theme = theme_classic(),
    main = "make",
    sub = NULL,
    ylab = "make",
    y.breaks = NULL,
    min = NA,
    max = NA,
    xlab = "make",
    x.labels = NULL,
    x.labels.rotate = NA,
    x.reorder = NULL,
    split.nrow = NULL,
    split.ncol = NULL,
    split.adjust = list(),
    do.raster = FALSE,
    raster.dpi = 300,
    jitter.size = 1,
    jitter.width = 0.2,
    jitter.color = "black",
    jitter.shape.legend.size = 5,
    jitter.shape.legend.show = TRUE,
    jitter.position.dodge = boxplot.position.dodge,
    boxplot.width = 0.2,
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
    line.linewidth = 0.5,
    line.opacity = 1,
    legend.show = TRUE,
    legend.title = "make",
    data.out = FALSE) {

    ridgeplot.shape <- match.arg(ridgeplot.shape)
    multivar.aes <- match.arg(multivar.aes)
    multivar.split.dir <- match.arg(multivar.split.dir)

    #Populate rows.use with a list of names if it was given anything else.
    rows.use <- .which_rows(rows.use, data_frame)
    all.rows <- .all_rows(data_frame)

    #Parse Title Defaults
    main <- .leave_default_or_null(
        main, default = paste0(unique(c(var, shape.by)), collapse = " and "))
    xlab <- .leave_default_or_null(
        xlab, default = group.by,
        null.if = multivar.aes=="group" && length(var)>1)
    ylab <- .leave_default_or_null(
        ylab, default = var, null.if = identical(main,"var") || length(var)>1)
    legend.title <- .leave_default_or_null(
        legend.title, var, null.if = is.null(shape.by))

    ### Make data_frame and aesthetic target edits
    cols_use <- list(
        var = var,
        group.by = group.by,
        color.by = color.by,
        shape.by = shape.by,
        split.by = split.by,
        group.aes = group.by
    )
    if (group.by != color.by) {
        cols_use$group.aes <- "group.aes"
        data_frame$`group.aes` <- paste0(
            as.character(data_frame[,group.by]),
            ".-.",
            as.character(data_frame[,color.by])
        )
    }
    # Relabel/reorder for groups
    data_frame[,group.by] <-
        .rename_and_or_reorder(data_frame[,group.by], x.reorder, x.labels)
    if (length(var) > 1) {
        # (Only numeric data supported, handles color adjustment and rows.use subsetting)
        multi_out <- .multi_var_restructure(
            data_frame, var, "var.multi", "var.which",
            var.adjustment, var.adj.fxn, rows.use,
            multivar.split.dir, split.by, multivar.aes
        )
        Target_data <- multi_out$data_use
        cols_use$var <- "var.multi"
        cols_use$split.by <- multi_out$split.by
        if (multivar.aes == "group") {
            cols_use$group.by <- "var.which"
        }
        if (multivar.aes == "color") {
            cols_use$color.by <- "var.which"
        }
    } else {
        # color adjustments
        if (!is.null(var.adjustment) || !is.null(var.adj.fxn)) {
            cols_use$var <- paste0(var, ".adj")
            data_frame[,cols_use$var] <-
                ._col(var, data_frame, var.adjustment, var.adj.fxn)
        }
        # rows.use subsetting
        Target_data <- data_frame[rows.use,]
    }
    # Hover prep
    if (do.hover) {
        hover_exists <- hover.data[hover.data %in% colnames(Target_data)]
        Target_data$hover.string <- .make_hover_strings_from_df(
            Target_data[,hover_exists,drop=FALSE], hover.round.digits)
        cols_use$hover.text <- "hover.string"
    }

    # Make the plot
    p <- ggplot(Target_data, aes(fill=.data[[cols_use$color.by]])) +
        theme +
        scale_fill_manual(name = legend.title, values=color.panel[colors]) +
        ggtitle(main, sub)
    if(!("ridgeplot" %in% plots)) {
        p <- .yPlot_add_data_y_direction(
            p, Target_data, cols_use$var, cols_use$group.by, cols_use$shape.by,
            plots, xlab, ylab, jitter.size,
            jitter.width, jitter.color, shape.panel, jitter.shape.legend.size,
            jitter.shape.legend.show, jitter.position.dodge,
            do.raster, raster.dpi,
            boxplot.width, boxplot.color, boxplot.show.outliers,
            boxplot.outlier.size, boxplot.fill,
            boxplot.position.dodge, boxplot.lineweight,
            vlnplot.lineweight, vlnplot.width, vlnplot.scaling,
            vlnplot.quantiles,
            x.labels.rotate, do.hover, y.breaks, min, max, data_frame,
            cols_use$group.aes)
    } else {
        p <- .yPlot_add_data_x_direction(
            p, Target_data, cols_use$var, cols_use$group.by,
            plots, xlab, ylab, jitter.size, jitter.color,
            jitter.shape.legend.size, jitter.shape.legend.show,
            ridgeplot.lineweight, ridgeplot.scale, ridgeplot.ymax.expansion,
            ridgeplot.shape, ridgeplot.bins, ridgeplot.binwidth,
            x.labels.rotate, do.hover, color.panel,
            colors, y.breaks, min, max)
    }
    # Extra tweaks
    if (!is.null(cols_use$split.by)) {
        p <- .add_splitting(
            p, cols_use$split.by, split.nrow, split.ncol, split.adjust)
    }

    # Get number of panels so that replicates of aesthetics can be generated if supplied for each line.
    pp <- ggplot_build(p)
    num.panels <- length(levels(pp$data[[1]]$PANEL))

    if (!is.null(add.line)) {
        if(!("ridgeplot" %in% plots)) {
            p <- .add_yline(p, add.line, line.linetype, line.color, line.linewidth, line.opacity, num.panels)
        } else {
            p <- .add_xline(p, add.line, line.linetype, line.color, line.linewidth, line.opacity, num.panels)
        }
    }

    if (!legend.show) {
        p <- .remove_legend(p)
    }

    if (do.hover) {
        p <- .warn_or_apply_plotly(p, plots)
    }

    # DONE. Return the plot +/- data
    if (data.out) {
        list(
            p = p,
            data = Target_data,
            cols_used = cols_use)
    } else {
        p
    }
}

.yPlot_add_data_y_direction <- function(
    p, Target_data, var, group.by, shape.by,
    plots, xlab, ylab,
    jitter.size, jitter.width, jitter.color,shape.panel,
    jitter.shape.legend.size, jitter.shape.legend.show, jitter.position.dodge,
    do.raster, raster.dpi,
    boxplot.width, boxplot.color, boxplot.show.outliers, boxplot.outlier.size,
    boxplot.fill, boxplot.position.dodge, boxplot.lineweight,
    vlnplot.lineweight, vlnplot.width, vlnplot.scaling, vlnplot.quantiles,
    x.labels.rotate, do.hover, y.breaks, min, max,
    data_frame, group.aes) {
    # This function takes in a partial yPlot ggplot data_frame without any data
    # overlay, and parses adding the main data visualizations.
    # Adds plots based on what is requested in plots, ordered by their order.

    # Now that we know the plot's direction, set direction & y-axis limits
    p <- p + aes(x = .data[[group.by]], y = .data[[var]])

    if (!is.null(y.breaks)) {
        p <- p + scale_y_continuous(breaks = y.breaks)
    }
    if (!is.na(min) || !is.na(max)) {
        p <- p + coord_cartesian(ylim=c(min,max))
    }

    # Add Plots
    for (i in seq_along(plots)) {
        if (plots[i] == "vlnplot") {
            p <- p + geom_violin(
                linewidth = vlnplot.lineweight,
                width = vlnplot.width,
                scale = vlnplot.scaling,
                draw_quantiles = vlnplot.quantiles,
                na.rm = TRUE)
        }

        if (plots[i] == "boxplot") {
            boxplot.args <- list(
                width = boxplot.width,
                color = boxplot.color,
                lwd = boxplot.lineweight,
                alpha = ifelse(boxplot.fill, 1, 0),
                position = position_dodge(width = boxplot.position.dodge),
                outlier.size = boxplot.outlier.size,
                na.rm = TRUE)
            if (is.na(boxplot.show.outliers)) {
                boxplot.show.outliers <- ifelse("jitter" %in% plots, FALSE, TRUE)
            }
            if (!boxplot.show.outliers) {
                boxplot.args$outlier.shape <- NA
            }
            p <- p + do.call(geom_boxplot, boxplot.args)
        }

        if (plots[i] == "jitter") {

            # Create geom_jitter() arguments
            jitter.args <- list(
                position = position_jitterdodge(
                    jitter.width = jitter.width,
                    jitter.height = 0,
                    dodge.width = jitter.position.dodge,
                    seed = NA
                ),
                size=jitter.size,
                color = jitter.color)

            geom_for_jitter <- geom_jitter
            if (do.raster) {
                .error_if_no_ggrastr()
                geom_for_jitter <- ggrastr::geom_jitter_rast
                jitter.args$raster.dpi <- raster.dpi
            }

            jitter.aes <- aes(group = .data[[group.aes]])
            if (do.hover) {
                jitter.aes <- modifyList(jitter.aes, aes(text = .data$hover.string))
            }

            # If shape.by given, use it. Else, shapes[1] which = dots (16) by default
            if (!is.null(shape.by)) {

                # Set shape in aes & set scales/theming.
                jitter.aes <- modifyList(jitter.aes, aes(shape = .data[[shape.by]]))

                p <- p + scale_shape_manual(
                    values = shape.panel[seq_along(colLevels(shape.by, data_frame, rownames(Target_data)))])

                if (!is.na(jitter.shape.legend.size)){
                    p <- p + guides(shape = guide_legend(
                        override.aes = list(size=jitter.shape.legend.size)))
                }
                if (jitter.shape.legend.show==FALSE){
                    p <- p + guides(shape = "none")
                }

            } else {
                # Set shape outside of aes
                jitter.args$shape <- shape.panel[1]
            }

            jitter.args$mapping <- jitter.aes

            if (do.hover) {
                p <- p + suppressWarnings(do.call(geom_for_jitter, jitter.args))
            } else {
                p <- p + do.call(geom_for_jitter, jitter.args)
            }
        }
    }

    # Add labels and, if requested, lines
    p <- p + xlab(xlab) + ylab(ylab)
    if (is.na(x.labels.rotate) || x.labels.rotate) {
        p <- p + theme(axis.text.x= element_text(angle=45, hjust = 1, vjust = 1))
    }

    p
}

#' @importFrom ggridges geom_density_ridges2
.yPlot_add_data_x_direction <- function(
    p, Target_data, var, group.by,
    plots, xlab, ylab, jitter.size, jitter.color,
    jitter.shape.legend.size, jitter.shape.legend.show,
    ridgeplot.lineweight, ridgeplot.scale,
    ridgeplot.ymax.expansion, ridgeplot.shape, ridgeplot.bins,
    ridgeplot.binwidth,
    x.labels.rotate, do.hover, color.panel, colors, y.breaks, min, max) {
    #This function takes in a partial yPlot ggplot object without any data overlay, and parses adding the main data visualizations.

    # Now that we know the plot's direction, set direction & "y"-axis limits
    p <- p + aes(x = .data[[var]], y = .data[[group.by]])

    if (!is.null(y.breaks)) {
        p <- p + scale_x_continuous(breaks = y.breaks)
    }
    if (!is.na(min) || !is.na(max)) {
        p <- p + coord_cartesian(xlim=c(min,max))
    }

    # For stylistic issues with plotting defaults, also adjust grouping-axis limits
    if (is.na(ridgeplot.ymax.expansion)) {
        num_groups <- length(unique(Target_data[,group.by]))
        # From 0.6 to 0.1 between 4 to 12 groups and 0.05 by 34 groups.
        set_exp <- stats::approxfun(
            x=c(3,12,34), y=c(0.6, 0.1, 0.05), yleft = 0.6, yright = 0.05)
        ridgeplot.ymax.expansion <- set_exp(num_groups)
    }
    p <- p + scale_color_manual(values=color.panel[colors]) +
        scale_y_discrete(expand = expansion(mult=c(0, ridgeplot.ymax.expansion)))

    # Add ridgeplot and jitter data
    ridge.args <- list(linewidth = ridgeplot.lineweight, scale = ridgeplot.scale)
    if (ridgeplot.shape == "hist") {
        ridge.args$stat <- "binline"
        ridge.args$bins <- ridgeplot.bins
        ridge.args$binwidth <- ridgeplot.binwidth
    }
    if ("jitter" %in% plots) {
        ridge.args <- c(ridge.args, jittered_points = TRUE,
            point_size = jitter.size, point_color = jitter.color)
    }

    p <- p + do.call(ggridges::geom_density_ridges2, ridge.args)

    if (!is.na(jitter.shape.legend.size)) {
        p <- p + guides(shape = guide_legend(override.aes = list(size=jitter.shape.legend.size)))
    }
    if (jitter.shape.legend.show==FALSE){
        p <- p + guides(shape = "none")
    }

    # Add labels and, if requested, lines
    p <- p + xlab(ylab) + ylab(xlab)
    if (!is.na(x.labels.rotate) && x.labels.rotate) {
        p <- p + theme(axis.text.y= element_text(angle=45, hjust = 1, vjust = 1))
    }

    p
}

#' @describeIn yPlot simple yPlot wrapper with distinct plots input defaults
#' @export
ridgePlot <- function(..., plots = c("ridgeplot")){ yPlot(..., plots = plots) }

#' @describeIn yPlot simple yPlot wrapper with distinct plots input defaults
#' @export
ridgeJitter <- function(..., plots = c("ridgeplot", "jitter")){ yPlot(..., plots = plots) }

#' @describeIn yPlot simple yPlot wrapper with distinct plots input defaults
#' @export
boxPlot <- function(..., plots = c("boxplot","jitter")){ yPlot(..., plots = plots) }


.warn_or_apply_plotly <- function(p, plots) {
    if ("ridgeplot" %in% plots) {
        warning("'do.hover = TRUE' request ignored because plotly does not support ridgeplots.")
    } else {
        .error_if_no_plotly()
        # Add hover.text to jitter, else just convert.
        if ("jitter" %in% plots) {
            p <- plotly::ggplotly(p, tooltip = "text")
        } else {
            p <- plotly::ggplotly(p)
        }
    }
    p
}
