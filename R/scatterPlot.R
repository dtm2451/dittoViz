#' Show RNAseq data overlayed on a scatter plot
#' @import ggplot2
#'
#' @param data_frame A data_frame where columns are features and rows are observations you wish to visualize.
#' @param x.by,y.by Single strings denoting the name of a column of \code{data_frame} to use for the x- and y-axis of the scatterplot.
#' Data Restriction: Must be continuous.
#' @param color.by Single string denoting the name of a column of \code{data_frame} to use for setting the color plotted points.
#' @param shape.by Single string denoting the name of a column of \code{data_frame} to use for setting the shape plotted points.
#' Data Restriction: Must be continuous.
#' @param split.by 1 or 2 strings naming denoting the name(s) of column(s) to use for faceting / separating data points into separate plots.
#' Data Restriction: Must be continuous.
#'
#' When 2 columns are named, c(row,col), the first is used as rows and the second is used for columns of the resulting grid.
#'
#' When 1 column is named, shape control can be achieved with \code{split.nrow} and \code{split.ncol}
#'
#' @param split.nrow,split.ncol Integers which set the dimensions of faceting/splitting when a single column is given to \code{split.by}.
#' @param split.show.all.others Logical which sets whether gray "others" cells of facets should include all cells of other facets (\code{TRUE}) versus just cells left out by \code{cell.use} (\code{FALSE}).
#' @param split.adjust A named list which allows extra parameters to be pushed through to the faceting function call.
#' List elements should be valid inputs to the faceting functions, e.g. `list(scales = "free")`.
#'
#' For options, when giving 1 metadata to \code{split.by}, see \code{\link[ggplot2]{facet_wrap}},
#' OR when giving 2 metadatas to \code{split.by}, see \code{\link[ggplot2]{facet_grid}}.
#' @param rows.use String vector of rownames names OR an integer vector specifying the row-indices of data points which should be targetted.
#'
#' Alternatively, a Logical vector, the same length as the number of cells in the object, which sets which cells to include.
#' @param show.others Logical. FALSE by default, whether cells not targetted by \code{rows.use} should be shown in the background, in light gray.
#' @param size Number which sets the size of data points. Default = 1.
#' @param color.panel String vector which sets the colors to draw from when \code{color.by} indicates discrete data.
#' \code{dittoColors()} by default, see \code{\link{dittoColors}} for contents.
#'
#' A named vector can be used if names are matched to the distinct values of the \code{color.by} data.
#' @param colors Integer vector, the indexes / order, of colors from \code{color.panel} to actually use.
#'
#' Useful for quickly swapping around colors of the default set.
#' @param do.hover Logical which controls whether the ggplot output will be converted to a plotly object so that data about individual points can be displayed when you hover your cursor over them.
#' The \code{hover.data} argument is used to determine what data to use.
#' @param hover.data String vector of column names of \code{data_frame} which denotes what data to show on hover when \code{do.hover} is set to \code{TRUE}.
#' @param shape.panel Vector of integers, corresponding to ggplot shapes, which sets what shapes with \code{shape.by}.
#' When nothing is supplied to \code{shape.by}, only the first value is used.
#' Default is a set of 6, \code{c(16,15,17,23,25,8)}, the first being a simple, solid, circle.
#' @param size Number which sets the size of data points. Default = 1.
#' @param opacity Number between 0 and 1.
#' 1 = opaque. 0 = invisible. Default = 1.
#' (In terms of typical ggplot variables, = alpha)
#' @param do.ellipse Logical. Whether the groups should be surrounded by median-centered ellipses.
#' @param do.label  Logical. Whether to add text labels near the center (median) of clusters for grouping vars.
#' @param labels.size Size of the the labels text
#' @param labels.highlight Logical. Whether the labels should have a box behind them
#' @param labels.repel Logical, that sets whether the labels' placements will be adjusted with \link{ggrepel} to avoid intersections between labels and plot bounds.
#' TRUE by default.
#' @param labels.split.by String of one or two metadata names which controls the facet-split calculations for label placements.
#' Defaults to \code{split.by}, so generally there is no need to adjust this except when you are utilizing the \code{extra.vars} input to achieve manual faceting control.
#' @param rename.color.groups String vector which sets new names for the identities of \code{var} groups.
#' @param rename.shape.groups String vector which sets new names for the identities of \code{shape.by} groups.
#' @param legend.show Logical. Whether any legend should be displayed. Default = \code{TRUE}.
#' @param legend.color.title,legend.shape.title Strings which set the title for the color or shape legends.
#' @param legend.color.size,legend.shape.size Numbers representing the size at which shapes should be plotted in the color and shape legends (for discrete variable plotting).
#' Default = 5. *Enlarging the icons in the colors legend is incredibly helpful for making colors more distinguishable by color blind individuals.
#' @param min.color color for \code{min} value of \code{color.by} data. Default = yellow
#' @param max.color color for \code{max} value of \code{color.by} data. Default = blue
#' @param min.value,max.value Number which sets the \code{color.by}-data value associated with the minimum or maximum colors.
#' @param legend.color.breaks Numeric vector which sets the discrete values to label in the color-scale legend for continuous data.
#' @param legend.color.breaks.labels String vector, with same length as \code{legend.breaks}, which sets the labels for the tick marks of the color-scale.
#' @param main String, sets the plot title.
#' A default title is automatically generated based on \code{color.by} and \code{shape.by} when either are provided.
#' To remove, set to \code{NULL}.
#' @param sub String, sets the plot subtitle.
#' @param xlab,ylab Strings which set the labels for the axes. To remove, set to \code{NULL}.
#' @param do.letter Logical which sets whether letters should be added on top of the colored dots. For extended colorblindness compatibility.
#' NOTE: \code{do.letter} is ignored if \code{do.hover = TRUE} or \code{shape.by} is provided a metadata because
#' lettering is incompatible with plotly and with changing the dots' to be different shapes.
#' @param do.contour Logical. Whether density-based contours should be displayed.
#' @param contour.color String that sets the color(s) of the \code{do.contour} contours.
#' @param contour.linetype String or numeric which sets the type of line used for \code{do.contour} contours.
#' Defaults to "solid", but see \code{\link[ggplot2]{linetype}} for other options.
#' @param add.trajectory.by.groups List of vectors representing trajectory paths, each from start-group to end-group, where vector contents are the group-names indicated by the \code{trajectory.group.by} column of \code{data_frame}.
#' @param trajectory.group.by String denoting the name of a column of \code{data_frame} to use for generating trajectories from data point groups.
#' @param trajectory.arrow.size Number representing the size of trajectory arrows, in inches.  Default = 0.15.
#' @param add.trajectory.curves List of matrices, each representing coordinates for a trajectory path, from start to end, where matrix columns represent x and y coordinates of the paths.
#' @param theme A ggplot theme which will be applied before dittoSeq adjustments.
#' Default = \code{theme_bw()}.
#' See \url{https://ggplot2.tidyverse.org/reference/ggtheme.html} for other options and ideas.
#' @param plot.order String. If the data should be plotted based on the order of the color data, sets whether to plot in "increasing" or "decreasing" order.
#' @param show.grid.lines Logical which sets whether gridlines of the plot should be shown.
#' @param do.raster Logical. When set to \code{TRUE}, rasterizes the internal plot layer, changing it from individually encoded points to a flattened set of pixels.
#' This can be useful for editing in external programs (e.g. Illustrator) when there are many thousands of data points.
#' @param raster.dpi Number indicating dots/pixels per inch (dpi) to use for rasterization. Default = 300.
#' @param data.out Logical. When set to \code{TRUE}, changes the output, from the plot alone, to a list containing the plot ("p"),
#' a data.frame containing the underlying data for target cells ("Target_data"),
#' and a data.frame containing the underlying data for non-target cells ("Others_data").
#'
#' @return a ggplot scatterplot where colored dots and/or shapes represent individual cells/samples. X and Y axes can be gene expression, numeric metadata, or manually supplied values.
#'
#' Alternatively, if \code{data.out=TRUE}, a list containing three slots is output: the plot (named 'p'), a data.table containing the underlying data for target cells (named 'Target_data'), and a data.table containing the underlying data for non-target cells (named 'Others_data').
#'
#' Alternatively, if \code{do.hover} is set to \code{TRUE}, the plot is coverted from ggplot to plotly &
#' cell/sample information, determined by the \code{hover.data} input, is retrieved, added to the dataframe, and displayed upon hovering the cursor over the plot.
#'
#' @details
#' This function creates a dataframe with X, Y, color, shape, and faceting data determined by \code{x.by}, \code{y.by}, \code{color.by}, \code{shape.var}, and \code{split.by}.
#' Any extra gene or metadata requested with \code{extra.var} is added as well.
#' For expression/counts data, \code{assay}, \code{slot}, and \code{adjustment} inputs (\code{.x}, \code{.y}, and \code{.color}) can be used to change which data is used, and if it should be adjusted in some way.
#'
#' Next, if a set of cells or samples to use is indicated with the \code{cells.use} input, then the dataframe is split into \code{Target_data} and \code{Others_data} based on subsetting by the target cells/samples.
#'
#' Finally, a scatter plot is created using these dataframes.
#' Non-target cells are colored in gray if \code{show.others=TRUE},
#' and target cell data is displayed on top, colored and shaped based on the \code{color.by}- and \code{shape.by}-associated data.
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
#' @author Daniel Bunis
#' @export
#' @examples
#' library(dittoSeq)
#' example(importDittoBulk, echo = FALSE)
#' myRNA
#'
#' #  == the default way to extract
#' myRNA$nCount_RNA <- runif(60,200,1000)
#' myRNA$nFeature_RNA <- myRNA$nCount_RNA*runif(60,0.95,1.05)
#' # and also percent.mito metadata
#' myRNA$percent.mito <- sample(c(runif(50,0,0.05),runif(10,0.05,0.2)))
#'
#' df <- as.data.frame(colData(myRNA))
#' df$gene1 <- dittoSeq::gene("gene1", myRNA)
#' df$gene2 <- dittoSeq::gene("gene2", myRNA)
#' df$gene3 <- dittoSeq::gene("gene3", myRNA)
#' df$pca1 <- dittoSeq:::.extract_Reduced_Dim("pca", 1, myRNA)$embeddings
#' df$pca2 <- dittoSeq:::.extract_Reduced_Dim("pca", 2, myRNA)$embeddings
#'
#' scatterPlot(
#'     df, x.by = "nCount_RNA", y.by = "nFeature_RNA")
#'
#' # Shapes or colors can be overlaid representing discrete metadata
#' #   or (only colors) continuous metadata / expression data by providing
#' #   metadata or gene names to 'color.by' and 'shape.by'
#' scatterPlot(
#'     df, x.by = "gene1", y.by = "gene2",
#'     color.by = "groups",
#'     shape.by = "SNP",
#'     size = 3)
#'
#' # Data can be "split" or faceted by a discrete variable as well.
#' scatterPlot(df, x.by = "gene1", y.by = "gene2",
#'     split.by = "timepoint") # single split.by element
#' scatterPlot(df, x.by = "gene1", y.by = "gene2",
#'     split.by = c("groups","SNP")) # row and col split.by elements
#' # OR with 'extra.vars' plus manually faceting for added control
#' scatterPlot(df, x.by = "gene1", y.by = "gene2") +
#'     facet_wrap("SNP", ncol = 1, strip.position = "left")
#'
#' # Countours can also be added to help illumunate overlapping samples
#' scatterPlot(df, x.by = "gene1", y.by = "gene2",
#'     do.contour = TRUE)
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
    color.adjustment = NULL,
    color.adj.fxn = NULL,
    split.show.all.others = TRUE,
    opacity = 1,
    color.panel = dittoColors(),
    colors = seq_along(color.panel),
    split.nrow = NULL,
    split.ncol = NULL,
    split.adjust = list(),
    shape.panel = c(16,15,17,23,25,8),
    rename.color.groups = NULL,
    rename.shape.groups = NULL,
    min.color = "#F0E442",
    max.color = "#0072B2",
    min.value = NULL,
    max.value = NULL,
    plot.order = c("unordered", "increasing", "decreasing"),
    xlab = x.by,
    ylab = y.by,
    main = "make",
    sub = NULL,
    theme = theme_bw(),
    do.hover = FALSE,
    hover.data = unique(c(color.by, paste0(color.by,"-adj"), shape.by, x.by, y.by)),
    do.contour = FALSE,
    contour.color = "black",
    contour.linetype = 1,
    add.trajectory.by.groups = NULL,
    add.trajectory.curves = NULL,
    trajectory.group.by,
    trajectory.arrow.size = 0.15,
    do.letter = FALSE,
    do.ellipse = FALSE,
    do.label = FALSE,
    labels.size = 5,
    labels.highlight = TRUE,
    labels.repel = TRUE,
    labels.split.by = split.by,
    legend.show = TRUE,
    legend.color.title = color.by,
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

    # Standardize rows vectors.
    rows.use <- .which_rows(rows.use, data_frame)
    all.rows <- .all_rows(data_frame)

    ### Make dataframe edits
    # Adjustments
    if (!is.null(color.adjustment) || !is.null(color.adj.fxn)) {
        new.color.by <- paste0(color.by, "-adj")
        data_frame[,new.color.by] <-
            ._col(color.by, data_frame, color.adjustment, color.adj.fxn)
        color.by <- new.color.by
    }
    # Relabels/reorders
    if (!is.null(color.by)) {
        data_frame[,color.by] <- .rename_and_or_reorder(
            data_frame[,color.by], reorder = NULL, relabels = rename.color.groups)
    }
    if (!is.null(shape.by)) {
        data_frame[,shape.by] <- .rename_and_or_reorder(
            data_frame[,shape.by], reorder = NULL, relabels = rename.shape.groups)
    }
    # Hover prep
    if (do.hover) {
        data_frame$hover.string <- .make_hover_strings_from_df(
            data_frame[,hover.data,drop=FALSE])
    }

    # Trim by rows.use, then order if wanted
    Target_data <- data_frame[rows.use,]
    Others_data <- data_frame[!(all.rows %in% rows.use),]

    if (plot.order != "unordered") {
        decreasing <- switch(plot.order,
                             "decreasing" = TRUE,
                             "increasing" = FALSE)
        Target_data <- Target_data[order(Target_data[,color.by], decreasing = decreasing),]
    }

    # Set title if "make"
    main <- .leave_default_or_null(
        main, paste0(c(color.by, shape.by), collapse = " and "))

    # Make the plot
    p <- .scatter_plot(
        Target_data, Others_data, x.by, y.by,
        color.by, shape.by, show.others, size, opacity,
        color.panel, colors, do.hover, shape.panel,
        min.color, max.color, min.value, max.value,
        xlab, ylab, main, sub, theme,
        legend.show, legend.color.title, legend.color.size,
        legend.color.breaks, legend.color.breaks.labels, legend.shape.title,
        legend.shape.size, do.raster, raster.dpi,
        split.by, split.show.all.others, show.grid.lines)

    ### Add extra features
    if (!is.null(split.by)) {
        p <- .add_splitting(
            p, split.by, split.nrow, split.ncol, split.adjust)
    }

    if (do.contour) {
        p <- .add_contours(p, Target_data, x.by, y.by, contour.color, contour.linetype)
    }

    p <- .add_letters_ellipses_labels_if_discrete(
        p, Target_data, x.by, y.by, color.by,
        is.discrete = !is.numeric(Target_data[color.by]),
        do.letter, do.ellipse, do.label,
        labels.highlight, labels.size, labels.repel, labels.split.by,
        size, opacity, legend.color.title, legend.color.size)

    if (is.list(add.trajectory.by.groups)) {
        p <- .add_trajectories_by_groups(
            p, data_frame, add.trajectory.by.groups,
            trajectory.group.by, trajectory.arrow.size, data_frame)
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
            Others_data = Others_data)
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
    p <- ggplot() + ylab(ylab) + xlab(xlab) + ggtitle(main,sub) + theme
    if (!show.grid.lines) {
        theme <- theme + theme(
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank())
    }

    # Determine how to add data while adding proper theming
    aes.args <- list(x = x.by, y = y.by)
    geom.args <- list(data = Target_data, alpha = opacity)

    if (is.character(size)) {
        aes.args$size <- size
    } else {
        geom.args$size <- size
    }

    if (!is.null(color.by)) {

        aes.args$color = color.by

        if (is.numeric(Target_data[,color.by])) {
            p <- p +
                scale_colour_gradient(
                    name = legend.color.title, low= min.color, high = max.color,
                    limits = c(
                        ifelse(is.null(min.value), min(Target_data[,color.by]), min.value),
                        ifelse(is.null(max.value), max(Target_data[,color.by]), max.value)),
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

        aes.args$shape = shape.by

        p <- p +
            scale_shape_manual(
                values = shape.panel[seq_along(levels(Target_data[,shape.by]))],
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
                    data = Others_data, aes_string(x = x.by, y = y.by),
                    size=size, color = "gray90", raster.dpi = raster.dpi)
            } else {
                p <- p + geom_point(
                    data = Others_data, aes_string(x = x.by, y = y.by),
                    size=size, color = "gray90")
            }
        }
    }
    # Target_data
    if (do.hover) {
        aes.args$text = "hover.string"
        geom.args$mapping <- do.call(aes_string, aes.args)
        p <- p + suppressWarnings(do.call(geom_point, geom.args))
    } else {
        geom.args$mapping <- do.call(aes_string, aes.args)
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

    all_data <- rbind(Target_data, Others_data)

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
