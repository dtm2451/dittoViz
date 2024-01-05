#' Example Data Generation
#' @name dittoExampleData
#' @details This documentation point exists only to be a set source of example data for other dittoViz documentation.
#' Running the examples section code creates a data.frame called 'example_df' containing data of various types.
#' These data are randomly generated each time and simulate what a user might use as the 'data_frame' input of dittoViz visualization functions.
#' @return Running \code{example("dittoExampleData")} creates a data.frame called example_df.
#' @author Daniel Bunis
#' @examples
#' # Generate some random data
#' nobs <- 120
#'
#' # Fake "PCA" that we'll based some other attributes on
#' example_pca <- matrix(rnorm(nobs*2), nobs)
#'
#' example_df <- data.frame(
#'         conditions = factor(rep(c("condition1", "condition2"), each=nobs/2)),
#'         timepoint = rep(c("d0", "d3", "d6", "d9"), each = nobs/4),
#'         SNP = rep(c(rep(TRUE,7),rep(FALSE,8)), nobs/15),
#'         groups = sample(c("A","B","C","D"), nobs, TRUE),
#'         score = seq_len(nobs)/2,
#'         gene1 = log2(rpois(nobs, 5) +1),
#'         gene2 = log2(rpois(nobs, 30) +1),
#'         gene3 = log2(rpois(nobs, 4) +1),
#'         gene4 = log2(rpois(nobs, 2) +1),
#'         gene5 = log2(rpois(nobs, 17) +1),
#'         PC1 = example_pca[,1],
#'         PC2 = example_pca[,2],
#'         clustering = as.character(1*(example_pca[,1]>0&example_pca[,2]>0) +
#'                        2*(example_pca[,1]<0&example_pca[,2]>0) +
#'                        3*(example_pca[,1]>0&example_pca[,2]<0) +
#'                        4*(example_pca[,1]<0&example_pca[,2]<0)),
#'         sample = rep(1:12, each = nobs/12),
#'         category = rep(c("A", "B"), each = nobs/2),
#'         subcategory = rep(as.character(rep(1:3,4)), each = nobs/12),
#'         row.names = paste0("obs", 1:nobs)
#'         )
#'
#' # cleanup
#' rm(example_pca, nobs)
#'
#' summary(example_df)
NULL

.msg_if <- function(verbose, ...){
    if (verbose) {
        message(...)
    }
}

.error_if_no_plotly <- function() {
    if (!requireNamespace("plotly", quietly = TRUE)) {
        stop("plotly installation required for using hover")
    }
}

.error_if_no_ggrastr <- function() {
    if (!requireNamespace("ggrastr", quietly = TRUE)) {
        stop("ggrastr installation required for using rasterization with dittoScatterPlot plotters.")
    }
}

.error_if_no_ggplot.multistats <- function() {
    if (!requireNamespace("ggplot.multistats", quietly = TRUE)) {
        stop("ggplot.multistats installation required for supplying 'color.by' to dittoHex plotters.")
    }
}

.leave_default_or_null <- function(
    target, default, null.if = FALSE, default.when = "make") {
    # Handles much of dittoViz's defaulting process
    # Takes in 'target' and outputs:
    #  - 'default' string when 'target' == 'default.when'
    #  - NULL when logical provided to 'null.if' is TRUE.
    #  - 'target' otherwise
    if (!is.null(target)) {
        if (target==default.when) {
            if (null.if) {
                target <- NULL
            } else {
                target <- default
            }
        }
    }
    target
}

.all_rows <- function(df) {
    rownames(df)
}

.which_rows <- function(rows.use, df) {
    # converts a 'rows.use' given as string, logical, or numeric vector
    # into the string vector format expected internally by dittoViz functions
    all.rows <- rownames(df)

    if (is.null(rows.use)) {
        # Returns all rows when 'rows.use' is NULL
        return(all.rows)
    }

    if (is.logical(rows.use)) {
        if (length(rows.use)!=length(all.rows)) {
            stop("'rows.use' length must equal the number of rows in 'data_frame' when given in logical form")
        }
        return(all.rows[rows.use])
    }

    if (is.numeric(rows.use)) {
        return(all.rows[rows.use])
    }

    rows.use
}

.make_hover_strings_from_df <- function(df){
    # Creates a single character vector where each element is the hoverstring
    # for a given row of the provided 'df' with structure
    # "col1name: col1-value\ncol2name: var2-value\ncol3name: var3-value\n..."
    vapply(
        seq_len(nrow(df)),
        function(row){
            paste(as.character(vapply(
                seq_len(ncol(df)),
                function(col){
                    paste0(names(df)[col],": ",df[row,col])
                }, FUN.VALUE = character(1))
            ),collapse = "\n")
        }, FUN.VALUE = character(1))
}

.rename_and_or_reorder <- function(orig.data, reorder = NULL, relabels = NULL) {
    # Takes in string vector or factor 'orig.data', integer vector 'reorder',
    # and string vector 'relabels'.
    # Turns character vectors into factors
    # Reorders the level of the factor based on indices provided to 'reorder'
    # Re-labels the levels of the factor based on labels provided to 'relabels'
    if (is.numeric(orig.data)) {
        return(orig.data)
    }
    rename.args <- list(x = orig.data)
    if (!(is.null(reorder))) {
        if (length(reorder)!=length(levels(factor(orig.data)))) {
            stop("incorrect number of indices provided to 'reorder' input")
        }
        rename.args$levels <- levels(factor(orig.data))[reorder]
    }
    if (!(is.null(relabels))) {
        if (length(relabels)!=length(levels(factor(orig.data)))) {
            stop("incorrect number of labels provided to 'relabel' input")
        }
        rename.args$labels <- relabels
    }
    do.call(factor, args = rename.args)
}

.multivar_adjust_split_by <- function(
    split.by, multivar.split.dir, multivar.col.vars,
    multivar.aes
) {

    if (multivar.aes != "split") {
        return(split.by)
    }

    if (is.null(split.by)) {
        split.by <- multivar.col.vars
    } else {
        if (length(split.by)>1) {
            warning(
                "Multi-feature display is prioiritized for faceting;",
                "'split.by' element, '",
                split.by[2],
                "', will be ignored.")
        }
        split.by[2] <- multivar.col.vars
        if (multivar.split.dir=="row") {
            split.by <- rev(split.by)
        }
    }
    split.by
}

.multi_var_restructure <- function(
        data_frame, vars, multivar.col.data, multivar.col.vars,
        adjustment, adj.fxn,
        rows.use,
        multivar.split.dir,
        split.by,
        multivar.aes = "split"
) {

    each_data <- lapply(
        vars, function(this_col) {
            this.out <- data_frame
            this.out[, multivar.col.data] <- ._col(this_col, data_frame, adjustment, adj.fxn)
            this.out[, multivar.col.vars] <- this_col
            list(
                data_use = this.out[rows.use,],
                data_other = this.out[!(rownames(this.out) %in% rows.use),]
            )
        }
    )

    if (any(unlist(lapply(each_data, function(x) { !is.numeric(x$data_use[, multivar.col.data]) })))) {
        stop("Only numeric columns are currently supported for plotting multiple data columns with the same aesthetic")
    }

    list(
        data_use = do.call(rbind, lapply(each_data, function(x) x$data_use)),
        data_other = do.call(rbind, lapply(each_data, function(x) x$data_other)),
        split.by = .multivar_adjust_split_by(
            split.by, multivar.split.dir, multivar.col.vars,
            multivar.aes
        )
    )
}

.data_adjust_scatter <- function(
    data_frame, x.by, y.by, color.by, shape.by, split.by,
    x.adjustment, y.adjustment, color.adjustment,
    x.adj.fxn, y.adj.fxn, color.adj.fxn,
    color.renames, shape.renames,
    multivar.split.dir, rows.use, do.hover, hover.data
) {
    ### Make dataframe edits while collecting col names to actually use
    cols_use <- list(
        x.by = x.by, y.by = y.by,
        color.by = color.by, shape.by = shape.by,
        split.by = split.by
    )
    if (identical(shape.by, NA)) {
        cols_use$shape.by <- NULL
    }
    # X/Y adjustments
    if (!is.null(x.adjustment) || !is.null(x.adj.fxn)) {
        cols_use$x.by <- paste0(x.by, ".x.adj")
        data_frame[,cols_use$x.by] <-
            ._col(x.by, data_frame, x.adjustment, x.adj.fxn)
    }
    if (!is.null(y.adjustment) || !is.null(y.adj.fxn)) {
        cols_use$y.by <- paste0(y.by, ".y.adj")
        data_frame[,cols_use$y.by] <-
            ._col(y.by, data_frame, y.adjustment, y.adj.fxn)
    }
    # color adjustment, color relabels, multi-color & rows.use splitting
    if (!is.null(color.by)) {
        if (length(color.by) == 1) {
            if (!is.null(color.adjustment) || !is.null(color.adj.fxn)) {
                cols_use$color.by <- paste0(color.by, ".color.adj")
                data_frame[,cols_use$color.by] <-
                    ._col(color.by, data_frame, color.adjustment, color.adj.fxn)
            }
            data_frame[,cols_use$color.by] <- .rename_and_or_reorder(
                data_frame[,cols_use$color.by],
                reorder = NULL, relabels = color.renames)
            data_use <- data_frame[rows.use,]
            data_other <- data_frame[!(rownames(data_frame) %in% rows.use),]
        } else {
            # (Only numeric data supported)
            multi_out <- .multi_var_restructure(
                data_frame, color.by, "color.multi", "color.which",
                color.adjustment, color.adj.fxn, rows.use,
                multivar.split.dir, split.by, "split"
            )
            data_use <- multi_out$data_use
            data_other <- multi_out$data_other
            cols_use$split.by <- multi_out$split.by
            cols_use$color.by <- "color.multi"
        }
    } else {
        data_use <- data_frame[rows.use,]
        data_other <- data_frame[!(rownames(data_frame) %in% rows.use),]
    }
    # Hover Prep
    if (do.hover) {
        data_use$hover.string <- .make_hover_strings_from_df(
            data_use[,hover.data,drop=FALSE])
        cols_use$hover.text <- "hover.string"
    }

    list(
        data_use = data_use,
        data_other = data_other,
        cols_use = cols_use)
}
