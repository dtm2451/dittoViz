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
        stop("ggplot.multistats installation required for supplying 'color.var' to dittoHex plotters.")
    }
}

.leave_default_or_null <- function(
    target, default, null.if = FALSE, default.when = "make") {
    # Handles much of dittoSeq's titles defaulting process
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
    # Re-labels the levels of the factor based on lebels provided to 'relabels'
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
