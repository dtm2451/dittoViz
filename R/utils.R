#' @importFrom utils modifyList
#' @importFrom stats median
NULL

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
