#### .isCol: Is this the name of a meta.data slot in my dataset? ####
#' Tests if an input is the name of a meta.data slot in a target data_frame.
#' @noRd
#' @param test String or vector of strings, the "potential.metadata.name"(s) to check for.
#' @param data_frame A data.frame.
#' @param return.values Logical which sets whether the function returns a logical \code{TRUE}/\code{FALSE} versus the \code{TRUE} \code{test} values . Default = \code{FALSE}
#' @return Returns a logical or logical vector indicating whether each instance in \code{test} is a column of \code{data_frame}.
#' Alternatively, returns the values of \code{test} that were indeed metadata slots if \code{return.values = TRUE}.
#' @details
#' For Seurat data_frames, also returns TRUE for the input \code{"ident"} because, for all dittoSeq visualiztions, \code{"ident"} will retrieve a Seurat data_frames' clustering slot.
#'
#' @seealso
#' \code{\link{getMetas}} for returning all metadata slots of an \code{data_frame}
#'
#' \code{\link{meta}} for obtaining the contants of metadata slots
#'
#' @examples
#' example(importDittoBulk, echo = FALSE)
#'
#' # To check if something is a metadata slot
#' .isCol("timepoint", data_frame = myRNA) # FTRUE
#' .isCol("nCount_RNA", data_frame = myRNA) # FALSE
#'
#' # To test if many things are metadata of an data_frame
#' .isCol(c("age","groups"), myRNA) # FALSE, TRUE
#'
#' # 'return.values' input is especially useful in these cases.
#' .isCol(c("age","groups"), myRNA,
#'     return.values = TRUE)
#'
#' @author Daniel Bunis
#' @export
#' @import ggplot2

.isCol <- function(test, data_frame, return.values=FALSE){
    if (return.values) {
        return(test[.isCol(test, data_frame, return.values=FALSE)])
    } else {
        cols <- colnames(data_frame)
        return(test %in% cols)
    }
}

#### ._col: for extracting the values of a particular column, and ensuring it's names are the rownames of the data_frame ####
#' Returns the values of a column, where values are named by the rownames of data_frame
#'
#' @noRd
#' @param col String, the name of the column to grab.
#' @param data_frame A data.frame.
#' @param adjustment A recognized string indicating whether numeric metadata should be used directly (default) versus adjusted to be
#' \itemize{
#' \item{"z-score": scaled with the scale() function to produce a relative-to-mean z-score representation}
#' \item{"relative.to.max": divided by the maximum expression value to give percent of max values between [0,1]}
#' }
#'
#' Ignored if the target metadata is not numeric.
#' @param adj.fxn A function which takes a vector (of metadata values) and returns a vector of the same length.
#'
#' For example, \code{function(x) \{log2(x)\}} or \code{as.factor}
#' @param rows.use String vector of rownames names OR an integer vector specifying the row-indices of data points which should be targetted.
#'
#' Alternatively, a Logical vector, the same length as the number of cells in the object, which sets which cells to include.
#' @return A named vector.
#' @details
#' Retrieves the values of a metadata slot from \code{object}, or the clustering slot if \code{meta = "ident"} and the \code{object} is a Seurat.
#'
#' If \code{adjustment} or \code{adj.fxn} are provided, then these requested adjustments are applied to these values (\code{adjustment} first).
#' Note: Alterations via \code{adjustment} are only applied when metadata is numeric, but \code{adj.fxn} alterations are applied to metadata of any type.
#'
#' Lastly, outputs these values are named as the cells'/samples' names.
#' @seealso
#' \code{\link{metaLevels}} for returning just the unique discrete identities that exist within a metadata slot
#'
#' \code{\link{getMetas}} for returning all metadata slots of an \code{object}
#'
#' \code{\link{.isCol}} for testing whether something is the name of a metadata slot
#' @examples
#' example(importDittoBulk, echo = FALSE)
#' ._col("groups", object = myRNA)
#'
#' myRNA$numbers <- seq_len(ncol(myRNA))
#' ._col("numbers", myRNA, adjustment = "z-score")
#' ._col("numbers", myRNA, adj.fxn = as.factor)
#' ._col("numbers", myRNA, adj.fxn = function(x) \{log2(x)\})
#'
#' @author Daniel Bunis
#' @export

._col <- function(col, data_frame,
                 adjustment = NULL, adj.fxn = NULL,
                 rows.use = NULL) {

    if (!.isCol(col, data_frame)) {
        stop(dQuote(col)," is not a column of 'data_frame'")
    }

    # Trim by rows.use
    if (!is.null(rows.use)) {
        rows.use <- .which_rows(rows.use, data_frame)
        data_frame <- data_frame[rows.use,]
    }

    # Retrieve target columns's values
    values <- data_frame[, col, drop = TRUE]

    # Add 'string' adjustments
    if (is.numeric(values)) {

        if (!is.null(adjustment) && !is.na(adjustment)) {
            if (adjustment=="z-score") {
                values <- as.numeric(scale(values))
            }
            if (adjustment=="relative.to.max") {
                values <- values/max(values)
            }
        }
    }

    # Apply adj.fxn
    if (!is.null(adj.fxn)) {
        values <- adj.fxn(values)
    }

    # Add names
    names(values) <- .all_rows(data_frame)

    values
}

#' Gives the distinct values of a meta.data slot (or ident)
#'
#' @noRd
#' @param col quoted column name name. the data column whose potential values should be retrieved.
#' @param data_frame A data.frame.
#' @param used.only TRUE by default, for target metadata that are factors, whether levels nonexistent in the target data should be ignored.
#' @param rows.use String vector of cells'/samples' names OR an integer vector specifying the indices of cells/samples which should be included.
#'
#' Alternatively, a Logical vector, the same length as the number of cells in the object, which sets which cells to include.
#' @return String vector, the distinct values of a metadata slot (factor or not) among all cells/samples, or for a subset of cells/samples.
#' (Alternatively, returns the distinct values of clustering if \code{meta = "ident"} and the object is a \code{Seurat} object).
#' @seealso
#' \code{\link{meta}} for returning an entire metadata slots of an \code{object}, not just the potential levels
#'
#' \code{\link{getMetas}} for returning all metadata slots of an \code{object}
#'
#' \code{\link{.isCol}} for testing whether something is the name of a metadata slot
#' @examples
#' example(importDittoBulk, echo = FALSE)
#'
#' metaLevels("clustering", object = myRNA)
#'
#' # Note: Set 'used.only' (default = TRUE) to FALSE to show unused levels
#' #  of metadata that are already factors.  By default, only the in use options
#' #  of a metadata are shown.
#' metaLevels("clustering", myRNA,
#'     used.only = FALSE)
#'
#' @author Daniel Bunis
#' @export

.colLevels <- function(col, data_frame, rows.use = NULL, used.only = TRUE){
    if (!.isCol(col, data_frame)) {
        stop(dQuote(col)," is not a column of 'data_frame'")
    }
    if (used.only) {
        values <- as.character(._col(col, data_frame))
    } else {
        values <- ._col(col, data_frame)
    }
    if (!is.null(rows.use)) {
        all.rows <- rownames(data_frame)
        rows.use <- .which_rows(rows.use, data_frame)
        values <- values[all.rows %in% rows.use]
    }
    levels(as.factor(values))
}
