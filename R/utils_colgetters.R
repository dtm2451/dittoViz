#' Tests if the test input is the name of a column in the target data_frame.
#' @noRd
#' @param test String or vector of strings, the "potential.column.name"(s) to check.
#' @param data_frame A data.frame.
#' @param return.values Logical which sets whether the function returns a logical \code{TRUE}/\code{FALSE} versus the \code{TRUE} \code{test} values . Default = \code{FALSE}
#' @return Returns a logical or logical vector indicating whether each instance in \code{test} is a column of \code{data_frame}.
#' Alternatively, returns the values of \code{test} that were indeed columns if \code{return.values = TRUE}.
#'
#' @author Daniel Bunis
.is_col <- function(test, data_frame, return.values=FALSE){
    if (return.values) {
        return(test[.is_col(test, data_frame, return.values=FALSE)])
    } else {
        cols <- colnames(data_frame)
        return(test %in% cols)
    }
}

#' Returns the values of a column, where values are named by the rownames of data_frame
#'
#' @noRd
#' @param col String, the name of the column to target.
#' @param data_frame A data.frame.
#' @param adjustment A recognized string indicating whether numeric data should be used directly (default) versus adjusted to be
#' \itemize{
#' \item{"z-score": scaled with the scale() function to produce a relative-to-mean z-score representation}
#' \item{"relative.to.max": divided by the maximum expression value to give percent of max values between [0,1]}
#' }
#'
#' Ignored if the target data is not numeric as these known adjustments target numeric data only.
#' @param adj.fxn A function which takes a vector of values and returns a vector of values of the same length.
#'
#' For example, \code{function(x) \{log2(x)\}} or \code{as.factor}
#' @return A named vector.
#' @details
#' Retrieves the values of \code{data_frame[,col]}
#'
#' If \code{adjustment} or \code{adj.fxn} are provided, then these requested adjustments are applied to these values (\code{adjustment} first).
#' Note: Alterations via \code{adjustment} are only applied when the data is numeric, but \code{adj.fxn} alterations are applied to data of any type.
#'
#' Lastly, the values are named as the rownames of data_frame, then output.
#' @author Daniel Bunis
._col <- function(
    col, data_frame,
    adjustment = NULL, adj.fxn = NULL,
    add.names = TRUE
) {

    if (!.is_col(col, data_frame)) {
        stop(dQuote(col)," is not a column of 'data_frame'")
    }

    # Retrieve target columns's values
    values <- data_frame[,col, drop = TRUE]

    # Add adjustments
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

    if (!is.null(adj.fxn)) {
        values <- adj.fxn(values)
    }

    # Add names
    if (add.names) {
        names(values) <- .all_rows(data_frame)
    }

    values
}

#' Gives the distinct values of a column of data from the data_frame
#'
#' @param col quoted column name. the data column whose potential values should be retrieved.
#' @param data_frame A data.frame.
#' @param used.only TRUE by default, for target data that are factors, whether levels nonexistent in the target data should be ignored.
#' @param rows.use String vector of rows names OR an integer vector specifying the indices of rows which should be included.
#'
#' Alternatively, a Logical vector, the same length as the number of rows in the data_frame, which indicates which rows to include.
#' @return String vector, the distinct values of the \code{col} data column (among the \code{rows.use} targeted rows) of \code{data_frame}.
#' @author Daniel Bunis
#' @examples
#' example("dittoExampleData", echo = FALSE)
#'
#' colLevels("conditions", example_df)
#'
#' # Note: Set 'used.only' (default = TRUE) to FALSE to show unused levels
#' #  of data that are already factors.  By default, only the used options
#' #  of the data will be given.
#' colLevels("conditions", example_df,
#'     rows.use = example_df$conditions!="condition1"
#'     )
#' colLevels("conditions", example_df,
#'     rows.use = example_df$conditions!="condition1",
#'     used.only = FALSE)
#' @export
colLevels <- function(col, data_frame, rows.use = NULL, used.only = TRUE){
    if (!.is_col(col, data_frame)) {
        stop(dQuote(col)," is not a column of 'data_frame'")
    }
    if (used.only) {
        values <- as.character(._col(col, data_frame))
    } else {
        values <- ._col(col, data_frame)
    }
    if (!is.null(rows.use)) {
        all.rows <- .all_rows(data_frame)
        rows.use <- .which_rows(rows.use, data_frame)
        values <- values[all.rows %in% rows.use]
    }
    levels(as.factor(values))
}
