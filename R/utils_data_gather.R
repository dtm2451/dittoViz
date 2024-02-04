.make_hover_strings_from_df <- function(df, round.digits){
    # Creates a single character vector where each element is the hoverstring
    # for a given row of the provided 'df' with structure
    # "col1name: col1-value\ncol2name: var2-value\ncol3name: var3-value\n..."

    # Ensure data exists
    if (ncol(df)<1) {
        stop("Hover-string generation has no data. 'hover.data' must be given at least one column name that exists in the data.")
    }

    # Round numeric data
    for (col in colnames(df)) {
        if (is.numeric(df[,col])) {
            df[,col] <- round(df[,col], digits = round.digits)
        }
    }

    # Collect and format
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
        multivar.split.dir, rows.use, do.hover, hover.data, hover.round.digits
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
        hover_exists <- hover.data[hover.data %in% colnames(data_use)]
        data_use$hover.string <- .make_hover_strings_from_df(
            data_use[,hover_exists,drop=FALSE], hover.round.digits)
        cols_use$hover.text <- "hover.string"
    }

    list(
        data_use = data_use,
        data_other = data_other,
        cols_use = cols_use)
}

.make_composition_summary_df <- function(
        data_frame, var, group.by, split.by, rows.use,
        x.reorder, x.labels,
        var.labels.reorder, var.labels.rename,
        do.hover, hover.round.digits = 5, max.normalize = FALSE,
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
        hover.data <- unique(c("grouping", "label", "count", "percent", split.by))
        hover.df <- data[, hover.data]
        colnames(hover.df)[1:2] <- c(group.by, var)
        # Make hover strings, "data.type: data" \n "data.type: data"
        data$hover.string <- .make_hover_strings_from_df(hover.df, hover.round.digits)
    }

    data
}
