.determine_color_to_group_relation <- function(
        data_frame,
        group.by,
        color.by
) {
    # This function determines the relation of requested group.by and color.by
    # Returns and Options are:
    # "same": Every group-level is exactly its own color-level. Can be from the
    #  exact same data being used for both, OR data with different labels but
    #  exactly 1:1 mapping
    # "sub": Color-values change within groups to create "sub"groupings. E.g.
    #  a treatment condition where samples are being grouped by some other
    #  feature.
    # "super": Color-values do not change within any individual group, but map
    #  1:many (color:groups) to provide "super"groupings. E.g. a category of
    #  samples where there are multiple observations per sample and group.by is
    #  given sample data while color.by is given category data.

    # Simplest "same" case which does not require any other checks
    if (group.by == color.by) {
        return("same")
    }

    # Collect all sets of colors per each group-level
    cols_per <- c()
    all_groups <- colLevels(group.by, data_frame)
    for (g in all_groups) {
        cols_per <- c(
            cols_per, colLevels(color.by, data_frame, data_frame[,group.by]==g)
        )
    }
    # Assess
    if (length(cols_per) > length(all_groups)) {
        # (many:1 or many:many)
        "sub"
    } else if (length(unique(cols_per)) < length(all_groups)) {
        # (1:many)
        "super"
    } else {
        # (1:1)
        "same"
    }
}

.calc_stats <- function(
        data_frame,
        var,
        comp.by, comp.setups,
        split.by,
        split.for.calc.only = NULL,
        sample.by = NULL,
        sample.summary = "mean",
        test.method = "wilcox.test",
        test.adjust = list(),
        p.symbols = FALSE,
        p.round.digits = 4,
        do.adjust = TRUE,
        p.adjust.method = "fdr",
        do.fc = TRUE,
        fc.pseudocount = 0,
        offset.first = 0.1,
        offset.between = 0.2,
        outermost = TRUE,
        split.completed = NULL
) {

    if (!is.function(get(test.method))) {
        stop("'get()' of pvalues 'test.method' does not retrieve a function.")
    }
    if (!identical(sample.by, NULL) && !is.function(get(sample.summary))) {
        stop("'get()' of pvalues 'sample.summary' does not retrieve a function.")
    }

    description <- ''

    # Data coming in should be pre-trimmed

    # Recursion for split.by
    # Ensure that within-facet split.by cols are innermost
    split.by <- unique(c(split.for.calc.only, split.by))
    split.by <- split.by[!split.by %in% split.completed]
    if (!is.null(split.by) && length(split.by)>0) {
        split_col <- split.by[length(split.by)]
        split.completed <- c(split.completed, split_col)
        out_list <- list()
        for (split_val in unique(data_frame[[split_col]])) {
            this_split <- data_frame[[split.by[length(split.by)]]]==split_val
            next_split.by <- split.by[-1*length(split.by)]
            new <- .calc_stats(
                data_frame[this_split,],
                var, comp.by, comp.setups,
                split.by = next_split.by, split.for.calc.only = split.for.calc.only,
                sample.by = sample.by, sample.summary = sample.summary,
                test.method = test.method, test.adjust = test.adjust,
                do.adjust = do.adjust,
                do.fc = do.fc, fc.pseudocount = fc.pseudocount,
                outermost = FALSE, split.completed = split.completed)
            print(new)
            if (!is.null(new) && nrow(new) > 0) {
                new[[split_col]] <- split_val
                out_list[[length(out_list)+1]] <- new
            }
        }
        out <- do.call(rbind, out_list)
        if (split_col %in% split.for.calc.only) {
            out$max_data <- max(out$max_data)
        }
    } else {
        # Standard / single actual iteration:
        stats <- list()
        offset <- 1 + offset.first
        # Loop though comparison setups
        for (ind in seq_along(comp.setups)) {
            group.1 <- comp.setups[[ind]][1]
            group.2 <- comp.setups[[ind]][2]
            g1s <- as.vector(data_frame[[comp.by]]==group.1)
            g2s <- as.vector(data_frame[[comp.by]]==group.2)

            if (sum(g1s)==0 || sum(g2s)==0) {
                warning("No data for a given data grouping in stats calculation.")
                next
            }

            new <- data.frame(
                group1 = group.1,
                group2 = group.2,
                max_data = max(data_frame[,var], na.rm = TRUE),
                min_data = min(data_frame[,var], na.rm = TRUE),
                stringsAsFactors = FALSE
            )

            if (!identical(sample.by, NULL)) {
                samples <- ._col(sample.by, data_frame, add.names = FALSE)
                .summarize_vals_per_sample <- function(set_logical) {
                    vapply(
                        colLevels(sample.by, data_frame[set_logical, , drop = FALSE]),
                        function(samp) {
                            get(sample.summary)(data_frame[set_logical & samples==samp, var], na.rm = TRUE)
                        },
                        numeric(1)
                    )
                }
                g1_vals <- .summarize_vals_per_sample(g1s)
                g2_vals <- .summarize_vals_per_sample(g2s)
            } else {
                g1_vals <- data_frame[g1s, var]
                g2_vals <- data_frame[g2s, var]
            }

            if (do.fc) {
                new$median_g1 <- median(g1_vals, na.rm = TRUE)
                new$median_g2 <- median(g2_vals, na.rm = TRUE)
                if (new$median_g2==0 && fc.pseudocount==0) {
                    warning("Looks like a pseudocount will be needed to avoid division by zero errors. Try adding 'fc.pseudocount = 0.000001' to your call and see the '?freq_stats' documentation for details.")
                }
                new$median_fold_change <- (new$median_g1 + fc.pseudocount) / (new$median_g2 + fc.pseudocount)
                new$median_log2_fold_change <- log2(new$median_fold_change)
            }

            test.adjust_this <- test.adjust
            test.adjust_this$x <- g1_vals
            test.adjust_this$y <- g2_vals
            new$p <- do.call(get(test.method), test.adjust_this)$p.value

            new$offset <- offset
            offset <- offset + offset.between

            stats[[length(stats)+1]] <- new
        }
        out <- do.call(rbind, stats)
        # Needed to avoid ggplot2 complaint per ggpubr implementation
        if (length(stats) > 0) {
            out[[comp.by]] <- group.1
        }
    }

    if (outermost) {
        # Multiple Hypothesis Correction
        p_use <- "p"
        if (do.adjust) {
            out$padj <- p.adjust(out$p, method = p.adjust.method)
            p_use <- "padj"
        }
        # Symbolize or Round p-values
        if (identical(p.symbols, TRUE)) {
            p.symbolize <- function(p) {
                ifelse(p > 0.05, "ns",
                       ifelse(p > 0.01, "*",
                              ifelse(p > 0.001, "**",
                                     ifelse(p > 0.0001, "***",
                                            "****"))))
            }
            out$p_show <- p.symbolize(out[,p_use])
        } else if (is.function(p.symbols)) {
            out$p_show <- p.symbols(out[,p_use])
        } else {
            # Round shown p-values when p.symbols not 'on'
            out$p_show <- round(out[,p_use], p.round.digits)
        }
        # ToDo: Add description in first row of the data frame
        # out$stat_calc_method_description <- NA
        # out$stat_calc_method_description[1] <- description
    }

    # Output
    out
}

#' @noRd
#' @importFrom stats setNames
add_x_pos <- function(stats, data, primary.by, p.by, secondary.by, dodge) {
    # ggpubr::stat_pvalue_manual looks to group1 and group2 columns except if
    # xmin and xmax columns exist.  Then, these are used instead.

    primary <- setNames(as.numeric(as.factor(stats[,primary.by])), stats[,primary.by])

    if (p.by != primary.by) {
        # Case: btwn subgroups, within groups
        dodge_steps <- list()
        dodge_vals <- list()
        for (this_group in colLevels(primary.by, data)) {
            these_levs <- colLevels(p.by, data[data[,primary.by]==this_group,])
            # x dodge distance between groups
            dodge_steps[[this_group]] <- dodge / length(these_levs)
            # centered unit locations of groups
            dodge_vals[[this_group]] <- setNames(
                as.vector(scale(seq_along(these_levs), center = TRUE, scale = FALSE)),
                these_levs
            )
        }
        stats$xmin <- sapply(seq_len(nrow(stats)), function(i) {
            primary[i] + dodge_vals[[stats[i,primary.by]]][stats[i,"group1"]] * dodge_steps[[stats[i,primary.by]]]
        })
        stats$xmax <- sapply(seq_len(nrow(stats)), function(i) {
            primary[i] + dodge_vals[[stats[i,primary.by]]][stats[i,"group2"]] * dodge_steps[[stats[i,primary.by]]]
        })
    } else {
        if (!is.null(secondary.by)) {
            # Case: btwn groups, within subgroups
            dodge_steps <- list()
            dodge_vals <- list()
            x_vals <- list()
            for (this_group in colLevels(primary.by, data)) {
                x_vals[[this_group]] <- length(x_vals)+1
                these_levs <- colLevels(secondary.by, data[data[,primary.by]==this_group,])
                # x dodge distance between groups
                dodge_steps[[this_group]] <- dodge / length(these_levs)
                # centered unit locations of groups
                dodge_vals[[this_group]] <- setNames(
                    as.vector(scale(seq_along(these_levs), center = TRUE, scale = FALSE)),
                    these_levs
                )
            }
            stats$xmin <- sapply(seq_len(nrow(stats)), function(i) {
                x_vals[[stats[i,"group1"]]] + dodge_vals[[stats[i,"group1"]]][stats[i,secondary.by]] * dodge_steps[[stats[i,"group1"]]]
            })
            stats$xmax <- sapply(seq_len(nrow(stats)), function(i) {
                x_vals[[stats[i,"group2"]]] + dodge_vals[[stats[i,"group2"]]][stats[i,secondary.by]] * dodge_steps[[stats[i,"group2"]]]
            })
        } else {
            # Case: No subgroups
            stats$xmin <- stats$group1
            stats$xmax <- stats$group2
        }
    }

    stats
}
