.add_splitting <- function(p, split.by, nrow, ncol, split.args) {
    # Adds ggplot faceting to go with 'split.by' utilization.

    # When split.by is length 1, the shape is controlled with ncol & nrow
    if (length(split.by) == 1) {
        split.args$facets <- split.by
        split.args$nrow <- nrow
        split.args$ncol <- ncol
        return(p + do.call(facet_wrap, split.args))
    }

    # When split.by is length 2, the first element is used for rows, and the
    # second element is used for columns.
    if (length(split.by) == 2) {
        split.args$rows <-
            eval(expr(paste0(".data$", split.by[1], "~ .data$", split.by[2])))
        return(p + do.call(facet_grid, split.args))
    }
}

.remove_legend <- function(ggplot) {
    # Shorthand for ggplot legend removal
    ggplot + theme(legend.position = "none")
}

#' @importFrom cowplot ggdraw get_legend
.grab_legend <- function(ggplot) {
    # Obtains and plots just the legend of a ggplot
    cowplot::ggdraw(cowplot::get_legend(ggplot))
}

.add_letters_ellipses_labels_if_discrete <- function(
    p, data, x.by, y.by, color.by,
    do.letter, do.ellipse, do.label,
    labels.highlight, labels.size, labels.repel, labels.split.by,
    labels.repel.adjust, labels.use.numbers, labels.numbers.spacer,
    legend.color.title,
    letter.size, letter.opacity, letter.legend.title, letter.legend.size) {
    if (!is.numeric(data[, color.by])) {
        if (do.letter) {
            p <- .add_letters(
                p, data, x.by, y.by, color.by,
                letter.size, letter.opacity, letter.legend.title, letter.legend.size
            )
        }

        if (do.ellipse) {
            p <- p + stat_ellipse(
                data = data,
                aes(x = .data[[x.by]], y = .data[[y.by]], colour = .data[[color.by]]),
                type = "t", linetype = 2, linewidth = 0.5, show.legend = FALSE, na.rm = TRUE
            )
        }

        if (do.label) {
            p <- .add_labels(
                p, data, color.by, x.by, y.by,
                labels.highlight, labels.size, labels.repel, labels.split.by,
                labels.repel.adjust, labels.use.numbers, labels.numbers.spacer,
                legend.color.title)
        }
    } else {
        # Data is incompatible, so message instead of adding.
        ignored.targs <- paste(
            c("do.letter", "do.ellipse", "do.label")[c(do.letter, do.ellipse, do.label)],
            collapse = ", "
        )
        .msg_if(
            do.letter || do.ellipse || do.label,
            ignored.targs, " was/were ignored for non-discrete data."
        )
    }

    p
}

.add_contours <- function(
    p, data, x.by, y.by, color, linetype = 1) {
    # Add contours based on the density of data points
    # (Dim and Scatter plots)

    p + geom_density_2d(
        data = data,
        mapping = aes(x = .data[[x.by]], y = .data[[y.by]]),
        color = color,
        linetype = linetype,
        na.rm = TRUE
    )
}

#' @importFrom stats setNames
.add_labels <- function(
    p, Target_data, labels.by, x.by, y.by,
    labels.highlight, labels.size, labels.repel, split.by,
    labels.repel.adjust, labels.use.numbers, labels.numbers.spacer,
    legend.color.title
    ) {
    # Add text labels at/near the median x and y values for each group
    # (Scatter plots)

    # Determine medians
    if (is.null(split.by)) {
        median.data <- .calc_xy_medians(Target_data, labels.by, x.by, y.by)
    } else if (length(split.by) == 1) {
        median.data <- NULL

        for (level in levels(as.factor(as.character(Target_data[, split.by])))) {
            level.dat <- Target_data[Target_data[, split.by] == level, ]

            level.med.dat <- .calc_xy_medians(level.dat, labels.by, x.by, y.by)
            # Add split.by columns
            level.med.dat$split1 <- level
            colnames(level.med.dat)[4] <- split.by

            median.data <- rbind(median.data, level.med.dat)
        }

        # Ensure retention of factor level ordering
        median.data[, split.by] <- .retain_factor_level_order(
            median.data[, split.by],
            possible_factor = Target_data[, split.by]
        )
    } else if (length(split.by) == 2) {
        median.data <- NULL

        for (level1 in levels(as.factor(as.character(Target_data[, split.by[1]])))) {
            for (level2 in levels(as.factor(as.character(Target_data[, split.by[2]])))) {
                level.dat <- Target_data[Target_data[, split.by[1]] == level1, ]
                level.dat <- level.dat[level.dat[, split.by[2]] == level2, ]

                if (nrow(level.dat) > 0) {
                    level.med.dat <- .calc_xy_medians(level.dat, labels.by, x.by, y.by)
                    # Add split.by columns
                    level.med.dat$split1 <- level1
                    level.med.dat$split2 <- level2
                    colnames(level.med.dat)[4:5] <- split.by

                    median.data <- rbind(median.data, level.med.dat)
                }
            }
        }

        # Ensure retention of factor level ordering
        median.data[, split.by[1]] <- .retain_factor_level_order(
            median.data[, split.by[1]],
            possible_factor = Target_data[, split.by[1]]
        )
        median.data[, split.by[2]] <- .retain_factor_level_order(
            median.data[, split.by[2]],
            possible_factor = Target_data[, split.by[2]]
        )
    }

    if (labels.use.numbers) {
        # Determine which scale will need to be updated
        which_scale <- NULL
        for (i in (seq_along(p$scales$scales))) {
            if (p$scales$scales[[i]]$name==legend.color.title &&
                p$scales$scales[[i]]$aesthetics %in% c("colour", "fill")) {
                which_scale <- i
                break
            }
        }
        if (is.null(which_scale)) {
            warning("Cannot determine which scale to modify in the legend for labeling by numbers.")
        } else {
            labels <- levels(as.factor(Target_data[, labels.by]))
            num_map <- setNames(
                seq_along(labels),
                labels)
            label_map <- setNames(
                paste0(seq_along(labels), labels.numbers.spacer, labels),
                labels)
            median.data$label <- num_map[median.data$label]

            # Update scale labels
            p$scales$scales[[which_scale]]$labels <- label_map[labels]
        }
    }

    # Add labels
    args <- list(
        data = median.data,
        mapping = aes(x = .data$cent.x, y = .data$cent.y, label = .data$label),
        size = labels.size
    )
    if (labels.repel) {
        if (is.list(labels.repel.adjust)) {
            args <- c(args, labels.repel.adjust)
        }
        geom.use <- if (labels.highlight) {
            ggrepel::geom_label_repel
        } else {
            ggrepel::geom_text_repel
        }
    } else {
        geom.use <- if (labels.highlight) {
            geom_label
        } else {
            geom_text
        }
    }

    p + do.call(geom.use, args)
}

.retain_factor_level_order <- function(new_data, possible_factor) {
    if (is.factor(possible_factor)) {
        factor(new_data, levels = levels(possible_factor))
    } else {
        new_data
    }
}

.calc_xy_medians <- function(x.y.group.df, group.col, x.by, y.by) {
    groups <- levels(as.factor(as.character(x.y.group.df[, group.col])))
    data.frame(
        cent.x = vapply(
            groups,
            function(level) {
                median(x.y.group.df[x.y.group.df[, group.col] == level, x.by], na.rm = TRUE)
            },
            FUN.VALUE = numeric(1)
        ),
        cent.y = vapply(
            groups,
            function(level) {
                median(x.y.group.df[x.y.group.df[, group.col] == level, y.by], na.rm = TRUE)
            },
            FUN.VALUE = numeric(1)
        ),
        label = groups
    )
}

.add_trajectories_by_groups <- function(
    p, data, x.by, y.by, trajectories, group.by, arrow.size = 0.15) {
    # Add trajectory path arrows, following sets of group-to-group paths, from group median to group median.
    # (Scatter plots)
    #
    # p = a ggplot to add to
    # data = a data_frame containing columns of x.by, y.by, and group.by
    # group.by = the name of the column that holds the group.by info
    # trajectories = List of lists of group-to-group paths. If relevant, equivalent to the output of SlingshotDataSet(SCE_with_slingshot)$lineages
    # arrow.size = numeric scalar that sets the arrow length (in inches) at the endpoints of trajectory lines.

    # Determine medians
    cluster.levels <- colLevels(group.by, data)
    group_medians <- .calc_xy_medians(data, group.by, x.by, y.by)

    # Add trajectories
    for (i in seq_along(trajectories)) {
        p <- p + geom_path(
            data = group_medians[as.character(trajectories[[i]]), ],
            aes(x = .data$cent.x, y = .data$cent.y),
            arrow = arrow(
                angle = 20, type = "closed", length = unit(arrow.size, "inches")
            )
        )
    }
    p
}

.add_trajectory_curves <- function(
    p, trajectories, arrow.size = 0.15) {
    # Add trajectory path arrows following sets of given (x,y) coordinates.
    # (Dim and Scatter plots)
    #
    # p = a ggplot to add to
    # trajectories = List of matrices (or data.frames) containing trajectory curves, all with two columns, x and y coordinates.
    # arrow.size = numeric scalar that sets the arrow length (in inches) at the endpoints of trajectory lines.

    # Add trajectories for general list of matrices provision method.
    for (i in seq_along(trajectories)) {
        data <- as.data.frame(trajectories[[i]])
        names(data) <- c("x", "y")
        p <- p + geom_path(
            data = data,
            aes(x = .data$x, y = .data$y),
            arrow = arrow(
                angle = 20, type = "closed", length = unit(arrow.size, "inches")
            )
        )
    }
    p
}

.add_letters <- function(
    p, Target_data, x.by, y.by, col.use = "color", size, opacity, legend.title,
    legend.size) {
    # Overlay letters on top of the original colored dots.
    # Color blindness aid
    # (Dim and Scatter plots)

    letters.needed <- length(levels(as.factor(Target_data[, col.use])))
    letter.labels <- c(
        LETTERS, letters, 0:9, "!", "@", "#", "$", "%", "^", "&", "*", "(",
        ")", "-", "+", "_", "=", ";", "/", "|", "{", "}", "~"
    )[seq_len(letters.needed)]
    names(letter.labels) <- levels(as.factor(Target_data[, col.use]))
    p <- p +
        geom_point(
            data = Target_data,
            aes(x = .data[[x.by]], y = .data[[y.by]], shape = .data[[col.use]]),
            color = "black", size = size * 3 / 4, alpha = opacity
        ) +
        scale_shape_manual(
            name = legend.title,
            values = letter.labels
        )
    p
}


# Checks wrapper for .add_*line functions
._ensure_lengths_and_adjust_for_panels <- function(
        add.line, add.line.name,
        line.param, line.param.name,
        num.panels
) {
    if (length(line.param) != length(add.line) & length(line.param) != 1) {
        warning("'", line.param.name, "' must be length 1 or the same length as '", add.line.name, "', using only the first provided value.")
        line.param[1]
    } else if (length(line.param) != 1) {
        rep(line.param, num.panels)
    } else {
        line.param
    }
}

.add_xline <- function(p, add.xline, xline.linetype, xline.color, xline.linewidth, xline.opacity, num.panels) {

    ._elafp <- function(l.p, l.p.n) {
        ._ensure_lengths_and_adjust_for_panels(add.xline, "add.xline", l.p, l.p.n, num.panels)
    }
    xline.linetype <- ._elafp(xline.linetype, "xline.linetype")
    xline.color <- ._elafp(xline.color, "xline.color")
    xline.linewidth <- ._elafp(xline.linewidth, "xline.linewidth")
    xline.opacity <- ._elafp(xline.opacity, "xline.opacity")

    p + geom_vline(
        xintercept = add.xline, linetype = xline.linetype, color = xline.color,
        linewidth = xline.linewidth, alpha = xline.opacity
    )
}

.add_yline <- function(p, add.yline, yline.linetype, yline.color, yline.linewidth, yline.opacity, num.panels) {

    ._elafp <- function(l.p, l.p.n) {
        ._ensure_lengths_and_adjust_for_panels(add.yline, "add.yline", l.p, l.p.n, num.panels)
    }
    yline.linetype <- ._elafp(yline.linetype, "yline.linetype")
    yline.color <- ._elafp(yline.color, "yline.color")
    yline.linewidth <- ._elafp(yline.linewidth, "yline.linewidth")
    yline.opacity <- ._elafp(yline.opacity, "yline.opacity")

    p + geom_hline(
        yintercept = add.yline, linetype = yline.linetype, color = yline.color,
        linewidth = yline.linewidth, alpha = yline.opacity
    )
}

.add_abline <- function(p, add.abline, abline.slope, abline.linetype, abline.color, abline.linewidth, abline.opacity, num.panels) {

    ._elafp <- function(l.p, l.p.n) {
        ._ensure_lengths_and_adjust_for_panels(add.abline, "add.abline", l.p, l.p.n, num.panels)
    }
    abline.linetype <- ._elafp(abline.linetype, "abline.linetype")
    abline.color <- ._elafp(abline.color, "abline.color")
    abline.linewidth <- ._elafp(abline.linewidth, "abline.linewidth")
    abline.opacity <- ._elafp(abline.opacity, "abline.opacity")
    if (length(abline.slope) != length(add.abline) & length(abline.slope) != 1) {
        warning("'abline.slope' must be length 1 or the same length as 'add.abline', using only the first provided value.")
        abline.slope <- abline.slope[1]
    }

    p + geom_abline(
        intercept = add.abline, slope = abline.slope, linetype = abline.linetype, color = abline.color,
        linewidth = abline.linewidth, alpha = abline.opacity
    )
}
