# Tests for visualization functions
# library(dittoViz); library(testthat); source("tests/testthat/setup.R"); for (i in list.files("R", pattern="^utils", full.names = TRUE)) source(i); source("tests/testthat/test-hover.R")

cont1 <- "number"
cont2 <- "bill_length_mm"
cont3 <- "flipper_length_mm"
cont4 <- "body_mass_g"
disc <- "groups"
disc2 <- "age"
disc3 <- "sex"
disc4 <- "island"

df$sample <- factor(
    sample(1:15, nrow(df), replace = TRUE),
    levels = 1:15)
df$sample_groups <- "B"
df$sample_groups[df$sample %in% 1:8] <- "A"
df$sample_subgroups <- "sg5"
df$sample_subgroups[df$sample %in% c(1,6,11)] <- "sg1"
df$sample_subgroups[df$sample %in% c(2,7,12)] <- "sg2"
df$sample_subgroups[df$sample %in% c(3,8,13)] <- "sg3"
df$sample_subgroups[df$sample %in% c(4,9,14)] <- "sg4"

plotly_installed <- requireNamespace("plotly", quietly = TRUE)

### scatterPlot
test_that("Showing hover.data works for scatterPlot (with rows.use)", {
    if (plotly_installed) {
        expect_s3_class(
            scatterPlot(df, cont1, cont2, do.hover = TRUE,
                             hover.data = c(cont1,disc2),
                             data.out = TRUE)[[1]],
            "plotly")
        expect_s3_class(
            scatterPlot(df, cont1, cont2, do.hover = TRUE,
                             hover.data = c(cont1,disc2),
                             rows.use = rep(c(TRUE,FALSE), length.out = nrow(df))),
            "plotly")
    } else {
        expect_error(
            scatterPlot(df, cont1, cont2, do.hover = TRUE,
                             hover.data = c(cont1,disc2)),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

test_that("scatterPlot hover.data default captures all desired aspects", {
    skip_if_not(plotly_installed, message = "No plotly")

    # Single var
    expect_s3_class(
        (x <- scatterPlot(
            df, cont1, cont2, cont3,
            x.adjustment = "z-score",
            y.adjustment = "relative.to.max",
            color.adj.fxn = ceiling,
            shape.by = disc, split.by = disc2,
            do.hover = TRUE,
            rows.use = rep(c(TRUE,FALSE), length.out = nrow(df)),
            data.out = TRUE))$plot,
        "plotly")
    first_hover <- x$Target_data$hover.string[1]
    expectations <- c(
        cont1, paste0(cont1, ".x.adj"),
        cont2, paste0(cont2, ".y.adj"),
        cont3, paste0(cont3, ".color.adj"),
        disc, disc2)
    expect_equal(
        vapply(
            expectations,
            function(check) {
                grepl(check, first_hover)
            },
            logical(1)
        ),
        rep(TRUE, length(expectations)),
        ignore_attr = TRUE
    )

    # Multi var
    expect_s3_class(
        (x <- scatterPlot(
            df, cont1, cont2, c(cont3, cont4),
            x.adjustment = "z-score",
            y.adjustment = "relative.to.max",
            color.adj.fxn = ceiling,
            shape.by = disc, split.by = disc2,
            do.hover = TRUE,
            rows.use = rep(c(TRUE,FALSE), length.out = nrow(df)),
            data.out = TRUE))$plot,
        "plotly")
    first_hover <- x$Target_data$hover.string[1]
    expectations <- c(
        cont1, paste0(cont1, ".x.adj"),
        cont2, paste0(cont2, ".y.adj"),
        cont3, cont4, "color.multi", "color.which",
        disc, disc2)
    expect_equal(
        vapply(
            expectations,
            function(check) {
                grepl(check, first_hover)
            },
            logical(1)
        ),
        rep(TRUE, length(expectations)),
        ignore_attr = TRUE
    )
})

test_that("scatterPlot hover.round.digits rounds numeric data", {
    skip_if_not(plotly_installed, message = "No plotly")

    length2 <- nchar(
        scatterPlot(
            df, cont1, cont2, do.hover = TRUE,
            hover.data = "PC1", data.out = TRUE,
            hover.round.digits = 2)$Target_data$hover.string[1]
    )
    length1 <- nchar(
        scatterPlot(
            df, cont1, cont2, do.hover = TRUE,
            hover.data = "PC1", data.out = TRUE,
            hover.round.digits = 1)$Target_data$hover.string[1]
    )

    expect_gt(length2, length1)
})

### yPlot
test_that("Showing hover.data works for yPlot, with rows.use", {
    if (plotly_installed) {
        expect_s3_class(
            yPlot(
                df, cont1,
                group.by = disc, color.by = disc,
                do.hover = TRUE,
                hover.data = c(cont1,disc2),
                data.out = TRUE)[[1]],
            "plotly")
        expect_s3_class(
            yPlot(
                df, cont1,
                group.by = disc, color.by = disc,
                do.hover = TRUE,
                hover.data = c(cont1,disc2),
                rows.use = rep(c(TRUE,FALSE), length.out = nrow(df))),
            "plotly")
        ### MANUAL CHECK: jitters should be centered within violins
        expect_s3_class(
            yPlot(
                df, cont1,
                group.by = disc, color.by = "sex",
                do.hover = TRUE,
                hover.data = c(cont1,disc2), vlnplot.width = 1,
                rows.use = rep(c(TRUE,FALSE), length.out = nrow(df))),
            "plotly")
    } else {
        expect_error(
            yPlot(
                df, cont1,
                group.by = disc, color.by = disc,
                do.hover = TRUE,
                hover.data = c(cont1,disc2)),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

test_that("yPlot hover.data default captures all desired aspects", {
    skip_if_not(plotly_installed, message = "No plotly")

    # Single var
    expect_s3_class(
        (x <- yPlot(
            df, cont1,
            var.adjustment = "z-score",
            group.by = disc, color.by = disc3,
            shape.by = disc2, split.by = disc4,
            do.hover = TRUE,
            plots = "jitter",
            rows.use = rep(c(TRUE,FALSE), length.out = nrow(df)),
            data.out = TRUE))$p,
        "plotly")
    first_hover <- x$data$hover.string[1]
    expectations <- c(cont1, paste0(cont1, ".adj"), disc, disc2, disc3, disc4)
    expect_equal(
        vapply(
            expectations,
            function(check) {
                grepl(check, first_hover)
            },
            logical(1)
        ),
        rep(TRUE, length(expectations)),
        ignore_attr = TRUE
    )

    # Multi var
    expect_s3_class(
        (x <- yPlot(
            df, c(cont1, cont2),
            var.adjustment = "z-score",
            group.by = disc, color.by = disc3,
            shape.by = disc2, split.by = disc4,
            do.hover = TRUE,
            plots = "jitter",
            rows.use = rep(c(TRUE,FALSE), length.out = nrow(df)),
            data.out = TRUE))$p,
        "plotly")
    first_hover <- x$data$hover.string[1]
    expectations <- c(cont1, cont2, "var.multi", "var.which", disc, disc2, disc3, disc4)
    expect_equal(
        vapply(
            expectations,
            function(check) {
                grepl(check, first_hover)
            },
            logical(1)
        ),
        rep(TRUE, length(expectations)),
        ignore_attr = TRUE
    )
})

test_that("yPlot hover.round.digits rounds numeric data", {
    skip_if_not(plotly_installed, message = "No plotly")

    length2 <- nchar(
        yPlot(
            df, cont1, group.by = disc, do.hover = TRUE,
            hover.data = "PC1", data.out = TRUE,
            hover.round.digits = 2)$data$hover.string[1]
    )
    length1 <- nchar(
        yPlot(
            df, cont1, group.by = disc, do.hover = TRUE,
            hover.data = "PC1", data.out = TRUE,
            hover.round.digits = 1)$data$hover.string[1]
    )

    expect_gt(length2, length1)
})

### barPlot
test_that("Showing hover.data works for barPlot (with rows.use)", {
    if (requireNamespace("plotly", quietly = TRUE)) {
        expect_s3_class(
            barPlot(
                df, disc2,
                group.by = disc,
                do.hover = TRUE,
                data.out = TRUE)[[1]],
            "plotly")
        expect_s3_class(
            barPlot(
                df, disc2,
                group.by = disc,
                do.hover = TRUE,
                rows.use = rep(c(TRUE,FALSE), length.out = nrow(df))),
            "plotly")
    } else {
        expect_error(
            barPlot(
                df, disc2,
                group.by = disc,
                do.hover = TRUE),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

test_that("barPlot hover.data default captures all desired aspects", {
    skip_if_not(plotly_installed, message = "No plotly")

    # var and group.by
    expect_s3_class(
        (x <- barPlot(
            df, disc,
            group.by = disc2, split.by = disc3,
            do.hover = TRUE,
            rows.use = rep(c(TRUE,FALSE), length.out = nrow(df)),
            data.out = TRUE))$p,
        "plotly")
    first_hover <- x$data$hover.string[1]
    expectations <- c(disc, disc2, disc3, "count", "percent")
    expect_equal(
        vapply(
            expectations,
            function(check) {
                grepl(check, first_hover)
            },
            logical(1)
        ),
        rep(TRUE, length(expectations)),
        ignore_attr = TRUE
    )
})

test_that("barPlot hover.round.digits rounds numeric data", {
    skip_if_not(plotly_installed, message = "No plotly")

    length2 <- nchar(
        barPlot(
            df, disc, group.by = disc2, do.hover = TRUE, data.out = TRUE,
            hover.round.digits = 2)$data$hover.string[1]
    )
    length0 <- nchar(
        barPlot(
            df, disc, group.by = disc2, do.hover = TRUE, data.out = TRUE,
            hover.round.digits = 0)$data$hover.string[1]
    )
    expect_gt(length2, length0)
})

### freqPlot
test_that("Showing hover.data works for freqPlot (with rows.use)", {
    if (requireNamespace("plotly", quietly = TRUE)) {
        expect_s3_class(
            freqPlot(
                df, disc2,
                group.by = disc,
                do.hover = TRUE,
                data.out = TRUE)[[1]],
            "plotly")
        expect_s3_class(
            freqPlot(
                df, disc2,
                group.by = disc,
                do.hover = TRUE,
                rows.use = rep(c(TRUE,FALSE), length.out = nrow(df))),
            "plotly")
    } else {
        expect_error(
            freqPlot(
                df, disc2,
                group.by = disc,
                do.hover = TRUE),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

test_that("freqPlot hover.data default captures all desired aspects", {
    skip_if_not(plotly_installed, message = "No plotly")

    # var and group.by
    expect_s3_class(
        (x <- freqPlot(
            df, "species",
            sample.by = "sample",
            group.by = "sample_groups",
            color.by = "sample_subgroups",
            do.hover = TRUE,
            plots = "jitter",
            rows.use = rep(c(TRUE,FALSE), length.out = nrow(df)),
            data.out = TRUE))$p,
        "plotly")
    first_hover <- x$data$hover.string[1]
    expectations <- c("label", "sample", "grouping", "sample_subgroups", "count", "percent")
    expect_equal(
        vapply(
            expectations,
            function(check) {
                grepl(check, first_hover)
            },
            logical(1)
        ),
        rep(TRUE, length(expectations)),
        ignore_attr = TRUE
    )
})

test_that("freqPlot hover.round.digits rounds numeric data", {
    skip_if_not(plotly_installed, message = "No plotly")

    length2 <- nchar(
        freqPlot(
            df, disc2, group.by = disc, do.hover = TRUE, data.out = TRUE,
            hover.round.digits = 2)$data$hover.string[1]
    )
    length0 <- nchar(
        freqPlot(
            df, disc2, group.by = disc, do.hover = TRUE, data.out = TRUE,
            hover.round.digits = 0)$data$hover.string[1]
    )
    expect_gt(length2, length0)
})

