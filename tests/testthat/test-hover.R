# Tests for visualization functions
# library(dittoViz); library(testthat); source("tests/testthat/setup.R"); source("tests/testthat/test-hover.R")

cont1 <- "number"
cont2 <- "bill_length_mm"
cont3 <- "flipper_length_mm"
disc <- "groups"
disc2 <- "age"

test_that("Showing hover.data works for scatterPlot (with rows.use)", {
    if (requireNamespace("plotly", quietly = TRUE)) {
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
        expect_warning(
            scatterPlot(df, cont1, cont2, do.hover = TRUE,
                             hover.data = c(cont1,disc2)),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

test_that("Showing hover.data works for yPlot (with rows.use)", {
    if (requireNamespace("plotly", quietly = TRUE)) {
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
    } else {
        expect_warning(
            yPlot(
                df, cont1,
                group.by = disc, color.by = disc,
                do.hover = TRUE,
                hover.data = c(cont1,disc2)),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

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
        expect_warning(
            barPlot(
                df, disc2,
                group.by = disc,
                do.hover = TRUE),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

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
        expect_warning(
            freqPlot(
                df, disc2,
                group.by = disc,
                do.hover = TRUE),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

