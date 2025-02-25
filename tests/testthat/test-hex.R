# Tests for scatterHex
# library(dittoViz); library(testthat); source("tests/testthat/setup.R"); for (i in list.files("R", pattern="^utils", full.names = TRUE)) source(i); source("tests/testthat/test-hex.R")

df$number <- as.numeric(seq_len(nrow(df)))
cont1 <- "bill_length_mm"
cont2 <- "number"
disc <- "species"
disc2 <- "island"

rows.names <- rownames(df)[1:40]
rows.logical <- c(rep(TRUE, 40), rep(FALSE,nrow(df)-40))

test_that("scatterHex can plot density only, and continuous or discrete color.by data", {
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2), "ggplot")
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc), "ggplot")
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2), "ggplot")
})

test_that("scatterHex - bins adjusts number of bins", {
    ### Manual check: Large bins
    expect_s3_class(scatterHex(df, x.by=cont1, y.by=cont2, bins = 5), "ggplot")
})

test_that("scatterHex - color.method options work for discrete data, and defaults to 'max'", {
    ### Manual: Should have continuous color-scale and max.props in its title
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                                color.method = "prop.Adelie"),
                    "ggplot")

    ### Manual: Should have continuous color-scale and max.props in its title
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                                color.method = "max.prop"),
                    "ggplot")

    ### Manual: Next 2 should be the same plot with discrete color legend and "max" in its title
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                                color.method = "max"),
                    "ggplot")
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc),
                    "ggplot")

    expect_error(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                             color.method = "abcde"),
                 "'color.method' not valid", fixed = TRUE)
    expect_error(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                            color.method = "prop.ABC"),
                 "'color.method' not valid", fixed = TRUE)
})

test_that("scatterHex - color.method options work for continuous data, and defaults to 'median'", {
    ### Manual: First should have lower max color than second
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                                color.method = "max"),
                    "ggplot")
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                                color.method = "sum"),
                    "ggplot")

    ### Manual: Next 2 should be the same plot
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                                color.method = "median"),
                    "ggplot")
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2),
                    "ggplot")

    expect_error(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                             color.method = "abcde"),
                 "'color.method' not valid", fixed = TRUE)
})

test_that("scatterHex can be subset to show only certain obs with any rows.use method", {
    expect_s3_class((c1 <- scatterHex(data_frame=df, x.by=cont1, y.by=cont2, data.out = TRUE,
                                       rows.use = rows.names))$plot,
                    "ggplot")
    expect_s3_class((c2 <- scatterHex(data_frame=df, x.by=cont1, y.by=cont2, data.out = TRUE,
                                       rows.use = rows.logical))$plot,
                    "ggplot")
    expect_s3_class((c3 <- scatterHex(data_frame=df, x.by=cont1, y.by=cont2, data.out = TRUE,
                                       rows.use = 1:40))$plot,
                    "ggplot")
    expect_equal(c1$data,c2$data)
    expect_equal(c1$data,c3$data)
    expect_equal(nrow(c3$data), 40)
    # And if we remove an entire grouping...
    expect_s3_class(scatterHex(disc, data_frame=df, x.by=cont1, y.by=cont2,
                                rows.use = df[[disc]]!=0),
                    "ggplot")
})

test_that("scatterHex colors can be adjusted for discrete data", {
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                                color.panel = cols), "ggplot")

    ### Manual check: These two should look the same.
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                                color.panel = cols[5:1]), "ggplot")
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                                color.panel = cols,
                                colors = 5:1), "ggplot")
})

test_that("scatterHex color legend: groupings can be renamed", {
    ### Manual check: color groups should be 1:5 (instead of A:E)
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                                rename.color.groups = 1:3), "ggplot")
})

test_that("scatterHex color scales can be adjusted for continuous color data", {

    ### Manual check: Legend range adjusted and black to orange
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                                min = -5, max = 150, min.color = "black", max.color = "orange"),
                    "ggplot")

    ### Manual check: Legend has breaks at all 50s in 50 to 300
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                                legend.color.breaks = seq(50,300,50)),
                    "ggplot")

    ### Manual check: Plot looks similar to above except from "WOW", 2:5, to "HEY"
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                                legend.color.breaks = seq(50,300,50),
                                legend.color.breaks.labels = c("WOW",2:5,"HEY!")),
                    "ggplot")
})

test_that("scatterHex color scales can be adjusted for density (color)", {

    ### Manual check: Legend range adjusted and black to orange
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2,
                               min.density = -2, max.density = 2, min.color = "black", max.color = "orange"),
                    "ggplot")

    ### Manual check: Legend from 1:3
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2,
                               legend.density.breaks = seq(1:3)),
                    "ggplot")

    ### Manual check: Plot looks similar to above except from "WOW", 2, to "HEY"
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2,
                               legend.density.breaks = seq(1:3),
                               legend.density.breaks.labels = c("WOW",2,"HEY!")),
                    "ggplot")
})

test_that("scatterHex color scales can be adjusted for density (opacity)", {

    ### Manual check: Opacity legend range adjusted -2 to 2 & barely any different
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                               min.density = -2, max.density = 2, min.opacity = 0.5, max.opacity = 0.6),
                    "ggplot")

    ### Manual check: Opacity legend breaks only at 1 and 3
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                               legend.density.breaks = c(1,3)),
                    "ggplot")

    ### Manual check: Opaacity legend from "WOW", 2, to "HEY"
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                               legend.density.breaks = seq(1:3),
                               legend.density.breaks.labels = c("WOW",2,"HEY!")),
                    "ggplot")
})

test_that("scatterHex titles and theme can be adjusted", {

    ### Manual check: All titles should be adjusted.
    expect_s3_class(
        scatterHex(
            cont2, data_frame=df, x.by=cont1, y.by=cont2,
            main = "Gotta catch", sub = "em all",
            xlab = "Pokemon", ylab = "Pokedex #s",
            legend.color.title = "groups",
            legend.density.title = "Encounters"),
        "ggplot")

    ### Manual check: density legend (color)  = Encounters
    expect_s3_class(
        scatterHex(
            data_frame=df, x.by=cont1, y.by=cont2,
            legend.density.title = "Encounters"),
        "ggplot")

    ### Manual check: top and right plot outline removed
    expect_s3_class(scatterHex(cont2, data_frame=df, x.by=cont1, y.by=cont2,
                                theme = theme_classic()),
                    "ggplot")

    ### Manual Check: Legend removed
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2,
                                legend.show = FALSE),
                    "ggplot")
})

test_that("scatterHex can be faceted with split.by (1 or 2 vars)", {

    # MANUAL CHECK: FACETING
    expect_s3_class(
        scatterHex(
            disc, data_frame=df, x.by=cont1, y.by=cont2,
            split.by = disc2),
        "ggplot")

    # MANUAL CHECK: horizontal faceting
    expect_s3_class(
        scatterHex(
            disc, data_frame=df, x.by=cont1, y.by=cont2,
            split.by = disc2,
            split.nrow = 1),
        "ggplot")

    # MANUAL CHECK: vertical faceting
    expect_s3_class(
        scatterHex(
            disc, data_frame=df, x.by=cont1, y.by=cont2,
            split.by = disc2,
            split.ncol = 1),
        "ggplot")

    # MANUAL CHECK: Grid with rows=age, cols=groups
    expect_s3_class(
        scatterHex(
            disc, data_frame=df, x.by=cont1, y.by=cont2,
            split.by = c(disc2,disc)),
        "ggplot")

    expect_s3_class(
        scatterHex(
            disc, data_frame=df, x.by=cont1, y.by=cont2,
            split.by = c(disc2,disc),
            rows.use = rows.logical),
        "ggplot")
})

test_that("scatterHex allows plotting of multiple vars, via faceting", {
    expect_s3_class(
        scatterHex(
            data_frame=df, x.by=cont1, y.by=cont2, c(cont1, cont2)),
        "ggplot")

    # Works with rows.use
    expect_s3_class(
        scatterHex(
            data_frame=df, x.by=cont1, y.by=cont2, c(cont1, cont2),
            rows.use = rows.logical),
        "ggplot")

    # These should have transposed facet grids
    expect_s3_class(
        print(scatterHex(
            data_frame=df, x.by=cont1, y.by=cont2, c(cont1, cont2),
            split.by = disc2)),
        "ggplot")
    expect_s3_class(
        print(scatterHex(
            data_frame=df, x.by=cont1, y.by=cont2, c(cont1, cont2),
            split.by = disc2, multivar.split.dir = "row")),
        "ggplot")

    expect_error(
        scatterHex(
            data_frame=df, x.by=cont1, y.by=cont2, c(disc, cont1, cont2)),
        "Only numeric", fixed = TRUE)

    expect_warning(
        scatterHex(
            data_frame=df, x.by=cont1, y.by=cont2, c(cont1, cont2),
            split.by = c(disc2,disc)),
        "will be ignored", fixed = TRUE)
})

##########
# Added Features
##########

test_that("scatterHex trajectory adding works", {
    expect_s3_class(
        scatterHex(
            data_frame=df, x.by=cont1, y.by=cont2, cont2,
            add.trajectory.by.groups = list(
                c("Adelie","Chinstrap","Gentoo")
                ),
            trajectory.group.by = disc),
        "ggplot")

    ### Manual Check: One large arrow.
    expect_s3_class(
        scatterHex(
            data_frame=df, x.by=cont1, y.by=cont2, cont2,
            add.trajectory.by.groups = list(
                c("Adelie","Chinstrap","Gentoo")),
            trajectory.group.by = disc,
            trajectory.arrow.size = 1),
        "ggplot")

    ### Manual Check: Arrows should be detached from points
    expect_s3_class(
        scatterHex(
            disc, data_frame=df, x.by=cont1, y.by=cont2,
            add.trajectory.curves = list(
                data.frame(
                    c(-10,0,-20),
                    c(-20,-10,0)),
                data.frame(
                    c(5:20),
                    c(5:10,9:5,6:10)
                ))),
        "ggplot")
})

test_that("scatterHex adding contours", {
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                                do.contour = TRUE),
                    "ggplot")

    ### Manual Check: Contour lines light blue and dashed
    expect_s3_class(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                                do.contour = TRUE,
                                contour.color = "lightblue", contour.linetype = "dashed"),
                    "ggplot")
})

test_that("scatterHex do.label/do.ellipse", {
    expect_s3_class(
        scatterHex(
            disc, data_frame=df, x.by=cont1, y.by=cont2,
            do.label = TRUE),
        "ggplot")
    expect_s3_class(
        scatterHex(
            disc, data_frame=df, x.by=cont1, y.by=cont2,
            do.ellipse = TRUE),
        "ggplot")
})

test_that("scatterHex ignores do.label/do.ellipse for continuous data", {
    expect_message(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                               do.label = TRUE),
                   "do.label was/were ignored for non-discrete data", fixed = TRUE)
    expect_message(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, cont2,
                               do.ellipse = TRUE),
                   "do.ellipse was/were ignored for non-discrete data", fixed = TRUE)

    # No message for discrete data && MANUAAL CHECK: ellipse is drawn
    expect_message(scatterHex(data_frame=df, x.by=cont1, y.by=cont2, disc,
                               do.ellipse = TRUE),
                   NA)
})

test_that("scatterHex labeling with numbers", {
    ### Manual Check
    # Number labels with matching legend
    expect_s3_class(
        scatterHex(
            disc, data_frame=df, x.by=cont1, y.by=cont2, do.label = TRUE,
            labels.use.numbers = TRUE),
        "ggplot")
    # _ instead of :
    expect_s3_class(
        scatterHex(
            disc, data_frame=df, x.by=cont1, y.by=cont2, do.label = TRUE,
            labels.use.numbers = TRUE,
            labels.numbers.spacer = "_"),
        "ggplot")
    # Colors should match with this original
    expect_s3_class(
        scatterHex(
            disc, data_frame=df, x.by=cont1, y.by=cont2, do.label = TRUE),
        "ggplot")
})

# adjustments
test_that("scatterPlot data adjustments applied", {
    expect_s3_class(
        (p <- scatterHex(
            cont1, data_frame = df, x.by=cont1, y.by=cont1, data.out = TRUE,
            x.adj.fxn=function(x) as.vector(scale(x)),
            y.adj.fxn=function(x) {round(as.vector(scale(x)), 0)},
            color.adjustment = "z-score"))$plot, "ggplot")
    expect_equal(
        p$data[[p$cols_used$y.by]],
        round(p$data[[p$cols_used$x.by]],0))
    expect_equal(
        round(mean(p$data[[p$cols_used$x.by]]),0),
        0)
    expect_equal(
        p$data[[p$cols_used$color.by]],
        p$data[[p$cols_used$x.by]])
    expect_s3_class(
        (p <- scatterHex(
            cont1, data_frame = df, x.by=cont1, y.by=cont1, data.out = TRUE,
            y.adjustment= "relative.to.max"))$plot, "ggplot")
    expect_equal(
        max(p$data[[p$cols_used$y.by]]), 1)
})

test_that("scatterHex added arbitrary horizontal, vertical, and diagonal lines work", {
    expect_s3_class(
        scatterHex(df, "PC1", "PC2", disc,
                    add.yline = c(-1, 1), add.xline = c(2)),
        "ggplot")

    ### Manual Check:
    # Vertical lines dotted and horizontal line red (not black and dashed as above)
    expect_s3_class(
        scatterHex(df, "PC1", "PC2", disc,
                    add.yline = c(-1, 1), add.xline = c(2),
                    yline.color = "red", xline.linetype = "dotted"),
        "ggplot")

    ### Manual Check:
    # split.by works with lines and ablines work
    expect_s3_class(
        scatterHex(df, "PC3", "PC2", disc, split.by = "groups",
            add.yline = c(-5, 5), add.xline = c(2),
            yline.color = "red", xline.linetype = "dotted",
            add.abline = c(5, 1.5), abline.slope = c(2, -3), 
            abline.linetype = "solid", abline.opacity = c(1, 0.5), abline.linewidth = c(1, 2), 
            abline.color = c("green", "blue")),
        "ggplot")
})

