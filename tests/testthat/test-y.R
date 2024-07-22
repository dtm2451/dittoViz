# Tests for yPlot function
# library(dittoViz); library(testthat); source("tests/testthat/setup.R"); for (i in list.files("R", pattern="^utils", full.names = TRUE)) source(i); source("tests/testthat/test-y.R")

df$all <- "A"
grp <- "species"
clr <- "island"
clr2 <- "groups"
cont1 <- "number"
cont2 <- "bill_length_mm"

test_that("yPlot can plot continuous data with all plot types", {
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter")),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("ridgeplot", "jitter")),
        "ggplot")
})

test_that("yPlots can be subset to show only certain rows with any rows.use method", {
    expect_s3_class(
        {c1 <- yPlot(
            df, cont1, group.by = grp, data.out = TRUE,
            plots = c("vlnplot", "boxplot"),
            rows.use = rows.names)
        c1$p},
        "ggplot")
    expect_s3_class(
        {c2 <- yPlot(
            df, cont1, group.by = grp, data.out = TRUE,
            plots = c("vlnplot", "boxplot"),
            rows.use = rows.logical)
        c2$p},
        "ggplot")
    expect_s3_class(
        {c3 <- yPlot(
            df, cont1, group.by = grp, data.out = TRUE,
            plots = c("vlnplot", "boxplot"),
            rows.use = rows.nums)
        c3$p},
        "ggplot")
    expect_equal(c1$data,c2$data)
    expect_equal(c1$data,c3$data)
    # And if we remove an entire grouping...
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot"),
            rows.use = df[,grp]!=0),
        "ggplot")
})

test_that("yPlot main legend can be removed", {
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            legend.show = FALSE),
        "ggplot")
})

test_that("yPlot colors can be distinct from group.by", {
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            color.by = clr),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            color.by = clr2),
        "ggplot")
})

test_that("yPlot shapes can be a distinct from group.by", {
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            shape.by = grp),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            shape.by = clr2),
        "ggplot")
})

test_that("yPlot shapes can be adjusted in many ways", {
    ### Manual Check
    # Shapes should be triangles instead of dots
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            shape.panel = 17),
        "ggplot")
    ### Manual Check
    # Shapes should be dot and triangle first instead of dot and square
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            shape.by = clr2, shape.panel = 16:20),
        "ggplot")
    ### Manual Check
    # Shapes should be enlarged in the legend
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            shape.by = clr2, jitter.shape.legend.size = 15),
        "ggplot")
    ### Manual Check
    # Shapes legend should be removed, but color still there
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            shape.by = clr2, jitter.shape.legend.show = FALSE),
        "ggplot")
})

test_that("yPlots colors can be adjusted", {
    ### Manual check:
    # These two should look the same.
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot"),
            color.panel = dittoColors()[5:1]),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot"),
            colors = 5:1),
        "ggplot")
})

test_that("yPlots titles and theme can be adjusted", {
    ### Manual check:
    # All titles should be adjusted.
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            main = "Gotta catch", sub = "em all",
            xlab = "Pokemon", ylab = "Pokedex #s",
            legend.title = "groups"),
        "ggplot")
    ### Manual check:
    # plot should be boxed
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            theme = theme_bw()),
        "ggplot")
})

test_that("yPlots y-axis can be adjusted, (x for ridgeplots)", {
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            min = -5, max = 100),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            y.breaks = seq(10,60,10)),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            min = -50, max = 100),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            y.breaks = seq(10,60,10)),
        "ggplot")
})

test_that("yPlots x-labels can be adjusted, (y for ridgeplots)", {
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            x.labels = 5:7),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            x.reorder = 3:1),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            x.labels.rotate = FALSE),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            x.labels = 5:7),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            x.reorder = 3:1),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            x.labels.rotate = FALSE),
        "ggplot")
    ### Manual Check:
    # L->R, 5:7, with horizontal labels
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            x.labels = 5:7, x.reorder = 3:1, x.labels.rotate = FALSE),
        "ggplot")
    ### Manual Check:
    # B -> T, 5:7 with rotated labels
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            x.labels = 5:7, x.reorder = 3:1, x.labels.rotate = TRUE),
        "ggplot")
})

test_that("yPlot can have lines added and adjusted individually", {
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            add.line = 20),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            add.line = 20, line.linetype = "solid", line.color = "green"),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            add.line = 20, line.linetype = "solid", line.color = "green"),
        "ggplot")
    
    # Manual Check:
    # Multiple lines, one solid, one dashed, both green, first thick, second thin.
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            add.line = c(20, 300), line.linetype = c("solid", "dotdash"), 
            line.color = "green", line.linewidth = c(0.5, 2)),
        "ggplot")

    # Manual Check:
    # Lines applied across panels properly for both ridgeplot and other plots
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp, split.by = "species",
            plots = "ridgeplot",
            add.line = c(20, 300), line.linetype = c("solid", "dotdash"), 
            line.color = "green", line.linewidth = c(0.5, 2)),
        "ggplot")
    
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp, split.by = "species",
            add.line = c(20, 300), line.linetype = c("solid", "dotdash"), 
            line.color = "green", line.linewidth = c(0.5, 2)),
        "ggplot")
})

test_that("yPlot jitter adjustments work", {
    # Manual Check:
    # Large blue dots that, in the y-plot, wider spread than normal.
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp, plots = "jitter",
            jitter.size = 10, jitter.color = "blue", jitter.width = 1),
        "ggplot")
    # Manual Check:
    # Large blue dots
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp, plots = c("jitter","ridgeplot"),
            jitter.size = 10, jitter.color = "blue", jitter.width = 1),
        "ggplot")

    ### Manual Check: 1. jitters that touch / align with vlnplot widths.
    #                 2. jitters that far from touch, NOT aligned properly
    #                 3. jitters that far from touch. Tests control, by default, by the boxplot input.
    #                 4. jitters that far from touch. Tests control, by default, by the vlnplot input via the boxplot input.

    # 1. Defaults
    expect_s3_class(
        print(yPlot(
            df, cont1, group.by = "all",
            color.by = clr2, plots = c("vlnplot", "boxplot", "jitter"),
            shape.panel = 21, jitter.size = 2, vlnplot.scaling = "width")),
        "ggplot")

    # 2. jitters further apart, not aligned with other reps
    expect_s3_class(
        print(yPlot(
            df, cont1, group.by = "all",
            color.by = clr2, plots = c("vlnplot", "boxplot", "jitter"),
            shape.panel = 21, jitter.size = 2, vlnplot.scaling = "width",
            jitter.position.dodge = 2)),
        "ggplot")

    # 3. set by boxplot dodge, jitters far apart but aligned with boxplots
    expect_s3_class(
        print(yPlot(
            df, cont1, group.by = "all",
            color.by = clr2, plots = c("vlnplot", "boxplot", "jitter"),
            shape.panel = 21, jitter.size = 2, vlnplot.scaling = "width",
            boxplot.position.dodge = 2)),
        "ggplot")

    # 4. set by vlnplot.width. jitters not touching (other edit), but aligned with violins and boxes
    expect_s3_class(
        print(yPlot(
            df, cont1, group.by = "all",
            color.by = clr2, plots = c("vlnplot", "boxplot", "jitter"),
            shape.panel = 21, jitter.size = 2, vlnplot.scaling = "width",
            vlnplot.width = 1.5,
            jitter.width = 0.1)),
        "ggplot")
})

test_that("yPlot boxplot adjustments work", {
    ### Manual Check:
    # Blue boxplots that touch eachother, with jitter visible behind.
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp, plots = c("jitter", "boxplot"),
            boxplot.width = 1, boxplot.color = "blue", boxplot.fill = FALSE,
            boxplot.show.outliers = TRUE),
        "ggplot")
    ### Manual Check:
    # boxplots that overlap, with thick lines
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp, plots = c("jitter","boxplot"),
            color.by = clr,
            boxplot.width = 0.4, boxplot.position.dodge = 0.2,
            boxplot.lineweight = 2),
        "ggplot")
    df$with_outlier <- df$number
    df$with_outlier[50] <- 400
    ### Manual Check:
    # no outlier
    expect_s3_class(
        yPlot(
            df, "with_outlier", group.by = grp, plots = c("boxplot"),
            boxplot.show.outliers = FALSE),
        "ggplot")
    ### Manual Check:
    # giant outlier
    expect_s3_class(
        yPlot(
            df, "with_outlier", group.by = grp, plots = c("boxplot"),
            boxplot.show.outliers = TRUE, boxplot.outlier.size = 20),
        "ggplot")
})

test_that("yPlot violin plot adjustments work", {
    ### Manual Check:
    # Almost non-existent lines, with quite overlapping vlns.
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            vlnplot.lineweight = 0.1, vlnplot.width = 5),
        "ggplot")
    ### Manual Check:
    # The next three should look different from eachother:
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            vlnplot.scaling = "count"),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            vlnplot.scaling = "area"),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            vlnplot.scaling = "width"),
        "ggplot")
})

test_that("yPlot ridgeplot adjustments work", {
    ### Manual Check:
    # Almost non-existent lines, with quite overlapping ridges.
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            ridgeplot.lineweight = 0.1, ridgeplot.scale = 5),
        "ggplot")
    ### Manual Check:
    # Lots of space at the top.
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            ridgeplot.ymax.expansion = 5),
        "ggplot")
    ### Manual Check:
    # Histogram
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            ridgeplot.shape = "hist"),
        "ggplot")
    ### Manual Check:
    # Hist with narrower bins
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            ridgeplot.shape = "hist", ridgeplot.bins = 60),
        "ggplot")
    ### Manual Check:
    # Hist with even narrower bins
    expect_s3_class(
        yPlot(
            df, cont1, group.by = grp,
            plots = "ridgeplot",
            ridgeplot.shape = "hist", ridgeplot.bins = 60,
            ridgeplot.binwidth = 1),
        "ggplot")
})

test_that("yPlot can be facted with split.by (1 or 2 vars)", {
    # MANUAL CHECK:
    # FACETING
    expect_s3_class(
        yPlot(
            df, cont1, grp, grp,
            split.by = grp),
        "ggplot")
    # horizontal
    expect_s3_class(
        yPlot(
            df, cont1, grp, grp,
            split.by = grp,
            split.nrow = 1),
        "ggplot")
    # vertical
    expect_s3_class(
        yPlot(
            df, cont1, grp, grp,
            split.by = grp,
            split.ncol = 1),
        "ggplot")
    # Grid with rows=age, cols=groups
    expect_s3_class(
        yPlot(
            df, cont1, grp, grp,
            split.by = c(grp,clr)),
        "ggplot")
    # Works with rows.use (should have grey points)
    expect_s3_class(
        yPlot(
            df, cont1, grp, grp,
            split.by = c(grp,clr),
            rows.use = rows.logical),
        "ggplot")
})

test_that("yPlot with and without jitter rasterization produces identical plots", {
    # MANUAL CHECK:
    # Should be indentical
    expect_s3_class(
        yPlot(
            df, cont1, grp, grp,
            do.raster = TRUE),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, grp, grp),
        "ggplot")

    # Jitter in front.
    expect_s3_class(
        yPlot(
            df, cont1, grp, grp,
            do.raster = TRUE,
            plots = c("vlnplot", "jitter")),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, cont1, grp, grp,
            plots = c("vlnplot", "jitter")),
        "ggplot")

    # Should be lower resolution
    expect_s3_class(
        yPlot(
            df, cont1, grp, grp,
            do.raster = TRUE,
            raster.dpi = 10,
            plots = c("vlnplot", "jitter")),
        "ggplot")
})

test_that("yPlot allows plotting of multiple vars, via faceting (deault)", {
    expect_s3_class(
        yPlot(
            df, c(cont1, cont2), grp),
        "ggplot")

    # Works with rows.use
    expect_s3_class(
        yPlot(
            df, c(cont1, cont2), grp,
            rows.use = rows.logical),
        "ggplot")

    # These should have transposed facet grids
    expect_s3_class(
        print(yPlot(
            df, c(cont1, cont2), grp,
            split.by = clr)),
        "ggplot")
    expect_s3_class(
        print(yPlot(
            df, c(cont1, cont2), grp,
            split.by = clr, multivar.split.dir = "row")),
        "ggplot")

    expect_warning(
        yPlot(
            df, c(cont1, cont2), grp,
            split.by = c(clr,clr2)),
        "will be ignored", fixed = TRUE)
})

test_that("yPlot allows plotting of multiple vars, via group or color", {
    expect_s3_class(
        print(yPlot(
            df, c(cont1, cont2), grp,
            multivar.aes = "group")),
        "ggplot")
    expect_s3_class(
        print(yPlot(
            df, c(cont1, cont2), grp,
            multivar.aes = "color")),
        "ggplot")

    # Works with rows.use
    expect_s3_class(
        yPlot(
            df, c(cont1, cont2), grp,
            multivar.aes = "group",
            rows.use = rows.logical),
        "ggplot")
    expect_s3_class(
        yPlot(
            df, c(cont1, cont2), grp,
            multivar.aes = "color",
            rows.use = rows.logical),
        "ggplot")
})
