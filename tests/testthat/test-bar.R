# Tests for barPlot function
# library(dittoViz); library(testthat); source("tests/testthat/setup.R"); source("tests/testthat/test-bar.R")

grp1 <- "species"
grp2 <- "island"
grp3 <- "groups"

test_that("barPlot can quantify clustering of groupings in percent or raw count", {
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3),
        "ggplot")
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3, scale = "count"),
        "ggplot")
})

test_that("barPlots can be subset to show only certain rows with any rows.use method", {
    expect_s3_class(
        {c1 <- barPlot(
            df, grp2, group.by = grp3,  data.out = TRUE,
            rows.use = rows.names)
        c1$p},
        "ggplot")
    expect_s3_class(
        {c2 <- barPlot(
            df, grp2, group.by = grp3, data.out = TRUE,
            rows.use = rows.logical)
        c2$p},
        "ggplot")
    expect_equal(c1$data, c2$data)
    expect_s3_class(
        {c3 <- barPlot(
            df, grp2, group.by = grp3, data.out = TRUE,
            rows.use = rows.nums)
        c3$p},
        "ggplot")
    expect_equal(c1$data, c3$data)

    # And if we remove an entire X grouping...
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            rows.use = df[,grp3]!="A"),
        "ggplot")

    # And if we remove an entire var grouping...
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            rows.use = df[,grp2]!="Dream"),
        "ggplot")
})

test_that("barPlot main legend can be removed", {
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            legend.show = FALSE),
        "ggplot")
})

test_that("barPlots colors can be adjusted", {
    ### Manual check:
    # Next two should look the same.
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            color.panel = dittoColors()[5:1]),
        "ggplot")
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            colors = 5:1),
        "ggplot")

    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            color.panel = c("red","blue","yellow")),
        "ggplot")
})

test_that("barPlots titles and theme can be adjusted", {
    ### Manual check:
    # All titles should be adjusted.
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            main = "Gotta catch", sub = "em all",
            xlab = "Pokemon", ylab = "Pokedex #s",
            legend.title = "types"),
        "ggplot")
    ### Manual check:
    # Plot should be boxed
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            theme = theme_bw()),
        "ggplot")
})

test_that("barPlots y-axis can be adjusted", {
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            y.breaks = seq(0,1,0.25)),
        "ggplot")
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            min = -0.5, max = 2,
            y.breaks = seq(0,1,0.25)),
        "ggplot")
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            scale = "count",
            y.breaks = seq(0,45,15)),
        "ggplot")
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            scale = "count",
            min = -5, max = 100,
            y.breaks = seq(0,45,15)),
        "ggplot")
})

test_that("barPlot var-labels can be adjusted and reordered", {
    # Manual Check:
    # legend groups changed to pikachu, squirtle, ivysaur
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            var.labels.rename = c("pikachu", "squirtle", "ivysaur")),
        "ggplot")
    # Manual Check:
    # Torgerson on top now, orange
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            var.labels.reorder = 3:1),
        "ggplot")
})

test_that("barPlots x-labels can be adjusted", {
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            x.labels = 4:8),
        "ggplot")
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            x.reorder = 5:1),
        "ggplot")
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            x.labels.rotate = FALSE),
        "ggplot")
    ### Manual Check:
    # L->R 4:8, with horizontal labels
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            scale = "count",
            x.labels = 4:8, x.reorder = 5:1, x.labels.rotate = FALSE),
        "ggplot")
    ### Manual Check:
    # L->R 4:8, but order reversed compared to above
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            scale = "count",
            x.labels = 4:8, x.labels.rotate = FALSE),
        "ggplot")
})

test_that("barPlot can be faceted with 'split.by'", {
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            split.by = grp1),
        "ggplot")
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            split.by = c(grp1,grp3)),
        "ggplot")

    # Work with rows.use
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            split.by = grp1,
            rows.use = df$number<50),
        "ggplot")
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            split.by = c(grp1,grp3),
            rows.use = df$number<50),
        "ggplot")
})

test_that("'split.by' can be given extra features", {
    # MANUAL: white space utilized fully
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3, scale = "count", split.ncol = 1,
            rows.use = df[,grp3]=="A",
            split.by = grp1,
            split.adjust = list(scales = "free")
        ),
        "ggplot")
    # MANUAL: white space utilized fully
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3,
            split.by = c(grp1,grp3),
            split.adjust = list(scales = "free")),
        "ggplot")
})

test_that("barPlot, 'retain.factor.level' can be used to respect factor levels", {
    df$var_factor <- factor(
        df[,grp2],
        levels = rev(colLevels(grp2, df)))
    df$grp_factor <- factor(
        df[,grp3],
        levels = rev(colLevels(grp3, df)))

    # Manual Check:
    # 1. var and group.by ordering should be reverse of alpha-numeric
    # 2. Removed group.by group, C, should remain but have no data
    # 3. Removed var group, Dream, should remain in the legend but not appear in the data
    expect_s3_class(
        barPlot(
            df, "var_factor", group.by = "grp_factor",
            retain.factor.levels = TRUE,
            rows.use = df[,"grp_factor"]!="C" & df[,"var_factor"]!="Dream"),
        "ggplot")
})

test_that("barPlot arbitrary horizontal lines work", {
    # Manual Check:
    # Multiple lines can be added and their parameters individually adjusted
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3, add.line = c(0.25, 0.5), 
            line.color = c("purple", "cyan"), line.linewidth = 2, 
            line.opacity = 0.6),
        "ggplot")

    # Manual Check:
    # Same lines applied across all panels
    expect_s3_class(
        barPlot(
            df, grp2, group.by = grp3, add.line = c(0.25, 0.5), 
            line.color = c("purple", "cyan"), line.linewidth = 2, 
            line.opacity = 0.6, split.by = grp3),
        "ggplot")
})
