# Tests for scatterPlot function
# library(dittoViz); library(testthat); source("tests/testthat/setup.R"); for (i in list.files("R", pattern="^utils", full.names = TRUE)) source(i); source("tests/testthat/test-scatter.R")

df$number <- as.numeric(seq_len(nrow(df)))
cont <- "number"
cont2 <- "bill_length_mm"
disc <- "species"
disc2 <- "island"

test_that("scatterPlot can plot continuous or discrete data", {
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc),
        "ggplot")
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", cont),
        "ggplot")
})

test_that("scatterPlot basic tweaks work", {
    # Manuel Check: big dots
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", cont,
            size = 10),
        "ggplot")
    # Manuel Check: triangles
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", cont,
            shape.panel = 17),
        "ggplot")
    # Manuel Check: see through large dots
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", cont,
            size = 5,
            opacity = 0.5),
        "ggplot")
})

test_that("scatterPlot main legend can be removed or adjusted, color and shape", {

    ### Manual Check: Legend removed
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            legend.show = FALSE),
        "ggplot")

    ### Manual Check: Legend title = "WOW" & Legend symbols LARGE (color)
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            legend.color.size = 45,
            legend.color.title = "WOW"),
        "ggplot")
    ### Manual Check: Legend title = "WOW" & Legend symbols LARGE (shape)
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", shape.by = disc,
            legend.shape.size = 45,
            legend.shape.title = "WOW"),
        "ggplot")

    ### Manual Check: Legend breaks not evenly dispersed & labeled with given text
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", "number",
            legend.color.breaks = c(1, 50, 150, 200),
            legend.color.breaks.labels = c("1", "fifty", "one-fifty", "two hundred plus")),
        "ggplot")

    ### Manual Check: Legend breaks not evenly dispersed & labeled with given text
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", "number",
            min.value = -50,
            max.value = 1000),
        "ggplot")
})

test_that("scatterPlot color scale can be adjusted", {

    ### Manual Check:
    # Scale extends from below zero to 1000, all points in yellow to green range
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", "number",
            min.value = -50,
            max.value = 1000),
        "ggplot")

    ### Manual Check:
    # Gray-scale
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", "number",
            min.color = "black",
            max.color = "grey70"),
        "ggplot")
})

test_that("scatterPlots can be subset to show only certain rows with any rows.use method", {
    expect_s3_class(
        {c1 <- scatterPlot(
            df, "PC1", "PC2", disc, data.out = TRUE,
            rows.use = rows.names)
        c1$p},
        "ggplot")
    expect_s3_class(
        {c2 <- scatterPlot(
            df, "PC1", "PC2", disc, data.out = TRUE,
            rows.use = rows.logical)
        c2$p},
        "ggplot")
    expect_s3_class(
        {c3 <- scatterPlot(
            df, "PC1", "PC2", disc, data.out = TRUE,
            rows.use = rows.nums)
        c2$p},
        "ggplot")

    # All equal?
    expect_equal(c1$Target_data, c2$Target_data)
    expect_equal(c1$Target_data, c3$Target_data)
    expect_equal(nrow(c3$Target_data), 40)

    # And if we remove an entire grouping...
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            rows.use = df[[disc]]!="Adelie"),
        "ggplot")
})

test_that("scatterPlot shapes and colors can be adjusted", {
    ### Manual check:
    # Shapes should be triangle, diamond, circle
    # Color should be red, blue, yellow
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2",
            color.by = disc,
            shape.by = disc2,
            shape.panel = 17:19,
            color.panel = cols),
        "ggplot")

    # By names
    ### Manual check:
    # Both flipped compared to above
    # Shapes should be triangle, diamond, circle
    # Color should be red, blue, yellow
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2",
            color.by = disc,
            shape.by = disc2,
            shape.panel = c(Biscoe = 19, Dream = 18, Torgersen = 17),
            color.panel = c(Gentoo = cols[1], Chinstrap = cols[2], Adelie = cols[3])),
        "ggplot")

    # 'colors' input
    ### Manual check:
    # Pink, red, darker blue
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2",
            color.by = disc,
            colors = 7:5),
        "ggplot")
})

test_that("scatterPlots titles and theme can be adjusted", {
    ### Manual check: All titles should be adjusted.
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", cont,
            main = "Gotta catch", sub = "em all",
            xlab = "Pokemon", ylab = "Pokedex #s",
            legend.color.title = "types"),
        "ggplot")
    ### Manual check: top and right plot outline removed
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", cont,
            theme = theme_classic()),
        "ggplot")
})

test_that("scatterPlot allows plotting of multiple vars, via faceting", {
    expect_s3_class(
        scatterPlot(
            data_frame=df, x.by=cont, y.by=cont2, c(cont, cont2)),
        "ggplot")

    # These should have transposed facet grids
    expect_s3_class(
        print(scatterPlot(
            data_frame=df, x.by=cont, y.by=cont2, c(cont, cont2),
            split.by = disc2)),
        "ggplot")
    expect_s3_class(
        print(scatterPlot(
            data_frame=df, x.by=cont, y.by=cont2, c(cont, cont2),
            split.by = disc2, multivar.split.dir = "row")),
        "ggplot")

    # Works with rows.use
    expect_s3_class(
        scatterPlot(
            data_frame=df, x.by=cont, y.by=cont2, c(cont, cont2),
            rows.use = rows.logical),
        "ggplot")

    expect_error(
        scatterPlot(
            data_frame=df, x.by=cont, y.by=cont2, c(disc, cont, cont2)),
        "Only numeric", fixed = TRUE)

    expect_warning(
        scatterPlot(
            data_frame=df, x.by=cont, y.by=cont2, c(cont, cont2),
            split.by = c(disc2,disc)),
        "will be ignored", fixed = TRUE)
})

test_that("scatterPlots discrete labels can be adjusted", {
    ### Manual Check:
    # Legend labels = 5:7
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            rename.color.groups = 5:7),
        "ggplot")
    ### Manual Check:
    # Legend labels = 3:5
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2",
            shape.by = disc2,
            rename.shape.groups = 3:5),
        "ggplot")
})

test_that("scatterPlot can be labeled or circled", {
    ### Manual Check (next 2)
    # Labels should move around between plots, only boxed in 1st
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            do.label = TRUE),
        "ggplot")
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            do.label = TRUE,
            labels.highlight = FALSE),
        "ggplot")

    ### Manual Check (next 2)
    # No movement of lebels, only boxed in 1st
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            do.label = TRUE,
            labels.repel = FALSE),
        "ggplot")
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            do.label = TRUE,
            labels.highlight = FALSE,
            labels.repel = FALSE),
        "ggplot")

    ### Manual Check
    # smaller labels
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            do.label = TRUE,
            labels.size = 3),
        "ggplot")

    ### Manual Check
    # labels to right side
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            do.label = TRUE,
            labels.repel.adjust = list(xlim=c(5,NA))),
        "ggplot")
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            do.label = TRUE,
            labels.repel.adjust = list(xlim=c(5,NA)),
            labels.highlight = FALSE),
        "ggplot")
})

test_that("scatterPlot labeling is robust to NAs", {

    # In color data
    na_in_color <- df
    na_in_color$species[df$sex=="male"] <- NA
    expect_warning(
        scatterPlot(
            na_in_color, "PC1", "PC2", "species",
            do.label = TRUE),
        NA)

    na_in_pc1 <- df
    na_in_pc1$PC1[1:10] <- NA

    # Manual Check: Should be all four labels!
    # There should be a warning here form the x/y coords, but not the labeling
    expect_true(
        all(!grepl(
            "label",
            names(warnings(
                scatterPlot(na_in_pc1, "PC1", "PC2", "species", do.label = TRUE)
            ))
        ))
    )

})

test_that("scatterPlot trajectory adding works", {
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            add.trajectory.by.groups = list(
                c("Adelie","Chinstrap","Gentoo"),
                c("Gentoo","Adelie")),
            trajectory.group.by = disc,
            do.label = TRUE),
        "ggplot")
    # Manual Check: Arrows should move & GROW.
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", cont,
            add.trajectory.by.groups = list(
                c("Chinstrap","Adelie")),
            trajectory.group.by = disc,
            trajectory.arrow.size = 1),
        "ggplot")
    # Manual Check: Arrows should be detached from points
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            add.trajectory.curves = list(
                data.frame(
                    c(-20,20,-50),
                    c(-20,-10,0)),
                data.frame(
                    c(5:20)*10,
                    c(5:10,9:5,6:10)
                )),
            trajectory.group.by = disc),
        "ggplot")
})

test_that("scatterPlot lettering works", {
    ### Manual Check: Letters should be added
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            do.letter = TRUE, size = 3),
        "ggplot")
    ### Manual Check: see through dots and letters
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            do.letter = TRUE, size = 3,
            opacity = 0.5),
        "ggplot")
})

test_that("scatterPlot plotting order can be ordered by the data, or have order randomized", {
    un <- scatterPlot(df, "PC1", "PC2", disc, data.out = TRUE, size = 10, plot.order = "unordered")
    dec <- scatterPlot(df, "PC1", "PC2", disc, data.out = TRUE, size = 10, plot.order = "decreasing")
    inc <- scatterPlot(df, "PC1", "PC2", disc, data.out = TRUE, size = 10, plot.order = "increasing")
    set.seed(42) # Hopefully with 2 different seeds, we can ensure that 1 will diverge from the original
    ran <- scatterPlot(df, "PC1", "PC2", disc, data.out = TRUE, size = 10, plot.order = "randomize")
    set.seed(12345)
    ran2 <- scatterPlot(df, "PC1", "PC2", disc, data.out = TRUE, size = 10, plot.order = "randomize")
    ### Manual Check:
    # Orange always in front
    expect_s3_class(
        dec$p,
        "ggplot")
    ### Manual Check:
    # Plots different
    expect_s3_class(
        un$p,
        "ggplot")
    expect_s3_class(
        ran$p,
        "ggplot")
    expect_s3_class(
        ran2$p,
        "ggplot")
    expect_false(
        all(c(
            identical(
                rownames(un$Target_data),
                rownames(ran$Target_data)),
            identical(
                rownames(un$Target_data),
                rownames(ran2$Target_data))
        ))
    )
    ### Manual Check: Dark blue always in front
    expect_equal(
        dec$Target_data$color,
        rev(inc$Target_data$color)
    )
})

test_that("scatterPlot adding contours", {
    expect_s3_class(
        scatterPlot(df, "PC1", "PC2", disc,
                    do.contour = TRUE),
        "ggplot")

    ### Manual Check:
    # Contour lines light blue and dashed (not black as above)
    expect_s3_class(
        scatterPlot(df, "PC1", "PC2", disc,
                    do.contour = TRUE,
                    contour.color = "lightblue", contour.linetype = "dashed"),
        "ggplot")
})

test_that("scatterPlot with and without rasterization produces identical plots", {
    ### Manual Check:
    # These plots should appear nearly identical
    expect_s3_class(
        scatterPlot(df, "PC1", "PC2", disc,
                    do.raster = TRUE),
        "ggplot")

    expect_s3_class(
        scatterPlot(df, "PC1", "PC2", disc),
        "ggplot")
})

test_that("scatterPlot ignores do.letter/do.label/do.ellipse for continuous data", {
    expect_message(scatterPlot(df, "PC1", "PC2", cont,
                                do.label = TRUE),
                   "do.label was/were ignored for non-discrete data", fixed = TRUE)
    expect_message(scatterPlot(df, "PC1", "PC2", cont,
                                do.letter = TRUE),
                   "do.letter was/were ignored for non-discrete data", fixed = TRUE)
    expect_message(scatterPlot(df, "PC1", "PC2", cont,
                                do.ellipse = TRUE),
                   "do.ellipse was/were ignored for non-discrete data", fixed = TRUE)

    # No message for discrete data && MANUAL CHECK: ellipse is drawn
    expect_message(scatterPlot(df, "PC1", "PC2", disc,
                                do.ellipse = TRUE),
                   NA)
})

test_that("scatterPlot can be faceted with split.by (1 or 2 vars)", {
    # MANUAL CHECK: FACETING
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = disc2),
        "ggplot")
    # horizontal
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = disc2,
            split.nrow = 1),
        "ggplot")
    # vertical
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = disc2,
            split.ncol = 1),
        "ggplot")
    # Grid with rows=island, cols=species
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = c(disc2,disc)),
        "ggplot")
})

test_that("scatterPlot faceting and rows.use and split.show.all.others work together", {
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = c(disc2),
            rows.use = rows.logical,
            split.show.all.others = FALSE),
        "ggplot")
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = c(disc2,disc),
            rows.use = rows.logical,
            split.show.all.others = FALSE),
        "ggplot")

    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = c(disc2,disc),
            rows.use = rows.logical,
            split.show.all.others = TRUE),
        "ggplot")
    expect_s3_class(
        scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = c(disc2,disc),
            rows.use = rows.logical,
            split.show.all.others = TRUE),
        "ggplot")
})

test_that("scatterPlot added features work with single-column faceting", {
    expect_error(
        print(scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = disc2,
            do.label = TRUE,
            split.show.all.others = FALSE)),
        NA)
    expect_error(
        print(scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = disc2,
            do.ellipse = TRUE,
            split.show.all.others = FALSE)),
        NA)
    expect_error(
        print(scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = disc2,
            do.letter = TRUE,
            split.show.all.others = FALSE)),
        NA)
    expect_error(
        print(scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = disc2,
            do.contour = TRUE,
            split.show.all.others = FALSE)),
        NA)
    expect_error(
        print(scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = disc2,
            add.trajectory.by.groups = list(
                c("C","A")),
            trajectory.group.by = disc,
            split.show.all.others = FALSE)),
        NA)
    expect_error(
        print(scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = disc2,
            add.trajectory.curves = list(
                data.frame(
                    c(-10,0,-20),
                    c(-20,-10,0)),
                data.frame(
                    c(5:20),
                    c(5:10,9:5,6:10))),
            split.show.all.others = FALSE)),
        NA)
})

test_that("scatterPlot added features work with double-column faceting", {
    expect_error(
        print(scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = c(disc2,disc),
            do.label = TRUE,
            split.show.all.others = FALSE)),
        NA)
    expect_error(
        print(scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = c(disc2,disc),
            do.ellipse = TRUE,
            split.show.all.others = FALSE)),
        NA)
    expect_error(
        print(scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = c(disc2,disc),
            do.letter = TRUE,
            split.show.all.others = FALSE)),
        NA)
    expect_error(
        print(scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = c(disc2,disc),
            add.trajectory.by.groups = list(
                c("C","A")),
            trajectory.group.by = disc,
            split.show.all.others = FALSE)),
        NA)
    expect_error(
        print(scatterPlot(
            df, "PC1", "PC2", disc,
            split.by = c(disc2,disc),
            add.trajectory.curves = list(
                data.frame(
                    c(-10,0,-20),
                    c(-20,-10,0)),
                data.frame(
                    c(5:20),
                    c(5:10,9:5,6:10))),
            split.show.all.others = FALSE)),
        NA)
})

# adjustments
test_that("scatterPlot data adjustments applied", {
    expect_s3_class(
        (p <- scatterPlot(
            cont, data_frame = df, x.by=cont, y.by=cont, data.out = TRUE,
            x.adj.fxn=function(x) as.vector(scale(x)),
            y.adj.fxn=function(x) {round(as.vector(scale(x)), 0)},
            color.adjustment = "z-score"))$plot, "ggplot")
    expect_equal(
        p$Target_data[[p$cols_used$y.by]],
        round(p$Target_data[[p$cols_used$x.by]],0))
    expect_equal(
        round(mean(p$Target_data[[p$cols_used$x.by]]),0),
        0)
    expect_equal(
        p$Target_data[[p$cols_used$color.by]],
        p$Target_data[[p$cols_used$x.by]])
    expect_s3_class(
        (p <- scatterPlot(
            cont, data_frame = df, x.by=cont, y.by=cont, data.out = TRUE,
            y.adjustment= "relative.to.max"))$plot, "ggplot")
    expect_equal(
        max(p$Target_data[[p$cols_used$y.by]]), 1)
})
