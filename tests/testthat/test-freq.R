# Tests for freqPlot function
# library(dittoViz); library(testthat); source("tests/testthat/setup.R"); source("tests/testthat/test-freq.R")

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

disc <- "species"
disc_char <- "species_char"
df$species_char <- as.character(df$species)
samp <- "sample"
grp <- "sample_groups"
sgrp <- "sample_subgroups"

bad_grp <- "island"

# Function relies on combination of machinery of yPlot & barPlot, so
# tests here just need to address unique pieces.

test_that("freqPlot can quantify values in percent or raw count", {
    # MANUAL: ymax <= 1
    expect_s3_class(
        freqPlot(
            df, disc, sample.by = samp, group.by = grp, color.by = sgrp),
        "ggplot")
    # MANUAL: ymax >= 1
    expect_s3_class(
        freqPlot(
            df, disc, sample.by = samp, group.by = grp, color.by = sgrp,
            scale = "count"),
        "ggplot")
})

test_that("freqPlots can be subset to show only summarize certain rows with any rows.use method", {
    expect_s3_class(
        {p1 <- freqPlot(
            df, disc, sample.by = samp, group.by = grp,  data.out = TRUE,
            rows.use = rows.names)
        p1$p},
        "ggplot")
    expect_s3_class(
        {p2 <- freqPlot(
            df, disc, sample.by = samp, group.by = grp, data.out = TRUE,
            rows.use = rows.logical)
        p2$p},
        "ggplot")
    expect_equal(p1$data, p2$data)
    expect_s3_class(
        {p3 <- freqPlot(
            df, disc, sample.by = samp, group.by = grp, data.out = TRUE,
            rows.use = rows.nums)
        p3$p},
        "ggplot")
    expect_equal(p1$data, p3$data)

    # And if we remove an entire X grouping...
    expect_s3_class(
        freqPlot(
            df, disc, sample.by = samp, group.by = grp,
            rows.use = df[,grp]!=0),
        "ggplot")
    # And if we remove an entire var grouping...
    ### MANUAL CHECK:
    # Factor, should remain
    expect_s3_class(
        freqPlot(
            df, "species", sample.by = samp, group.by = grp,
            rows.use = df$species!="Adelie"),
        "ggplot")

    ### MANUAL CHECK:
    # non-factor, should be removed altogether
    df_edit <- df
    df_edit$species <- as.character(df_edit$species)
    expect_s3_class(
        freqPlot(
            df_edit, "species", sample.by = samp, group.by = grp,
            rows.use = df$species!="Adelie"),
        "ggplot")
})

test_that("freqPlot can trim to individual var-values with vars.use", {
    # MANUAL: single facet
    expect_s3_class(
        freqPlot(
            df, disc, sample.by = samp, group.by = grp,
            vars.use = "Adelie"),
        "ggplot")

    # MANUAL: should only be two facets
    expect_s3_class(
        freqPlot(
            df, disc, sample.by = samp, group.by = grp,
            vars.use = c("Adelie", "Chinstrap")),
        "ggplot")

    # MANUAL: Two facets, "A" & "B", same look otherwise as above
    expect_s3_class(
        freqPlot(
            df, disc, sample.by = samp, group.by = grp,
            vars.use = c("A", "B"),
            var.labels.rename = c("A","B","C")),
        "ggplot")
})

test_that("freqPlot can max.normalize the data", {
    # MANUAL: ymax = 1
    expect_s3_class(
        {p <- freqPlot(
            df, disc, sample.by = samp, group.by = grp,
            data.out = TRUE,
            max.normalize = TRUE)
        p$p},
        "ggplot")
    expect_true(all(
        c("count.norm", "percent.norm") %in% colnames(p$data)
    ))
})

test_that("freqPlot properly checks if samples vs grouping-data has mismatches", {
    expect_error(
        freqPlot(
            df, disc, sample.by = samp, group.by = bad_grp),
        "Unable to interpret 'group.by' with 'sample.by'. 'island' data does not map 1 per sample.", fixed = TRUE)
    expect_error(
        freqPlot(
            df, disc, sample.by = samp, group.by = grp,
            color.by = bad_grp),
        "Unable to interpret 'color.by' with 'sample.by'. 'island' data does not map 1 per sample.", fixed = TRUE)

    # No error if no sample.by given
    expect_s3_class(
        freqPlot(
            df, disc, group.by = grp,
            color.by = bad_grp),
        "ggplot")
})

test_that("freqPlot computes for all rows even when not factor and a sample has zero", {
    df_edit <- df
    df_edit$species_char[df_edit$species_char=="Adelie" & df$sample_subgroups=="sg5"] <- "Chinstrap"
    (p <- freqPlot(
        df_edit, disc_char, group.by = sgrp, sample.by = samp, data.out = TRUE,
        plots = "jitter"
    ))
    # Checking 0 value ensures existence + equal to 0
    expect_true(all(p$data$count[p$data$grouping=="sg5"&p$data$label=="Adelie"]==0))
})

test_that("freqPlot computes for only one grouping per sample", {
    data <- freqPlot(
        df, disc_char, sample.by = samp, group.by = sgrp, data.out = TRUE,
        plots = "jitter"
    )$data
    expect_true(all(
        rowSums(table(data$sample, data$grouping)!=0)==1
    ))
})

test_that("freqPlot errors meaningfully for NAs in group.by or color.by", {
    df_edit <- df
    df_edit$sample_groups[df$sample==df$sample[1]] <- NA
    expect_error(
        freqPlot(
            df_edit, disc, sample.by = samp, group.by = grp, color.by = sgrp),
        "Cannot calculate composition among grouping data containing NAs. Offending column: sample_groups",
        fixed = TRUE
    )
    df_edit <- df
    df_edit$sample[df$sample==df$sample[1]] <- NA
    expect_error(
        freqPlot(
            df_edit, disc, sample.by = "sample", group.by = grp, color.by = sgrp),
        "Cannot calculate composition among sub-grouping data containing NAs. Offending column: sample",
        fixed = TRUE
    )
    df_edit <- df
    df_edit$sample_subgroups[df$sample==df$sample[1]] <- NA
    expect_error(
        freqPlot(
            df_edit, disc, sample.by = samp, group.by = grp, color.by = "sample_subgroups"),
        "Cannot calculate composition among sub-grouping data containing NAs. Offending column: sample_subgroups",
        fixed = TRUE
    )
})
