# Tests for visualization functions
# library(dittoViz); library(testthat); source("tests/testthat/setup.R"); source("R/utils_colgetters.R"); source("R/utils_plot_mods.R"); source("R/utils.R"); source("tests/testthat/test-getters.R")

test_that(".is_col tells if column exists", {
    expect_false(.is_col("HELLO", df))
    expect_true(.is_col("number", df))
})

test_that("._col grabs columns and can run adjustments", {
    expect_type(
        ._col("number", df),
        "double")
    expect_type(
        ._col("number", df, adjustment = "z-score"),
        "double")
    expect_equal(
        0,
        mean(._col("number", df, adjustment = "z-score")))
    expect_equal(
        1,
        max(._col("number", df, adjustment = "relative.to.max")))
    expect_equal(
        factor(._col("number", df)),
        ._col("number", df, adj.fxn = function(x) {factor(x)}))
})

test_that("._col adds rownames if wanted", {
    expect_type(
        names(._col("number", df, add.names = TRUE)),
        "character")
    expect_true(
        identical(
            names(._col("number", df, add.names = FALSE)),
            NULL
        )
    )
})

test_that("colLevels gives unique values of a column, character or factor", {
    expect_equal(
        sort(colLevels("groups", df)),
        sort(unique(df[,"groups"]))
    )

    df$groups_fac <- factor(df$groups)
    expect_equal(
        sort(colLevels("groups_fac", df)),
        sort(levels(df[,"groups_fac"]))
    )
})

test_that("colLevels' rows.use & used.only adjust levels returned as expected", {
    expect_type(groups <- colLevels("groups", df),
                "character")
    df$groups_fac <- factor(df$groups)

    # rows.use can remove a level for both chacrater and factors
    expect_gt(
        length(groups),
        length(colLevels("groups", df,
                          rows.use = df$groups!=groups[1]))
    )
    expect_gt(
        length(groups),
        length(colLevels("groups_fac", df,
                          rows.use = df$groups_fac!=groups[1]))
    )

    # Unused levels retained for factor when used.only = FALSE
    expect_equal(
        length(groups),
        length(colLevels("groups_fac", df,
                          rows.use = df$groups_fac!=groups[1],
                          used.only = FALSE))
    )
})

test_that("._col and colLevels give error when given a non-col", {
    expect_error(._col("a", df),
                 "is not a column of 'data_frame'", fixed = TRUE)
    expect_error(colLevels("a", df),
                 "is not a column of 'data_frame'", fixed = TRUE)
})

test_that(".which_rows converts non-string rows.use to string", {
    expect_equal(
        .which_rows(1:10, df),
        rownames(df)[1:10])

    logical <- rep(FALSE, nrow(df))
    logical[1:10] <- TRUE
    expect_equal(.which_rows(logical, df), rownames(df)[1:10])
})

test_that(".which_rows errors when logical 'rows.use' is the wrong length", {
    expect_error(.which_rows(TRUE, df),
                 "'rows.use' length must equal the number of rows in 'data_frame' when given in logical form",
                 fixed = TRUE)
})

test_that(".rename_and_or_reorder can rename & reorder",{
    # relabel
    expect_equal(
        as.character(.rename_and_or_reorder(as.character(1:4), relabels = 4:1)),
        as.character(4:1))
    expect_error(
        .rename_and_or_reorder(as.character(1:4), relabels = 3:1),
        "incorrect number of labels provided to 'relabel' input", fixed = TRUE)
    # reorder
    expect_equal(
        levels(.rename_and_or_reorder(as.character(1:4), reorder = 4:1)),
        as.character(4:1))
    expect_error(
        .rename_and_or_reorder(as.character(1:4), reorder = 3:1),
        "incorrect number of indices provided to 'reorder' input", fixed = TRUE)
})
