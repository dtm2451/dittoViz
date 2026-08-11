# Tests for yPlot function
# library(dittoViz); library(testthat); source("tests/testthat/setup.R"); for (i in list.files("R", pattern="^utils", full.names = TRUE)) source(i); source("tests/testthat/test-stat.R")

df$all <- "A"
grp <- "species"
clr <- "island"
clr2 <- "groups"
cont1 <- "number"
cont2 <- "bill_length_mm"

all_comps <- unname(as.list(as.data.frame(combn(colLevels(grp,df), 2))))
comp1 <- all_comps[1]
comp1_vector <- all_comps[[1]]

# add.pvalues = NULL,
# pvalues.round.digits = 4,
# pvalues.test.method = "wilcox.test",
# pvalues.test.adjust = list(),
# pvalues.adjust = TRUE,
# pvalues.adjust.method = "fdr",
# pvalues.offset.first = 0.1,
# pvalues.offset.between = 0.2,
# pvalues.offset.above = 0.1,
# pvalues.do.fc = FALSE,
# pvalues.fc.pseudocount = 0,

# Test that default works
test_that("can calculate stats for yPlots (explicit, single, comparison set)", {
    no_stat <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        data.out = TRUE)
    yes_stat <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = comp1,
        data.out = TRUE)
    expect_false("stats" %in% names(no_stat))
    expect_true("stats" %in% names(yes_stat))
    expect_s3_class(
        yes_stat$p,
        "ggplot")

    # Can be given as vector when just 1 comb, and matches list format
    expect_equal(
        yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            add.pvalues = comp1_vector,
            data.out = TRUE)$stats,
        yes_stat$stats
    )

    # Can use "all"
    expect_gt(
        nrow(yPlot(
            df, cont1, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            add.pvalues = "all",
            data.out = TRUE)$stats),
        nrow(yes_stat$stats)
    )
})

test_that("can calculate stats with the test method adjusted", {
    wilcox_stat <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        data.out = TRUE)
    t_stat <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.test.method = "t.test",
        data.out = TRUE)
    wilcox_stat_alt <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.test.adjust = list(alternative = "greater"),
        data.out = TRUE)
    expect_true("stats" %in% names(wilcox_stat))
    expect_true("stats" %in% names(t_stat))
    expect_true("stats" %in% names(wilcox_stat_alt))

    expect_false(
        identical(
            wilcox_stat$stats,
            t_stat$stats
        )
    )
    expect_false(
        identical(
            wilcox_stat$stats,
            wilcox_stat_alt$stats
        )
    )
})

test_that("can control rounding in stats calc/plotting", {
    default_stat <- yPlot(
        df, cont2, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        data.out = TRUE)
    round0 <- yPlot(
        df, cont2, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.round.digits = 0,
        data.out = TRUE)
    round10 <- yPlot(
        df, cont2, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.round.digits = 0,
        data.out = TRUE)
    # Shorter when rounded to 0 decimals
    expect_equal(round0$stats$p_show, round(round0$stats$padj, 0))
    # Rounding by default
    expect_gte(
        sum(default_stat$stats$p_show - floor(default_stat$stats$p_show)),
        sum(round0$stats$p_show - floor(round0$stats$p_show))
    )
})

test_that("can control multiple-hypothesis adjustment in stats calculation", {
    default_stat <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.round.digits = Inf,
        data.out = TRUE)
    no_adj <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.adjust = FALSE,
        pvalues.round.digits = Inf,
        data.out = TRUE)
    bonf_adj <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.adjust.method = "bonferroni",
        pvalues.round.digits = Inf,
        data.out = TRUE)
    expect_true("stats" %in% names(no_adj))
    expect_false("padj" %in% names(no_adj$stats))
    expect_true("stats" %in% names(bonf_adj))

    # Method adjusted
    expect_false(
        identical(
            default_stat$stats$padj,
            bonf_adj$stats$padj
        )
    )

    # p_show is based on p when adjustment is off
    expect_equal(
        default_stat$stats$p_show, default_stat$stats$padj
    )
    expect_equal(
        no_adj$stats$p_show, no_adj$stats$p
    )
})

test_that("can adjust stats plotting heights", {
    default_stat <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.round.digits = Inf,
        data.out = TRUE)
    first_up <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.offset.first = 0.3,
        data.out = TRUE)
    btwn_up <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.offset.between = 0.3,
        data.out = TRUE)

    # first
    expect_true(all(
        first_up$stats$offset > default_stat$stats$offset
    ))
    # between
    expect_equal(
        btwn_up$stats$offset[1], default_stat$stats$offset[1]
    )
    expect_true(all(
        btwn_up$stats$offset[-1] > default_stat$stats$offset[-1]
    ))
    # ToDo: test pvalues.offset.above
})

test_that("can control multiple-hypothesis adjustment in stats calculation", {
    default_stat <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.round.digits = Inf,
        data.out = TRUE)
    no_adj <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.adjust = FALSE,
        pvalues.round.digits = Inf,
        data.out = TRUE)
    bonf_adj <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.adjust.method = "bonferroni",
        pvalues.round.digits = Inf,
        data.out = TRUE)
    expect_true("stats" %in% names(no_adj))
    expect_false("padj" %in% names(no_adj$stats))
    expect_true("stats" %in% names(bonf_adj))

    # Method adjusted
    expect_false(
        identical(
            default_stat$stats$padj,
            bonf_adj$stats$padj
        )
    )

    # p_show is based on p when adjustment is off
    expect_equal(
        default_stat$stats$p_show, default_stat$stats$padj
    )
    expect_equal(
        no_adj$stats$p_show, no_adj$stats$p
    )
})

test_that("can add median fold-change comparison in stats calculation", {
    default_fc <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.do.fc = TRUE,
        data.out = TRUE)
    pseudo_fc <- yPlot(
        df, cont1, group.by = grp,
        plots = c("vlnplot", "boxplot", "jitter"),
        add.pvalues = "all",
        pvalues.do.fc = TRUE,
        pvalues.fc.pseudocount = 0.01,
        data.out = TRUE)
    expect_true("stats" %in% names(default_fc))
    expect_true(all(c("median_g1", "median_g2", "median_fold_change", "median_log2_fold_change") %in% names(default_fc$stats)))

    # Pseudocount added
    expect_false(
        identical(
            default_fc$stats$median_fold_change,
            pseudo_fc$stats$median_fold_change
        )
    )
})

### ToDo:
# expected errors
# color and group
