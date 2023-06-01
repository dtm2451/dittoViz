# Tests setup

set.seed(1)
library(palmerpenguins)
df <- as.data.frame(penguins[!apply(penguins, 1, function(x) { any(is.na(x)) }), ])

rownames(df) <- paste0("row", rownames(df))

# Dimensionality reduction
pca <- prcomp(df[, c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")])
df <- cbind(df, pca$x)

de_df <- do.call(
    rbind, lapply(
        list(
            c("Adelie", "Chinstrap"),
            c("Adelie", "Gentoo"),
            c("Chinstrap", "Gentoo")),
        function(targs) {
            this <- do.call(
                rbind,
                lapply(
                    c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g"),
                    function(obs) {
                        g1s <- as.vector(df$species==targs[1])
                        g2s <- as.vector(df$species==targs[2])
                        new <- data.frame(
                            feature = obs,
                            comparison = paste0(targs[1], "_vs_", targs[2]),
                            median_g1 = median(df[[obs]][g1s]),
                            median_g2 = median(df[[obs]][g2s]),
                            stringsAsFactors = FALSE
                        )
                        new$median_fold_change <- new$median_g1 / new$median_g2
                        new$median_log2_fold_change <- log2(new$median_fold_change)
                        new$p <- wilcox.test(x=df[[obs]][g1s],
                                             y=df[[obs]][g2s])$p.value
                        new
                    })
                )
            this$`-log10(p)` <- -1* log10(this$p)
            this$fdr <- p.adjust(this$p, method = "fdr")
            this$`-log10(fdr)` <- -1* log10(this$fdr)
            this
        }
    )
)

# Additional 'random' observations
df$groups <- sample(c("A","B","C","D","E"), nrow(df), replace = TRUE)
df$age <- sample(c("1","2","3","4"), nrow(df), replace = TRUE)
df$number <- as.numeric(seq_len(nrow(df)))

# For rows.use subsetting checks
rows.nums <- sort(sample(seq_len(nrow(df)), 40))
rows.names <- rownames(df)[rows.nums]
rows.logical <- seq_len(nrow(df)) %in% rows.nums

# Alternative colors
cols <- c("red", "blue", "yellow", "green", "black", "gray", "white")

# Remove the unneeded external data
rm(pca)
