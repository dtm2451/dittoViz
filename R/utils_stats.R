.determine_color_to_group_relation <- function(
        data_frame,
        group.by,
        color.by
) {
    # This function determines the relation of requested group.by and color.by
    # Returns and Options are:
    # "same": Every group-level is exactly its own color-level. Can be from the
    #  exact same data being used for both, OR data with different labels but
    #  exactly 1:1 mapping
    # "sub": Color-values change within groups to create "sub"groupings. E.g.
    #  a treatment condition where samples are being grouped by some other
    #  feature.
    # "super": Color-values do not change within any individual group, but map
    #  1:many (color:groups) to provide "super"groupings. E.g. a category of
    #  samples where there are multiple observations per sample and group.by is
    #  given sample data while color.by is given category data.

    # Simplest "same" case which does not require any other checks
    if (group.by == color.by) {
        return("same")
    }

    # Collect all sets of colors per each group-level
    cols_per <- c()
    all_groups <- colLevels(group.by, data_frame)
    for (g in all_groups) {
        cols_per <- c(
            cols_per, colLevels(color.by, data_frame, data_frame[,group.by]==g)
        )
    }
    # Assess
    if (length(cols_per) > length(all_groups)) {
        # (many:1 or many:many)
        "sub"
    } else if (length(unique(cols_per)) < length(all_groups)) {
        # (1:many)
        "super"
    } else {
        # (1:1)
        "same"
    }
}
