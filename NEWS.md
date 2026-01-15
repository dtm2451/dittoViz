# dittoViz 1.0.6

* 'scatterPlot()' now allows 'opacity' to be given a string that names a column of 'data_frame', and point opacity will then vary based on the targeted data.
* 'do.hover' is now allowed for yPlot() ridge-plotting as plotly supports the ggridges geoms.
* BugFix: yPlot() 'legend.title': default filled in from 'color.by' instead of 'var' and now also left blank when multivar-use would override 'color.by'.
* BugFix: Respect factor level ordering of original data when setting the group aes of yPlot() 'jitter' representations. Previously, the ordering subgroups could become mismatched between jitter versus box and violin representations.
* BugFix: Setting the group aes of yPlot() 'jitter' representations now properly adapts to multivar-use when 'multivar.aes' is 'group' or 'color'

# dittoViz 1.0.5

(All relating to integrating dittoViz functions with dittoSeq)

* Minor warning and documentation adjustments.
* BugFix: ignore 'do.letter' when conflicts with 'shape.by' or 'do.hover' use
* BugFix: Allow 'labels.use.numbers = TRUE' to work alongside of 'legend.color.title = NULL'

# dittoViz 1.0.4

* Minor changes to ensure compatibility with ggplot2 version 4.0.0 changes. Notably, the 'MASS' package is now added as a suggested companion package, and error messages suggesting its installation were added for features which rely on functionality from this package. This change is concurrent with ggplot2 itself downgrading the tool from an imported requirement to a suggested addition.

# dittoViz 1.0.3

* Expanded flexibility when adding lines to plots: Adds ability to place sloped lines with scatterPlot() and scatterHex() via new 'add.abline' and 'abline.*' inputs, as well as previously unavailable opacity and lineweight control inputs for all existing 'add.line' variants.

# dittoViz 1.0.2

* For scatterPlot() and scatterHex(), added the ability to use numbers as data labels instead of the data-values themselves, because sometimes data-values are quite long. Controlled with new inputs 'labels.use.numbers' and 'labels.numbers.spacer'.
* Added new 'color.method = "prop.<value>"' functionality to scatterHex() which allows coloring bins by the proportion of a given 'color.by'-data value.
* BugFix: Let 'show.grid.lines = FALSE' turn off grid lines (by adjusting 'theme' before, instead of after, adding 'theme' to the growing plot).

# dittoViz 1.0.1

* Multiple improvements to 'do.hover' ggplotly() conversion functionality: 
  * BugFix: Fixed issue where 'yPlot()' jitter points appeared to get rank-based locations when using 'do.hover = TRUE', by explicitly setting the 'group' aesthetic.
  * Improved default data shown when 'do.hover = TRUE'. Defaults now capture all data columns given to 'var', 'group.by', 'x.by', 'split.by', etc. inputs, as well as all internally generated columns for when multiple 'var's or data adjustments are requested.
  * Added 'hover.round.digits' input to give control over the maximum number of decimal digits shown for numeric hover data.
* Added the ability to have vertical and horizontal lines drawn on top of scatterPlot() and scatterHex() plots, controlled via new inputs 'add.xline', 'xline.color', 'xline.linetype', 'add.yline', 'yline.color', and 'yline.linetype'
* Various behind the scenes updates to align with ggplot2-v3 recommendations and make warnings go away

# dittoViz 1.0.0

* Submitted to CRAN

# dittoViz 0.1.0

* Initialized package with 'scatterPlot()', 'yPlot()', 'barPlot()', 'freqPlot()', and 'scatterHex()' visualization and 'colLevels()' and 'dittoColors()' helper functions
