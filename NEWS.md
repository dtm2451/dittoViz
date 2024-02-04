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
