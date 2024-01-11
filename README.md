# dittoViz

A comprehensive visualization toolkit built with coders of all skill levels and
color-vision impaired audiences in mind. It allows creation of finely-tuned,
publication-quality figures from single function calls.

Spawned out of the scRNAseq-focused [dittoSeq](https://github.com/dtm2451/dittoSeq)-package,
dittoViz contains most of it's visualization machinery with none of the added
scRNAseq-related dependencies.

Visualizations include scatter plots, compositional bar plots, violin, box, and
ridge plots, and more. Customization ranges from size and title adjustments to
discrete-group circling and labeling, hidden data overlay upon cursor hovering
via ggplotly() conversion, and many more, all with simple, discrete inputs.

Color blindness friendliness is powered by legend adjustments (enlarged keys),
and by allowing the use of shapes or letter-overlay in addition to the carefully
selected dittoColors().

## Getting Started

Installation:

```
install.packages("dittoViz")
```

Then there are a few options for getting aquainted with the package:

1. Read and follow along with the vignette available [here!](vignettes/intro.md)
2. View documentation and run examples inside R with, e.g. `?yPlot` or
`example("yPlot")`.

Visualizations functions are:

- `scatterPlot()`
- `scatterHex()`
- `yPlot()`
- `barPlot()`
- `freqPlot()`

Helper functions are:

- `dittoColors()`
- `colLevels()`

## Long-Term plans for dittoViz

1. Carry over as much functionality from dittoSeq as makes sense for targeting data stored in a data.frame-type object (rather than in Seurat/SingleCellExperiemnt/SummarizedExperiment objects).
  - Much of this work is now complete! ... But there are still a few missing bits, the biggest of which are `dittoDotPlot()` and `dittoHeatmap()` counterparts.
2. Implement dittoViz inside of a dittoSeq-v2.0 after it gets accepted into Bioconductor.
3. Python counterpart: a similarly named, largely equivalent, python companion which would target `pandas.DataFrame`s using the `plotnine` ggplot-mimic and as similar syntax as possible.
