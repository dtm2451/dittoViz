# dittoViz

A comprehensive visualization toolkit built with coders of all skill levels and
color-vision impaired audiences in mind. It allows creation of finely-tuned,
publication-quality figures from single function calls.

Spawned out of the scRNAseq-focused dittoSeq-package, dittoViz contains most of
it's visualization machinery with none of the added (sc)RNAseq-related
dependency requirements.

Visualizations include scatterplots, compositional bar plots, violin, box, and
ridge plots, and more. Customizations range from size and title adjustments to
discrete-group circling and labeling, hidden data overlay upon cursor hovering
via ggplotly conversion, and many more. All with simple, discrete inputs.

Color blindness friendliness is powered by legend adjustments (enlarged keys),
and by allowing the use of shapes or letter-overlay in addition to the carefully
selected dittoColors.

## Getting Started

A vignette detailing installation and use is available [here!](vignettes/intro.md)

## Long-Term plans for dittoViz

1. Carry over as much functionality from dittoSeq as makes sense for targeting data stored in a data.frame-type object (rather than in Seurat/SingleCellExperiemnt/SummarizedExperiment objects).
  - Much of this work is now complete! ... But there are still a few missing bits, the biggest of which are `dittoDotPlot()` and `dittoHeatmap()` counterparts.
2. Implement dittoViz inside of a dittoSeq-v2.0 after it gets accepted into Bioconductor.
3. Python counterpart: a similarly named, largely equivalent, python companion which would target `pandas.DataFrame`s using the `plotnine` ggplot-mimic and as similar syntax as possible.
