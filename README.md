# dittoViz

This packagae is not fully functional yet.  Very much in a "Use at your own risk" state as I will not be shying away from making breaking changes at this early stage when I'm still determining the 'right' way to code things!

## Scope

This package will be a data.frame focused version of [dittoSeq](https://github.com/dtm2451/dittoSeq), where all the simplified plot setup and color blindness friendly tweaks afforded by dittoSeq can be applied to any data type, not just to omics data.

The two major goals for this package:

1. Carry over as much functionality from dittoSeq as makes sense for targeting data stored in a data.frame-type object (rather than in Seurat/SingleCellExperiemnt/SummarizedExperiment objects).
2. Python counterpart: a similarly named, largely equivalent, python companion which would target `pandas.DataFrame`s using similar syntax. (Similarity level is a question, due to divergent constraints on the R vs. python versions, but the priority is there.)
