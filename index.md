## Automation and visualization of flow cytometry data analysis pipelines

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![R-CMD-check-bioc](https://github.com/UCLouvain-CBIO/CytoPipelineGUI/workflows/R-CMD-check-bioc/badge.svg)](https://github.com/UCLouvain-CBIO/CytoPipelineGUI/actions?query=workflow%3AR-CMD-check-bioc)
[![license](https://img.shields.io/badge/license-GPL3.0-blue)](https://opensource.org/licenses/GPL-3.0)

### What is CytoPipelineGUI?

`CytoPipelineGUI` is the companion package of `CytoPipeline`, and is
used for interactive visualization. It implements two shiny applications
: - a shiny app for interactive comparison of flow frames that are the
results of CytoProcessingSteps of the same or different CytoPipeline
experiments.  
It is launched using the following statement:
[`CytoPipelineCheckApp()`](https://uclouvain-cbio.github.io/CytoPipelineGUI/reference/CytoPipelineCheckApp.md)  
- a shiny app for interactive visualization and manual adjustments of
scale transformation objects. It is launched using the following
statement:
[`ScaleTransformApp()`](https://uclouvain-cbio.github.io/CytoPipelineGUI/reference/ScaleTransformApp.md)

### License

The `CytoPipelineGUI` code is provided under [GPL license version 3.0 or
higher](https://opensource.org/licenses/GPL-3.0). The documentation,
including the manual pages and the vignettes, are distributed under a
[CC BY-SA 4.0 license](https://creativecommons.org/licenses/by-sa/4.0/).

### Citation

If you use `CytopipelineGUI` in your research, please use the following
citation:

> Hauchamps P, Bayat B, Delandre S, Hamrouni M, Toussaint M, Temmerman
> S, Lin D, Gatto L (2024). “CytoPipeline and CytoPipelineGUI: a
> Bioconductor R package suite for building and visualizing automated
> pre-processing pipelines for flow cytometry data.” *BMC
> Bioinformatics*, *25*(1), 80. <doi:10.1186/s12859-024-05691-z>
> <https://doi.org/10.1186/s12859-024-05691-z>.

or run `citation("CytoPipelineGUI")` to get the bibtex entry.
