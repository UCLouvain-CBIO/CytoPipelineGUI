## Automation and visualization of flow cytometry data analysis pipelines

[![Project Status: Active – The project has reached a stable, usable state and is being actively developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![license](https://img.shields.io/badge/license-GPL3.0-blue)](https://opensource.org/licenses/GPL-3.0)

### What is CytoPipelineGUI?

`CytoPipelineGUI` is the companion package of `CytoPipeline`, and is used for
interactive visualization. It implements two shiny applications :
- a shiny app for interactive comparison of flow frames that are the results 
of CytoProcessingSteps of the same or different CytoPipeline experiments.  
It is launched using the following statement: `CytoPipelineCheckApp()`  
- a shiny app for interactive visualization and manual adjustments of scale 
transformation objects. It is launched using the following statement: 
`ScaleTransformApp()` 

### License

The `CytoPipeline` code is provided under [GPL license version 3.0 or 
higher](https://opensource.org/licenses/GPL-3.0). The documentation, 
including the manual pages and the vignettes, are distributed under a [CC BY-SA 
4.0 license](https://creativecommons.org/licenses/by-sa/4.0/).
