# CytoPipelineGUI - 
# Copyright (C) <2022> <Université catholique de Louvain (UCLouvain), Belgique>
#   
#   Description and complete License: see LICENSE file.
# 
# This program (CytoPipelineGUI) is free software: 
#   you can redistribute it and/or modify it under the terms of the GNU 
#   General Public License as published by the Free Software Foundation, 
#   either version 3 of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details (<http://www.gnu.org/licenses/>).

#' @title interactive display and modification of scale transform list
#' @description this application allows the user to visualize a scale 
#' transformation list, possibly amending it channel after channel, 
#' and save the results on disk. 
#' The needed input tranformation list and flow frame for visualization needs
#' to be read from a CytoPipeline experiments stored in cache.
#' @param dir the root directory into which the engine will look for existing
#' CytoPipeline experiments
#' @return nothing
#' @import shiny
#' @import CytoPipeline
#' @export
#' @examples
#' 
#' # run CytoPipeline object first
#' 
#' outputDir <- base::tempdir()
#' 
#' 
#' rawDataDir <-
#'     system.file("extdata", package = "CytoPipeline")
#' experimentName <- "OMIP021_PeacoQC"
#' sampleFiles <- 
#'     file.path(rawDataDir, list.files(rawDataDir, pattern = "Donor"))
#' jsonDir <- system.file("extdata", package = "CytoPipeline")
#' jsonPath <- file.path(jsonDir, "pipelineParams.json")
#' 
#' pipL2 <- 
#'     CytoPipeline(
#'         jsonPath,
#'         experimentName = experimentName,
#'         sampleFiles = sampleFiles)
#' 
#' suppressWarnings(execute(
#'     pipL2,
#'     rmCache = TRUE,
#'     path = outputDir))
#'
#' # run shiny app
#'
#' #ScaleTransformApp(dir = outputDir)                     
#' 
ScaleTransformApp <- function(dir = ".") {
    if (interactive()) {
        path <- dir
        
        # initialize input lists
        experimentNames <-
            getCytoPipelineExperimentNames(path = path)
        if (length(experimentNames) == 0) {
            stop("no experiment found in current directory!")
        }
        ui <- fluidPage(
            # App title ----
            titlePanel("Manual scale transformations adjustments"),
            scaleTransformUI(
                id = "scaleTransformUI",
                experimentNames = experimentNames,
                enableTransfoListUpdate = TRUE, 
                enableTransfoListSave = TRUE)
        )
        
        server <- function(input, output, session) {
            
            scaleTransformServer(
                id = "scaleTransformUI",
                path = path)
        }
        
        shinyApp(ui, server)
    }
    
}

#' @title interactive visualization of flow cytometry data analysis pipeline 
#' objects stored in cache
#'
#' @param dir the root directory into which the engine will look for existing
#' CytoPipeline experiments
#' @return no return value
#' @import shiny
#' @import CytoPipeline
#' @export
#' @examples
#' 
#' # run CytoPipeline object first
#' 
#' outputDir <- base::tempdir()
#' 
#' 
#' rawDataDir <-
#'     system.file("extdata", package = "CytoPipeline")
#' experimentName <- "OMIP021_PeacoQC"
#' sampleFiles <- 
#'     file.path(
#'         rawDataDir, 
#'         list.files(
#'             rawDataDir, 
#'             pattern = "Donor"))
#' jsonDir <- system.file("extdata", package = "CytoPipeline")
#' jsonPath <- file.path(jsonDir, "pipelineParams.json")
#' 
#' pipL2 <- CytoPipeline(
#'     jsonPath,
#'     experimentName = experimentName,
#'     sampleFiles = sampleFiles)
#' 
#' suppressWarnings(execute(
#'     pipL2,
#'     rmCache = TRUE,
#'     path = outputDir))
#'
#' # run shiny app
#'
#' # CytoPipelineCheckApp(dir = outputDir)                    
#'
CytoPipelineCheckApp <-  function(dir = ".") {
    if (interactive()) {
        path <- dir
        # initialize input lists
        experimentNames <-
            getCytoPipelineExperimentNames(path = path)
        if (length(experimentNames) == 0) {
            stop("no experiment found in current directory!")
        }
        
        ui <- 
            navbarPage(
                "CytoPipeline check", id = "tabs",
                tabPanel(
                    "Experiments",
                    splitLayout(
                        fluidPage(
                            selectInput(
                                inputId = "experimentFrom",
                                label = "experiment:",
                                choices = experimentNames),
                            selectInput(
                                inputId = "whichQueueFrom",
                                label = "processing queue:",
                                choices = c("pre-processing",
                                            "scale transform")),
                            conditionalPanel(
                                condition = 
                                    "input.whichQueueFrom == 'pre-processing'",
                                selectInput(
                                    inputId = "sampleFrom",
                                    label = "sample:",
                                    choices = c(" "))
                            ),
                            plotOutput("workflowPlotFrom")
                        ),
                        fluidPage(
                            selectInput(
                                inputId = "experimentTo",
                                label = 
                                    "experiment (for comparisons):",
                                choices = experimentNames),
                            selectInput(
                                inputId = "whichQueueTo",
                                label = "processing queue:",
                                choices = c("pre-processing",
                                            "scale transform")),
                            conditionalPanel(
                                condition = 
                                    "input.whichQueueTo == 'pre-processing'",
                                selectInput(
                                    inputId = "sampleTo",
                                    label = "sample:",
                                    choices = c(" "))
                            ),
                            plotOutput("workflowPlotTo")
                            
                        )
                    )
                ),
                tabPanel(
                    "flow frames compare",
                    splitLayout(
                        fluidPage(
                            selectInput(
                                inputId = "flowFrameFrom",
                                label = "flow frame:",
                                choices = c(" ")),
                            selectInput(
                                inputId = "xchannelFrom",
                                label = "x channel/marker:",
                                choices = c(" ")),
                            selectInput(
                                inputId = "ychannelFrom",
                                label = "y channel/marker:",
                                choices = c(" ")),
                            plotOutput("ffPlotFrom")
                        ),
                        fluidPage(
                            selectInput(
                                inputId = "flowFrameTo",
                                label = "flow frame: (for comparison)",
                                choices = c(" ")),
                            selectInput(
                                inputId = "xchannelTo",
                                label = "x channel/marker:",
                                choices = c(" ")),
                            selectInput(
                                inputId = "ychannelTo",
                                label = "y channel/marker:",
                                choices = c(" ")),
                            plotOutput("ffPlotTo")
                        )
                    ),
                    
                    div(
                        plotly::plotlyOutput("ffDiffPlotly", width = "50%"), 
                        align = "center")
                ),
                tabPanel(
                    "scale transforms",
                    scaleTransformUI(
                        id = "scaleTransformUI",
                        experimentNames = experimentNames,
                        enableTransfoListUpdate = FALSE, 
                        enableTransfoListSave = FALSE),
                ),
                navbarMenu(
                    "More",
                    tabPanel(
                        "flowframe plots settings", 
                        fluidPage(
                            checkboxInput(
                                inputId = "useAllSampleEvents",
                                label = "Use all sample events:",
                                value = FALSE),
                            conditionalPanel(
                                condition = "!input.useAllSampleEvents",
                                numericInput(
                                    inputId = "nSubSampleEvents",
                                    label = 
                                        "Max nb of displayed events:",
                                    value = 10000,
                                    min = 100,
                                    max = NA)),
                            checkboxInput(
                                inputId = "useFixedLinearRange",
                                label = "Use fixed linear range:",
                                value = FALSE),
                            conditionalPanel(
                                condition = 
                                    "input.useFixedLinearRange",
                                numericInput(
                                    inputId = "minValueLinearRange",
                                    label = "Min:",
                                    value = -100,
                                    min = -50000,
                                    max = 262143),
                                numericInput(
                                    inputId = "maxValueLinearRange",
                                    label = "Max:",
                                    value = 262144,
                                    min = 100,
                                    max = NA)
                            ),
                            checkboxInput(
                                inputId = "useSelectedTransfo",
                                label = 
                                    "Use scale transformation list object:",
                                value = FALSE)
                        )),
                    #   "------",
                    #   tabPanel("About", "About page")
                )
                
            ) # end navbarPage
        
        
        server <- function(input, output, session) {
            
            observeEvent(input$experimentFrom, {
                #message(paste0("obs event: experimentFrom"))
                
                # update available processing queues, as well as
                # list of samples, based on selected experiment
                updateExperiment(
                    newExperimentName = input$experimentFrom,
                    whichQueueName = "whichQueueFrom",
                    currentWhichQueueValue = input$whichQueueFrom,
                    sampleListName = "sampleFrom",
                    currentSampleValue = input$sampleFrom,
                    path = path)
                
                # update list of available FF objects for FF comparison
                # note we force sampleFile to be " ", 
                # because it is automatically the current choice, 
                # and to avoid ghost sample file flowing
                updateFFList(
                    experimentName = input$experimentFrom,
                    whichQueue = input$whichQueueFrom,
                    sampleFile = input$sampleFrom,
                    path = path,
                    inputId = "flowFrameFrom",
                    currentValue = input$flowFrameFrom)
                
                # update experiment for comparison 
                # (by default, but only if no currently selected experiment)
                if(input$experimentTo == " ") {
                    updateSelectInput(
                        inputId = "experimentTo",
                        selected = input$experimentFrom)
                }
                
                #message(paste0("end obs event: experimentFrom"))
            })
            
            observeEvent(input$experimentTo, {
                #message(paste0("obs event: experimentTo"))

                # update available processing queues, as well as
                # list of samples, based on selected experiment
                updateExperiment(
                    newExperimentName = input$experimentTo,
                    whichQueueName = "whichQueueTo",
                    currentWhichQueueValue = input$whichQueueTo,
                    sampleListName = "sampleTo",
                    currentSampleValue = input$sampleTo,
                    path = path)
                
                # update list of available FF objects
                # note we force sampleFile to be " ", 
                # because it is automatically the current choice, 
                # and to avoid ghost sample file flowing
                updateFFList(
                    experimentName = input$experimentTo,
                    whichQueue = input$whichQueueTo,
                    sampleFile = input$sampleTo,
                    path = path,
                    inputId = "flowFrameTo",
                    currentValue = input$flowFrameTo)
                
                #message(paste0("end obs event: experimentTo"))
            })
            
            observeEvent(input$whichQueueFrom, {
                #message(paste0("obs event: whichQueueFrom"))
                
                # update list of available FF objects
                
                updateFFList(
                    experimentName = input$experimentFrom,
                    whichQueue = input$whichQueueFrom,
                    sampleFile = input$sampleFrom,
                    path = path,
                    inputId = "flowFrameFrom",
                    currentValue = input$flowFrameFrom)
                
                # update which queue for comparison (default)
                updateSelectInput(
                    inputId = "whichQueueTo",
                    selected = input$whichQueueFrom)
                #message(paste0("end obs event: whichQueueFrom"))
            })
            
            observeEvent(input$whichQueueTo, {
                #message(paste0("obs event: whichQueueTo"))
                
                # update list of available FF objects
                
                updateFFList(
                    experimentName = input$experimentTo,
                    whichQueue = input$whichQueueTo,
                    sampleFile = input$sampleTo,
                    path = path,
                    inputId = "flowFrameTo",
                    currentValue = input$flowFrameTo)
                #message(paste0("end obs event: whichQueueTo"))
            })
            
            observeEvent(input$sampleFrom, {
                #message(paste0("obs event: sampleFrom"))
                # update list of available FF objects
                updateFFList(
                    experimentName = input$experimentFrom,
                    whichQueue = input$whichQueueFrom,
                    sampleFile = input$sampleFrom,
                    path = path,
                    inputId = "flowFrameFrom",
                    currentValue = input$flowFrameFrom)
                
                # update sample for comparison (default)
                updateSelectInput(
                    inputId = "sampleTo",
                    selected = input$sampleFrom)
                #message(paste0("end obs event: sampleFrom"))
            })
            
            observeEvent(input$sampleTo, {
                #message(paste0("obs event: sampleTo"))
                # update list of available FF objects
                updateFFList(
                    experimentName = input$experimentTo,
                    whichQueue = input$whichQueueTo,
                    sampleFile = input$sampleTo,
                    path = path,
                    inputId = "flowFrameTo",
                    currentValue = input$flowFrameTo)
                #message(paste0("end obs event: sampleTo"))
            })
            
            observeEvent(input$flowFrameFrom, {
                #message(paste0("obs event: flowFrameFrom"))
                # update list of channels
                updateChannelMarkerList(
                    experimentName = input$experimentFrom,
                    whichQueue = input$whichQueueFrom,
                    sampleFile = input$sampleFrom,
                    path = path,
                    flowFrameName = input$flowFrameFrom,
                    inputIds = c(
                        "xchannelFrom",
                        "ychannelFrom"),
                    currentValues = c(
                        input$xchannelFrom,
                        input$ychannelFrom))
                #message(paste0("end obs event: flowFrameFrom"))
            })
            
            observeEvent(input$flowFrameTo, {
                #message(paste0("obs event: flowFrameTo"))
                # update list of channels
                updateChannelMarkerList(
                    experimentName = input$experimentTo,
                    whichQueue = input$whichQueueTo,
                    sampleFile = input$sampleTo,
                    path = path,
                    flowFrameName = input$flowFrameTo,
                    inputIds = c(
                        "xchannelTo",
                        "ychannelTo"),
                    currentValues = c(
                        input$xchannelTo,
                        input$ychannelTo))
                #message(paste0("end obs event: flowFrameTo"))
            })
            
            observeEvent(input$xchannelFrom, {
                #message(paste0("obs event: xchannelFrom"))
                # update xchannel for comparison (default)
                updateSelectInput(
                    inputId = "xchannelTo",
                    selected = input$xchannelFrom)
                #message(paste0("end obs event: xchannelFrom"))
            })
            
            observeEvent(input$ychannelFrom, {
                #message(paste0("obs event: ychannelFrom"))
                # update ychannel for comparison (default)
                updateSelectInput(
                    inputId = "ychannelTo",
                    selected = input$ychannelFrom)
                #message(paste0("end obs event: ychannelFrom"))
            })
            
            observeEvent(input$useAllSampleEvents, {
                #message(paste0("obs event: useAllSampleEvents"))
            })
            
            output$workflowPlotFrom <- renderPlot({
                pipL <- buildCytoPipelineFromCache(
                    input$experimentFrom,
                    path = path)
                #message("rendering from workflow plot")
                sampleFile <- input$sampleFrom
                if (sampleFile != " " || 
                    input$whichQueueFrom == "scale transform") {
                    try(
                        plotCytoPipelineProcessingQueue(
                            pipL,
                            whichQueue = input$whichQueueFrom,
                            sampleFile = sampleFile,
                            path = path,
                            title = FALSE,
                            box.size = 0.15,
                            box.prop = 0.3))
                    
                }
            })
            
            output$workflowPlotTo <- renderPlot({
                pipL <- buildCytoPipelineFromCache(
                    input$experimentTo,
                    path = path)
                #message("rendering to workflow plot")
                sampleFile <- input$sampleTo
                if (sampleFile != " " || 
                    input$whichQueueTo == "scale transform") {
                    try(
                        plotCytoPipelineProcessingQueue(
                            pipL,
                            whichQueue = input$whichQueueTo,
                            sampleFile = sampleFile,
                            path = path,
                            title = FALSE,
                            box.size = 0.15,
                            box.prop = 0.3))
                }
            })
            
            
            output$ffPlotFrom <- renderPlot({
                transfoListName <- " "
                if (input$useSelectedTransfo){
                    transfoListName <- 
                        input[[NS("scaleTransformUI", "scaleTransfoList")]]
                }
                plotSelectedFlowFrame(
                    experimentName = input$experimentFrom,
                    whichQueue = input$whichQueueFrom,
                    sampleFile = input$sampleFrom,
                    path = path,
                    flowFrameName = input$flowFrameFrom,
                    xChannelLabel = input$xchannelFrom,
                    yChannelLabel = input$ychannelFrom,
                    useAllCells = input$useAllSampleEvents,
                    nDisplayCells = input$nSubSampleEvents,
                    useFixedLinearRange =
                        input$useFixedLinearRange,
                    linearRange = c(
                        input$minValueLinearRange,
                        input$maxValueLinearRange),
                    transfoListName = transfoListName)
            })
            
            output$ffPlotTo <- renderPlot({
                transfoListName <- " "
                if (input$useSelectedTransfo){
                    transfoListName <- 
                        input[[NS("scaleTransformUI", "scaleTransfoList")]]
                }
                plotSelectedFlowFrame(
                    experimentName = input$experimentTo,
                    whichQueue = input$whichQueueTo,
                    sampleFile = input$sampleTo,
                    path = path,
                    flowFrameName = input$flowFrameTo,
                    xChannelLabel = input$xchannelTo,
                    yChannelLabel = input$ychannelTo,
                    useAllCells = input$useAllSampleEvents,
                    nDisplayCells = input$nSubSampleEvents,
                    useFixedLinearRange =
                        input$useFixedLinearRange,
                    linearRange = c(input$minValueLinearRange,
                                    input$maxValueLinearRange),
                    transfoListName = transfoListName)
            })
            
            output$ffDiffPlotly <- plotly::renderPlotly({
                transfoListName <- " "
                if (input$useSelectedTransfo){
                    transfoListName <- 
                        input[[NS("scaleTransformUI", "scaleTransfoList")]]
                }
                plotDiffFlowFrame(
                    experimentNameFrom = input$experimentFrom,
                    experimentNameTo = input$experimentTo,
                    whichQueueFrom = input$whichQueueFrom,
                    whichQueueTo = input$whichQueueTo,
                    sampleFileFrom = input$sampleFrom,
                    sampleFileTo = input$sampleTo,
                    path = path,
                    flowFrameNameFrom = input$flowFrameFrom,
                    flowFrameNameTo = input$flowFrameTo,
                    xChannelLabelFrom = input$xchannelFrom,
                    xChannelLabelTo = input$xchannelTo,
                    yChannelLabelFrom = input$ychannelFrom,
                    yChannelLabelTo = input$ychannelTo,
                    interactive = TRUE,
                    useAllCells = input$useAllSampleEvents,
                    nDisplayCells = input$nSubSampleEvents,
                    useFixedLinearRange =
                        input$useFixedLinearRange,
                    linearRange = c(input$minValueLinearRange,
                                    input$maxValueLinearRange),
                    transfoListName = transfoListName)
            })
            
            scaleTransformServer(
                id = "scaleTransformUI",
                path = path)
            
            
        } # end main server
        shinyApp(ui, server)
    }
    
}

