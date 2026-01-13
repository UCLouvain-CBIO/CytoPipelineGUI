# CytoPipelineGUI - Copyright (C) <2022-2026> 
# <Université catholique de Louvain (UCLouvain), Belgique>
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

scaleTransformUI <- function(
        id,
        experimentNames, 
        enableTransfoListUpdate = FALSE,
        enableTransfoListSave = FALSE) {
    tagList(
        # Sidebar layout with input and output definitions ----
        sidebarLayout(
            
            # Sidebar panel for inputs ----
            sidebarPanel(
                selectInput(
                    inputId = NS(id, "experimentTransfo"),
                    label = "experiment:",
                    choices = experimentNames),
                
                # Input: the scale transformation list we would like to
                # display
                selectInput(
                    inputId = NS(id, "scaleTransfoList"),
                    label = "scale transform list",
                    choices = c(" ")),
                
                # Input: the flow frame object, from which we display
                # the distributions
                selectInput(
                    inputId = NS(id, "flowFrameTransfo"),
                    label = "flow frame:",
                    choices = c(" ")),
                
                # Input: the active channel
                selectInput(
                    inputId = NS(id, "transfoChannel"),
                    label = "channel/marker:",
                    choices = c(" ")),
                
                # Input : transform data or scale only ?
                selectInput(
                    inputId = NS(id, "applyTransform"),
                    label = "Visualize transformation on:",
                    choices = c("axis scale only", "data")),
                
                # Input: type of transformation : linear or logicle
                selectInput(
                    inputId = NS(id, "transfoType"),
                    label = "Transformation type:",
                    choices = c("linear", "logicle")),
                
                # Input: set of inputs for linear transformations
                conditionalPanel(
                    condition = "input.transfoType == 'linear'",
                    ns = NS(id),
                    # condition = 
                    #     paste0("input.", 
                    #            NS(id, "transfoType"), " == 'linear'"),
                    numericInput(
                        inputId = NS(id, "linA"),
                        label = "slope",
                        value = 1., min = 0., 
                        max = 1000000, step = NA),
                    numericInput(
                        inputId = NS(id, "linB"),
                        label = "intercept",
                        value = 0., min = -Inf, 
                        max = Inf, step = NA)),
                
                # Input: set of inputs for conditional transformations
                conditionalPanel(
                    condition = "input.transfoType == 'logicle'",
                    ns = NS(id),
                    # condition = 
                    #     paste0("input.",
                    #            NS(id, "transfoType"), " == 'logicle'"),
                    sliderInput(
                        inputId = NS(id, "negDecades"),
                        label = "Extra Neg Decades:",
                        min = 0,
                        max = 1,
                        step = 0.1,
                        value = 0),
                    sliderInput(
                        inputId = NS(id, "width"),
                        label = "Width:",
                        min = 0,
                        max = 3,
                        step = 0.1,
                        value = 1),
                    sliderInput(
                        inputId = NS(id, "posDecades"),
                        label = "Pos Decades:",
                        min = 2,
                        max = 7,
                        step = 0.1,
                        value = 4.5)
                ),
                
                conditionalPanel(
                    condition = tolower(as.character(enableTransfoListUpdate)),
                    actionButton(
                        inputId = NS(id, "apply"),
                        label = "Apply to channel")),
                
                conditionalPanel(
                    condition = tolower(as.character(enableTransfoListSave)),
                    downloadButton(
                        outputId = NS(id, "save"),
                        label = "Save")),
                
                # conditionalPanel(
                #   condition = tolower(
                #       as.character(transListSource == "input")),
                #   actionButton(inputId = NS(id, "done"),
                #                label = "Done!")),
                
            ), # end sidebar panel
            
            # Main panel for displaying outputs ----
            mainPanel(
                
                # Output: Histogram ----
                plotOutput(outputId = NS(id, "distPlot"))
                
            )
        ) # end sidebarLayout
    ) # end taglist
}

scaleTransformServer <- function(id, path, transfoList = NULL, ff = NULL) {
    moduleServer(id, function(input, output, session) {
        ##### SCALE TRANSFORM PART ###################################
        
        # stores currently obtained scale transfo list
        currentTransfoList <- NULL
        
        if (!is.null(transfoList)) {
            if (is.null(ff)) {
                stop(
                    "if transfoList parameter is passed as non null, ",
                    "so must be ff parameter")
            } 
        } else {
            if (!is.null(ff)) {
                stop(
                    "if ff parameter is passed as non null, ",
                    "so must be transfoList parameter")
            }
        }
        
        # note the following statement also holds when the RHS is NULL
        currentTransfoList <- transfoList
        
        currentFFFull <- reactive({
            ### Warning: activating messages in reactive disturbs the process!!
            if (!is.null(ff)) {
                ff
            } else {
                if (input$flowFrameTransfo == " ") {
                    NULL
                } else if (grepl(
                    pattern = "Synth. agg. - ", 
                    x = input$flowFrameTransfo)) {
                    # no flow frame in the scale transform queue
                    # => build synthetic one from pre-processing queue
                    # => sample 10,000 events from max. 5 flow frames
                    nTotalEvents <- 10000
                    nFilesMax <- 5
                    
                    objectName <- sub(
                        pattern = "Synth. agg. - ", 
                        replacement = "",
                        x = input$flowFrameTransfo)
                    
                    pipL <- buildCytoPipelineFromCache(
                        input$experimentTransfo,
                        path = path)
                    samples <- sampleFiles(pipL)
                    nSamples <- length(samples)
                    if (nSamples >= nFilesMax) {
                        samples <- 
                            samples[sample.int(nSamples, size = nFilesMax)]
                    }
                    ffList <- list()
                    for (s in samples) {
                        df <- getCytoPipelineObjectInfos(
                            pipL,
                            whichQueue = "pre-processing",
                            sampleFile = s,
                            path = path)
                        if (nrow(df[(
                            df$ObjectClass == "flowFrame" | 
                            df$ObjectClass == "cytoframe") & 
                            df$ObjectName == objectName,]) > 0) {
                            myff <- getCytoPipelineFlowFrame(
                                pipL,
                                whichQueue = "pre-processing",
                                sampleFile = s,
                                objectName = objectName,
                                path = path)
                            
                            ffList <- c(ffList, myff)
                        }
                    }
                    if (length(ffList) == 0)
                        NULL
                    
                    fs <- flowCore::flowSet(ffList)
                    ff <- 
                        CytoPipeline::aggregateAndSample(
                            fs,
                            nTotalEvents = nTotalEvents,
                            seed = 0)
                    ff
                } else {
                    pipL <- 
                        buildCytoPipelineFromCache(
                            input$experimentTransfo,
                            path = path)
                    
                    df <- getCytoPipelineObjectInfos(
                        pipL,
                        whichQueue = "scale transform",
                        sampleFile = NULL,
                        path = path)
                    if (nrow(df[(
                        df$ObjectClass == "flowFrame" | 
                        df$ObjectClass == "cytoframe") & 
                        df$ObjectName == input$flowFrameTransfo,
                    ]) > 0) {
                        ff <- 
                            getCytoPipelineFlowFrame(
                                pipL,
                                whichQueue = "scale transform",
                                sampleFile = NULL,
                                objectName = input$flowFrameTransfo,
                                path = path)
                        
                        ff
                    } else {
                        NULL
                    }
                } 
            }
        })
        
        currentFF <- reactive({
            if (is.null(currentFFFull)) {
                NULL
            } else if (input$transfoChannel != " ") {
                channel <- 
                    channelLabel2Name(input$transfoChannel, currentFFFull())
                currentFFFull()[, channel]
            } else {
                NULL
            }
        })
        
        r <- reactiveValues(
            transfoType = "linear",
            linA = 1.0,
            linB = 0.0,
            negDecades = 0.0,
            width = 1,
            posDecades = 4.5)
        
        getTransfoList <- function(
            experimentName,
            path,
            transfoName) {
            
            if (transfoName != " ") {
                pipL <- buildCytoPipelineFromCache(
                    experimentName,
                    path = path)
                
                transfoList <-
                    getCytoPipelineScaleTransform(
                        pipL,
                        whichQueue = "scale transform",
                        objectName = transfoName,
                        path = path)
                return(transfoList)
            } else {
                return(NULL)
            }
        }
        
        updateTransfoParams <- function(
            transfoList,
            ff) {
            if (!is.null(transfoList) && !is.null(ff)) {
                tPars <- getTransfoParams(
                    transfoList,
                    channel = flowCore::colnames(ff)[1])
                if (is.null(tPars)) {
                    # trial with "Comp-"
                    tPars <- 
                        getTransfoParams(
                            transfoList,
                            channel = paste0(
                                "Comp-",
                                flowCore::colnames(ff)[1]))
                }
                
                if (!is.null(tPars)) {
                    updateSelectInput(
                        inputId = "transfoType",
                        selected = tPars$type)
                    
                    if (tPars$type == "logicle") {
                        val_negDecades <- round(tPars$paramsList$a, digits = 1)
                        val_width <- round(tPars$paramsList$w, digits = 1)
                        val_posDecades <- 
                            round(
                                tPars$paramsList$m -
                                    tPars$paramsList$w, digits = 1)
                        
                        updateSliderInput(
                            inputId = "negDecades",
                            value = val_negDecades)
                        updateSliderInput(
                            inputId = "width",
                            value = val_width)
                        updateSliderInput(
                            inputId = "posDecades",
                            value = val_posDecades)
                        
                        synchronizeReactiveTransfoParams(
                            transfoType = tPars$type,
                            negDecades = val_negDecades,
                            width = val_width,
                            posDecades = val_posDecades)
                    } else {
                        val_a <- signif(tPars$paramsList$a, 3)
                        val_b <- signif(tPars$paramsList$b, 3)
                        
                        updateNumericInput(
                            inputId = "linA",
                            value = val_a)
                        updateNumericInput(
                            inputId = "linB",
                            value = val_b)
                        
                        synchronizeReactiveTransfoParams(
                            transfoType = tPars$type,
                            linA = val_a,
                            linB = val_b)
                    }
                    
                }
                
            }
        }
            
        
        synchronizeReactiveTransfoParams <- function(
            transfoType,
            linA = 0.,
            linB = 0.,
            negDecades = 0.,
            width = 0.,
            posDecades = 0.){
            if (transfoType != r$transfoType) {
                #message("transfoType is different => updating...")
                r$transfoType <- transfoType
            }
            if (transfoType == "linear") {
                if (linA != r$linA) {
                    #message("linA is different => updating...")
                    r$linA <- linA
                }
                if (linB != r$linB) {
                    #message("linB is different => updating...")
                    r$linB <- linB
                }
            } else {
                if (negDecades != r$negDecades) {
                    #message("negDecades is different => updating...")
                    r$negDecades <- negDecades
                }
                if (width != r$width) {
                    #message("width is different => updating...")
                    r$width <- width
                }
                if (posDecades != r$posDecades) {
                    #message("posDecades is different => updating...")
                    r$posDecades <- posDecades
                }
            }
        }
        
        observeEvent(input$experimentTransfo, {
            #message("obs event: experimentTransfo")
            # update list of available transList objects 
            # for scale transfo display
            updateTransList(
                experimentName = input$experimentTransfo,
                path = path,
                inputId = "scaleTransfoList",
                currentValue = input$scaleTransfoList)
            
            # update list of available FF objects for scale transfo display
            updateFFListForTransDisplay(
                experimentName = input$experimentTransfo,
                path = path,
                inputId = "flowFrameTransfo",
                currentValue = input$flowFrameTransfo)
            
            #message("end obs event: experimentTransfo")
        })
        
        observeEvent(input$flowFrameTransfo, {
            #message("obs event: flowFrameTransfo")
            # update list of channels
            ffName <- input$flowFrameTransfo
            if (grepl(
                    pattern = "Synth. agg. - ", 
                    x = ffName)) {
                ffName <- sub(
                    pattern = "Synth. agg. - ", 
                    replacement = "",
                    x = ffName)  
                updateChannelMarkerList(
                    experimentName = input$experimentTransfo,
                    whichQueue = "pre-processing",
                    sampleFile = 1,
                    path = path,
                    flowFrameName = ffName,
                    inputIds = c("transfoChannel"),
                    currentValues = c(input$transfoChannel))
                
            } else {
                updateChannelMarkerList(
                    experimentName = input$experimentTransfo,
                    whichQueue = "scale transform",
                    sampleFile = NULL,
                    path = path,
                    flowFrameName = ffName,
                    inputIds = c("transfoChannel"),
                    currentValues = c(input$transfoChannel))
            }
            
            #message("end obs event: flowFrameTransfo")
        })
        
        observeEvent(input$scaleTransfoList, {
            #message("obs event: scaleTransfoList")
            
            assign(
                "currentTransfoList", 
                getTransfoList(
                    input$experimentTransfo,
                    path = path,
                    transfoName = input$scaleTransfoList),
                inherits = TRUE)
            
            updateTransfoParams(
                currentTransfoList,
                currentFF())  
            #message("end obs event: scaleTransfoList")
        })
        
        observeEvent(input$transfoChannel, {
            #message("obs event: transfoChannel")
            updateTransfoParams(
                currentTransfoList,
                currentFF())
        })
        
        
        observeEvent({
            input$transfoType
            input$linA
            input$linB
            input$negDecades
            input$width
            input$posDecades
        },{
            #message(paste0("obs event: one transfo param"))
            synchronizeReactiveTransfoParams(
                transfoType = req(input$transfoType),
                linA = req(input$linA),
                linB = req(input$linB),
                negDecades = req(input$negDecades),
                width = req(input$width),
                posDecades = req(input$posDecades))
            #message(paste0("end obs event: one transfo param"))
        })
        
        observeEvent(input$apply, {
            #message("obs event: apply")
            
            if (!is.null(currentTransfoList) && !is.null(currentFF())) {
                
                myList <- isolate(reactiveValuesToList(r))
                
                if (r$transfoType == "linear") {
                    newTrans <- 
                        flowCore::linearTransform(
                            a = myList$linA,
                            b = myList$linB)
                } else {
                    newTrans <- 
                        flowCore::logicleTransform(
                            w = myList$width,
                            m = myList$posDecades + 
                                myList$width,
                            a = myList$negDecades)
                }
                
                channel <- flowCore::colnames(currentFF())[1]
                
                localTransfoList <- currentTransfoList
                localTransfoList@transforms[[channel]] <- NULL
                localTransfoList <-
                    c(  localTransfoList, 
                        flowCore::transformList(channel, newTrans))
                
                assign(
                    "currentTransfoList",
                    localTransfoList,
                    inherits = TRUE)
            }
            
        })
        
        output$distPlot <- renderPlot({
            message("Rendering scale transformed channel...")
            channel <- flowCore::colnames(currentFF())[1]
            plotScaleTransformedChannel(
                ff = currentFF(),
                channel = channel,
                applyTransform = input$applyTransform,
                transfoType = r$transfoType,
                linA = r$linA,
                linB = r$linB,
                negDecades = r$negDecades,
                width = r$width,
                posDecades = r$posDecades)
            
        })
        
        output$save <- downloadHandler(
            filename = function() {
                paste0(path, "/", "savedScaleTranfoList", ".rds")
            },
            content = function(file) {
                saveRDS(currentTransfoList, file = file)
            }
        )
    })
}