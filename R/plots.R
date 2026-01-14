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

#' @title Plot a pipeline workflow from a CytoPipeline run
#'
#' @param experimentName the experiment name (representing a pipeline run) 
#' from which to extract the workflow
#' @param whichQueue "pre-processing" or "scale transform"
#' @param sampleFile in case 'whichQueue' is set to 'pre-processing, which
#' sample file to look at. This can be a number or a character.
#' - if whichQueue == "scale transform", the sampleFile is ignored
#' - if NULL and whichQueue == "pre-processing", the sampleFile is defaulted
#' to the first one belonging to the experiment
#' @param path the root path to look for the CytoPipeline experiment cache
#'
#' @return nothing, but displays the plot as a side effect
#' @export
#'
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
#'         list.files(rawDataDir, pattern = "Donor"))
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
#' plotSelectedWorkflow(
#'     experimentName = experimentName,
#'     whichQueue = "pre-processing",
#'     sampleFile = sampleFiles[1],
#'     path = outputDir)
#'     
#' plotSelectedWorkflow(
#'     experimentName = experimentName,
#'     whichQueue = "scale transform",
#'     sampleFile = NULL,
#'     path = outputDir)
#' 
#' 
plotSelectedWorkflow <- function(
    experimentName,
    whichQueue,
    sampleFile,
    path = path) {
    pipL <- buildCytoPipelineFromCache(
        experimentName,
        path = path)
    
    if (whichQueue == "scale transform") {
        plotCytoPipelineProcessingQueue(
                pipL,
                whichQueue = whichQueue,
                sampleFile = NULL,
                path = path,
                title = FALSE,
                box.size = 0.15,
                box.prop = 0.3)
    } else if (is.null(sampleFile) ||
               is.numeric(sampleFile)) {
        plotCytoPipelineProcessingQueue(
            pipL,
            whichQueue = whichQueue,
            sampleFile = sampleFile,
            path = path,
            title = FALSE,
            box.size = 0.15,
            box.prop = 0.3)
        
    } else if (is.character(sampleFile)){
        if (sampleFile %in% sampleFiles(pipL)){
            plotCytoPipelineProcessingQueue(
                pipL,
                whichQueue = whichQueue,
                sampleFile = sampleFile,
                path = path,
                title = FALSE,
                box.size = 0.15,
                box.prop = 0.3)
        } else {
            displayNames <- sampleDisplayNames(pipL)
            if (sampleFile %in% displayNames) {
                actualSampleFile <- sampleNameFromDisplayName(pipL, sampleFile)
                plotCytoPipelineProcessingQueue(
                    pipL,
                    whichQueue = whichQueue,
                    sampleFile = actualSampleFile,
                    path = path,
                    title = FALSE,
                    box.size = 0.15,
                    box.prop = 0.3)
            }
        } # else do nothing
    }
}

#' @title Plot a flow frame from a CytoPipeline run
#' @description Based on an experiment name, this function will gather the
#' required flowFrame from the CytoPipeline disk cache and display it using 
#' the user chosen 1D or 2D view.
#' @param experimentName the experiment name (representing a pipeline run) 
#' from which to extract the flow frame
#' @param whichQueue "pre-processing" or "scale transform"
#' @param sampleFile in case 'whichQueue' is set to 'pre-processing, which
#' sample file to look at. This can be a number or a character.
#' - if whichQueue == "scale transform", the sampleFile is ignored
#' - if NULL and whichQueue == "pre-processing", the sampleFile is defaulted
#' to the first one belonging to the experiment
#' @param flowFrameName the name of the object to fetch   
#' (as referenced in the pipeline workflow)
#' @param path the root path to look for the CytoPipeline experiment cache
#' @param xChannelLabel the label of the channel to be displayed on the x axis:
#' the conventional syntax is : `channelName` + " - " + `channelMarker`
#' @param yChannelLabel the label of the channel to be displayed on the y axis:
#' the conventional syntax is : `channelName` + " - " + `channelMarker`
#' @param useAllCells if TRUE, no subsampling will be done
#' @param nDisplayCells if useAllCells == FALSE, the number of subsampled cells
#' @param useFixedLinearRange if TRUE, all channels using a linear scale will
#' use a fixed range set by linearRange
#' @param linearRange set for all channels using a linear scale,
#' if useFixedLinearRange == TRUE
#' @param transfoListName if not set to " ", the transformation list 
#' (as an object name ending with "_obj", as referenced in the pipeline 
#' workflow) to be used for for display. 
#' @return a ggplot object
#' @export
#'
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
#'         list.files(rawDataDir, pattern = "Donor"))
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
#' plotSelectedFlowFrame(
#'     experimentName = experimentName,
#'     whichQueue = "pre-processing",
#'     sampleFile = 1,
#'     flowFrameName = "remove_debris_obj",
#'     path = outputDir,
#'     xChannelLabel = "FSC-A : NA",
#'     yChannelLabel = "SSC-A : NA",
#'     useAllCells = TRUE,
#'     nDisplayCells = 0,
#'     useFixedLinearRange = TRUE,
#'     linearRange = c(-100, 262144))
#' 
#' plotSelectedFlowFrame(
#'     experimentName = experimentName,
#'     whichQueue = "pre-processing",
#'     sampleFile = 1,
#'     flowFrameName = "remove_debris_obj",
#'     path = outputDir,
#'     xChannelLabel = "FSC-A : NA",
#'     yChannelLabel = "SSC-A : NA",
#'     useAllCells = FALSE,
#'     nDisplayCells = 100,
#'     useFixedLinearRange = FALSE,
#'     linearRange = NULL)
#' 
#' plotSelectedFlowFrame(
#'     experimentName = experimentName,
#'     whichQueue = "pre-processing",
#'     sampleFile = 1,
#'     flowFrameName = "remove_debris_obj",
#'     path = outputDir,
#'     xChannelLabel = "Comp-670/30Violet-A : BV785 - CD3",
#'     yChannelLabel = "Comp-780/60Red-A : APCCy7 - CD4",
#'     useAllCells = TRUE,
#'     nDisplayCells = 0,
#'     useFixedLinearRange = FALSE,
#'     linearRange = NULL,
#'     transfoListName = "scale_transform_estimate_obj")
#' 
plotSelectedFlowFrame <- function(
        experimentName,
        whichQueue,
        sampleFile,
        flowFrameName,
        path,
        xChannelLabel,
        yChannelLabel,
        useAllCells,
        nDisplayCells,
        useFixedLinearRange,
        linearRange,
        transfoListName = " ") {
    if (xChannelLabel != " ") {
        
        message("displaying flow frame plot...")
        pipL <- buildCytoPipelineFromCache(
            experimentName,
            path = path)
        if (is.character(sampleFile)){
            if (!sampleFile %in% sampleFiles(pipL)){
                displayNames <- sampleDisplayNames(pipL)
                if (sampleFile %in% displayNames) {
                    sampleFile <- sampleNameFromDisplayName(pipL, sampleFile)
                } # else do nothing
            }
        }
        ff <- getCytoPipelineFlowFrame(
            pipL,
            whichQueue = whichQueue,
            sampleFile = sampleFile,
            objectName = flowFrameName,
            path = path)

        xChannel <- channelLabel2Name(xChannelLabel, ff)
        if (length(xChannel) == 0) return()

        fluoChannels <- flowCore::colnames(ff)[areFluoCols(ff)]

        transList <- NULL
        if (transfoListName != " ") {
            transList <-
                getCytoPipelineScaleTransform(
                    pipL,
                    whichQueue = "scale transform",
                    objectName = transfoListName,
                    path = path)
        }

        xScale <- "linear"
        if (xChannel %in% fluoChannels) {
            xScale <- "logicle"
        }

        yScale <- "linear"
        if (yChannelLabel == " ") {
            yChannel <- NULL
        } else {
            yChannel <- channelLabel2Name(yChannelLabel, ff)
            if (length(yChannel) == 0) return()
            if (yChannel %in% fluoChannels) {
                yScale <- "logicle"
            }
        }


        if (useFixedLinearRange) {
            xLinearRange <- linearRange
            yLinearRange <- linearRange
        } else {
            xLinearRange <- NULL
            yLinearRange <- NULL
        }

        if (useAllCells) {
            nDisplayCells <- Inf
        }

        p <- ggplotEvents(
            obj = ff,
            xChannel = xChannel,
            yChannel = yChannel,
            nDisplayCells = nDisplayCells,
            seed = 0,
            bins = 216,
            fill = "lightgreen",
            alpha = 0.2,
            xScale = xScale,
            yScale = yScale,
            xLinearRange = xLinearRange,
            yLinearRange = yLinearRange,
            transList = transList)


        theSubtitle <- paste0("nb of events: ", flowCore::nrow(ff))

        p <- p + ggplot2::labs(subtitle = theSubtitle)
        p

    }
}

#' @title Plot the difference plot between two flow frames 
#' from a CytoPipeline run
#' @description Based on an experiment name, this function will gather the
#' required flowFrames from the CytoPipeline disk cache and display 
#' a difference plot using the user chosen 1D or 2D view.
#' @param experimentNameFrom the experiment name (representing a pipeline run) 
#' from which to extract the flow frame ('from' situation)
#' @param experimentNameTo the experiment name (representing a pipeline run) 
#' from which to extract the flow frame ('to' situation)
#' @param whichQueueFrom "pre-processing" or "scale transform" 
#' ('from' situation)
#' @param whichQueueTo "pre-processing" or "scale transform" 
#' ('to' situation)
#' @param sampleFileFrom in case 'whichQueueFrom' is set to 'pre-processing, 
#' which sample file to look at for the 'from' situation. 
#' This can be a number or a character.
#' - if whichQueueFrom == "scale transform", the sampleFileFrom is ignored
#' - if NULL and whihQueueFrom == "pre-processing", the sampleFileFrom 
#' is defaulted to the first one belonging to the experiment
#' @param sampleFileTo same as sampleFileFrom, but for the 'to' situation
#' @param path the root path to look for the CytoPipeline experiment cache
#' @param flowFrameNameFrom for the 'from' situation, 
#' the name of the object to fetch (as referenced in the pipeline workflow)
#' @param flowFrameNameTo for the 'to' situation, 
#' the name of the object to fetch (as referenced in the pipeline workflow)
#' @param xChannelLabelFrom the label of the channel 
#' to be displayed on the x axis:
#' the conventional syntax is : `channelName` + " - " + `channelMarker`
#' @param xChannelLabelTo should be equal to xChannelLabelFrom
#' (otherwise no plot is returned but NULL)
#' @param yChannelLabelFrom the label of the channel 
#' to be displayed on the y axis:
#' the conventional syntax is : `channelName` + " - " + `channelMarker`
#' @param yChannelLabelTo should be equal to yChannelLabelFrom
#' (otherwise no plot is returned but NULL) 
#' @param interactive if TRUE, uses ggplot_shiny
#' @param useAllCells if TRUE, no subsampling will be done
#' @param nDisplayCells if useAllCells == FALSE, the number of subsampled cells
#' @param useFixedLinearRange if TRUE, all channels using a linear scale will
#' use a fixed range set by linearRange
#' @param linearRange set for all channels using a linear scale,
#' if useFixedLinearRange == TRUE
#' @param transfoListName if not set to " ", the transformation list 
#' (as an object name ending with "_obj", as referenced in the pipeline 
#' workflow) to be used for for display. 
#' @return a ggplot (or plotly if interactive = TRUE) object
#' @export
#'
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
#'         list.files(rawDataDir, pattern = "Donor"))
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
#'
#' plotDiffFlowFrame(
#'     experimentNameFrom = experimentName,
#'     whichQueueFrom = "pre-processing",
#'     sampleFileFrom = 1,
#'     flowFrameNameFrom = "remove_doublets_obj",
#'     xChannelLabelFrom = "FSC-A : NA",
#'     yChannelLabelFrom = "SSC-A : NA",
#'     path = outputDir,
#'     experimentNameTo = experimentName,
#'     whichQueueTo = "pre-processing",
#'     sampleFileTo = 1,
#'     flowFrameNameTo = "remove_debris_obj",
#'     xChannelLabelTo = "FSC-A : NA",
#'     yChannelLabelTo = "SSC-A : NA",
#'     useAllCells = TRUE,
#'     nDisplayCells = 0,
#'     useFixedLinearRange = TRUE,
#'     linearRange = c(-100, 262144))
#' 
#' plotDiffFlowFrame(
#'     experimentNameFrom = experimentName,
#'     whichQueueFrom = "pre-processing",
#'     sampleFileFrom = 1,
#'     flowFrameNameFrom = "remove_doublets_obj",
#'     xChannelLabelFrom = "FSC-A : NA",
#'     yChannelLabelFrom = "SSC-A : NA",
#'     path = outputDir,
#'     experimentNameTo = experimentName,
#'     whichQueueTo = "pre-processing",
#'     sampleFileTo = 1,
#'     flowFrameNameTo = "remove_debris_obj",
#'     xChannelLabelTo = "FSC-A : NA",
#'     yChannelLabelTo = "SSC-A : NA",
#'     useAllCells = FALSE,
#'     nDisplayCells = 100,
#'     useFixedLinearRange = FALSE,
#'     linearRange = NULL)
#' 
#' plotDiffFlowFrame(
#'     experimentNameFrom = experimentName,
#'     whichQueueFrom = "pre-processing",
#'     sampleFileFrom = 1,
#'     flowFrameNameFrom = "remove_debris_obj",
#'     xChannelLabelFrom = "FSC-A : NA",
#'     yChannelLabelFrom = "Comp-525/50Violet-A : L/D Aqua - Viability",
#'     path = outputDir,
#'     experimentNameTo = experimentName,
#'     whichQueueTo = "pre-processing",
#'     sampleFileTo = 1,
#'     flowFrameNameTo = "remove_dead_cells_obj",
#'     xChannelLabelTo = "FSC-A : NA",
#'     yChannelLabelTo = "Comp-525/50Violet-A : L/D Aqua - Viability",
#'     useAllCells = TRUE,
#'     nDisplayCells = 0,
#'     useFixedLinearRange = FALSE,
#'     linearRange = NULL,
#'     transfoListName = "scale_transform_estimate_obj")   
#' 
plotDiffFlowFrame <- function(
        experimentNameFrom,
        experimentNameTo,
        whichQueueFrom,
        whichQueueTo,
        sampleFileFrom,
        sampleFileTo,
        path,
        flowFrameNameFrom,
        flowFrameNameTo,
        xChannelLabelFrom,
        xChannelLabelTo,
        yChannelLabelFrom,
        yChannelLabelTo,
        interactive = FALSE,
        useAllCells,
        nDisplayCells,
        useFixedLinearRange,
        linearRange,
        transfoListName = " ") {

    if (xChannelLabelFrom != " " &&
        yChannelLabelFrom != " " &&
        xChannelLabelTo == xChannelLabelFrom &&
        yChannelLabelTo == yChannelLabelFrom &&
        whichQueueFrom == whichQueueTo &&
        sampleFileFrom == sampleFileTo) {
        message("displaying flow frame comparison plot...")
        pipLFrom <- buildCytoPipelineFromCache(
            experimentNameFrom,
            path = path)

        if (is.character(sampleFileFrom)){
            if (!sampleFileFrom %in% sampleFiles(pipLFrom)){
                displayNames <- sampleDisplayNames(pipLFrom)
                if (sampleFileFrom %in% displayNames) {
                    sampleFileFrom <- 
                        sampleNameFromDisplayName(
                            pipLFrom,
                            sampleFileFrom)
                } # else do nothing
            }
        }

        ffFrom <- getCytoPipelineFlowFrame(
            pipLFrom,
            whichQueue = whichQueueFrom,
            sampleFile = sampleFileFrom,
            objectName = flowFrameNameFrom,
            path = path)

        pipLTo <- buildCytoPipelineFromCache(
            experimentNameTo,
            path = path)

        if (is.character(sampleFileTo)){
            if (!sampleFileTo %in% sampleFiles(pipLTo)){
                displayNames <- sampleDisplayNames(pipLTo)
                if (sampleFileTo %in% displayNames) {
                    sampleFileTo <- 
                        sampleNameFromDisplayName(
                            pipLTo,
                            sampleFileTo)
                } # else do nothing
            }
        }

        ffTo <- getCytoPipelineFlowFrame(
            pipLTo,
            whichQueue = whichQueueTo,
            sampleFile = sampleFileTo,
            objectName = flowFrameNameTo,
            path = path)

        transList <- NULL
        if (transfoListName != " ") {
            transList <-
                getCytoPipelineScaleTransform(
                    pipLFrom,
                    whichQueue = "scale transform",
                    objectName = transfoListName,
                    path = path)
        }

        xChannel <- channelLabel2Name(xChannelLabelFrom, ffFrom)
        if (length(xChannel) == 0) return()

        fluoChannels <- flowCore::colnames(ffFrom)[areFluoCols(ffFrom)]
        xScale <- "linear"

        if (xChannel %in% fluoChannels) {
            xScale <- "logicle"
        }

        yScale <- "linear"
        if (yChannelLabelFrom == " ") {
            yChannel <- NULL
        } else {
            yChannel <- channelLabel2Name(yChannelLabelFrom, ffFrom)
            if (length(yChannel) == 0) return()
            if (yChannel %in% fluoChannels) {
                yScale <- "logicle"
            }
        }

        if (useFixedLinearRange) {
            xLinearRange <- linearRange
            yLinearRange <- linearRange
        } else {
            xLinearRange <- NULL
            yLinearRange <- NULL
        }


        if (useAllCells) {
            nDisplayCells <- Inf
        }

        # used to prevent shiny app being frozen by plotly plot
        nEffectiveDisplayCells <- min(10000, nDisplayCells)

        p <- ggplotFilterEvents(
            ffPre = ffFrom,
            ffPost = ffTo,
            xChannel = xChannel,
            yChannel = yChannel,
            nDisplayCells = nEffectiveDisplayCells,
            seed = 0,
            size = 0.1,
            xScale = xScale,
            yScale = yScale,
            xLinearRange = xLinearRange,
            yLinearRange = yLinearRange,
            transList = transList,
            interactive = interactive)

        nEventFrom <- flowCore::nrow(ffFrom)
        nEventTo <- flowCore::nrow(ffTo)

        removedEvents <- 100 *
            (nEventFrom - nEventTo) / max(nEventFrom, nEventTo)
        if (experimentNameFrom == experimentNameTo &&
            sampleFileFrom == sampleFileTo &&
            whichQueueFrom == whichQueueTo &&
            whichQueueFrom == "pre-processing" &&
            nEventFrom >= nEventTo) {
            theSubtitle <- paste0(
                "removed ", round(removedEvents,2),
                "% of events, ", nEventTo,
                " good events remaining")
        } else {
            theSubtitle <- paste0(
                "nb events left: ", nEventFrom,
                ", nb events right: ", nEventTo,
                ", diff: ", round(abs(removedEvents), 2), "%")
        }

        if (interactive) {

            if (is.null(p)) {
                p <- plotly::plotly_empty(type="scatter", mode = "markers")
            } else {
                p <- plotly::layout(
                    p = plotly::ggplotly(p),
                    title = list(
                        text = theSubtitle,
                        xanchor = "center",
                        font = list(
                            size = 10)))
            }

        } else {
            p <- p + ggplot2::labs(subtitle = theSubtitle)
        }


        return(p)

    }
    return(NULL)
}

#' @title Plot a flow frame in 1D with explicit user given scale transform  
#' @description This function plots a 1D view, i.e. the marginal distribution
#' for one specified channel, of the given flow frame, using the specific 
#' user-provided scale transformation parameters.
#' @param ff the `flowFrame` to be plotted
#' @param channel the name of the channel of which to display the marginal 
#' distribution (i.e. the channel name used as column in the ff expression 
#' matrix).
#' @param applyTransform if "data", data are explicitly transformed using
#' the user provided sclae transformation parameters, before display
#' if "axis scale only" (default), the data are not transformed, 
#' i.e. only the x axis scale is defined according to 
#' the scale transformation parameters. 
#' @param transfoType the transformation type, currently only 
#' `linear` and `logicle`(bi-exponential) are supported.
#' @param linA the intercept parameter of the linear transformation.  
#' @param linB the slope parameter of the linear transformation. 
#' @param t the max scale parameter of the logicle tranformation.
#' @param m the number of positive decades of the logicle tranformation.
#' @param w the width parameter of the logicle transformation.
#' @param a the number of additional decades on the negative side 
#' for the logicle transformation.   
#' @return a ggplot object
#' @export
#'
#' @examples
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
#'         list.files(rawDataDir, pattern = "Donor"))
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
#' ff <- CytoPipeline::getCytoPipelineFlowFrame(
#' pipL2,
#' path = outputDir,
#' whichQueue = "scale transform",
#' objectName = "flowframe_aggregate_obj"
#' )
#' 
#' plotScaleTransformedChannel(
#'     ff,
#'     channel = "FSC-A",
#'     transfoType = "linear",
#'     linA = 0.0002,
#'     linB = -0.5)
#' 
#' plotScaleTransformedChannel(
#'     ff,
#'     channel = "Comp-670/30Violet-A",
#'     transfoType = "logicle",
#'     t = 262144,
#'     m = 4.5,
#'     w = 0.5,
#'     a = 1.0
#' )
#' 
#' plotScaleTransformedChannel(
#'     ff,
#'     channel = "CD3",
#'     applyTransform = "data",
#'     transfoType = "logicle",
#'     t = 262144,
#'     m = 4.5,
#'     w = 0.5,
#'     a = 1.0
#' )
#'
plotScaleTransformedChannel <- function(
        ff,
        channel,
        applyTransform =
            c("axis scale only", "data"),
        transfoType =
            c("linear", "logicle"),
        linA, linB,
        t, m, w, a
        #negDecades, width, posDecades
        ) {
    applyTransform <- match.arg(applyTransform)
    transfoType <- match.arg(transfoType)
    if (!is.null(ff)) {
        
        if (transfoType == "linear") {
            theTrans <- flowCore::linearTransform(
                a = linA,
                b = linB)
        } else {
            theTrans <- flowCore::logicleTransform(
                t = t,
                m = m,
                w = w,
                a = a)
            # theTrans <- flowCore::logicleTransform(
            #     w = width,
            #     m = posDecades + width,
            #     a = negDecades)
        }

        # find the channel name (can be based on marker name)
        ch <- flowCore::getChannelMarker(ff, channel)$name

        theTransList <- flowCore::transformList(
            from = ch,
            tfun = theTrans)

        if (applyTransform == "data") {
            runTransforms <- TRUE
            if (transfoType == "logicle") {
                #linearRange <- c(0, posDecades + width)
                linearRange <- c(0, m)
            } else {
                linearRange <- NULL
            }

        } else {
            runTransforms <- FALSE
            linearRange <- NULL
        }

        ggplotEvents(
            ff,
            xChannel = ch,
            xLinearRange = linearRange,
            nDisplayCells = 10000,
            seed = 0,
            transList = theTransList,
            runTransforms = runTransforms,
            fill = "lightgreen",
            alpha = 0.2)
    }

}