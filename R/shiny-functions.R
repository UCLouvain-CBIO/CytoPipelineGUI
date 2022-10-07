# CytoPipelineGUI - Copyright (C) <2022> <Université catholique de Louvain (UCLouvain), Belgique>
#   
#   Description and complete License: see LICENSE file.
# 
# This program (CytoPipelineGUI) is free software: 
#   you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details (<http://www.gnu.org/licenses/>).

channelLabel2Name <- function(channelLabel, ff) {
  channels <- unname(flowCore::colnames(ff))#[areSignalCols(ff)])
  chLabels <- lapply(channels,
                     FUN = function(ch){
                       chMk <- flowCore::getChannelMarker(ff, ch)
                       lab <- paste0(chMk$name, " : ", chMk$desc)
                       lab
                     })
  
  channel <- channels[which(chLabels == channelLabel)]
  return(channel)
}


updateFFList <- function(experimentName,
                         whichQueue,
                         sampleFile,
                         path,
                         inputId){
  #browser()
  noFF <- FALSE
  if (!is.null(sampleFile)){
    if (sampleFile == " ") {
      if (whichQueue == "pre-processing") {
        noFF <- TRUE
      } else {
        sampleFile <- NULL
      }
    }
  }
  
  if (noFF) {
    newChoices = " "
  } else {
    pipL <- buildCytoPipelineFromCache(experimentName,
                                       path = path)
    df <- getCytoPipelineObjectInfos(pipL,
                                     whichQueue = whichQueue,
                                     sampleFile = sampleFile,
                                     path = path)
    FFNames <-
      df[df$ObjectClass == "flowFrame" | df$ObjectClass == "cytoframe",
         "ObjectName"]
    if (length(FFNames) == 0)
      stop("no flow frame objects attached to selected (experiment;queue;sample")
    newChoices = c(" ", FFNames)
  }
  updateSelectInput(inputId = inputId, 
                      choices = newChoices,
                      selected = " ")
  
}

updateTransList <- function(experimentName,
                            path,
                            inputId){
  #browser()
  if (experimentName != " ") {
    pipL <- buildCytoPipelineFromCache(experimentName,
                                       path = path)
    df <- getCytoPipelineObjectInfos(pipL,
                                     whichQueue = "scale transform",
                                     sampleFile = NULL,
                                     path = path)
    scaleTransfoNames <-
      df[df$ObjectClass == "transformList", "ObjectName"]
    if (length(scaleTransfoNames) == 0)
      scaleTransfoNames <- " "
    updateSelectInput(inputId = inputId, 
                      choices = c(" ", scaleTransfoNames),
                      selected = " ")
  }
}

updateChannelMarkerList <- function(experimentName,
                                    whichQueue,
                                    sampleFile,
                                    path,
                                    flowFrameName,
                                    inputIds){
  #browser()
  if (flowFrameName != " ") {
    pipL <- buildCytoPipelineFromCache(experimentName,
                                       path = path)
    ff <- getCytoPipelineFlowFrame(pipL,
                                   whichQueue = whichQueue,
                                   sampleFile = sampleFile,
                                   objectName = flowFrameName,
                                   path = path)
    
    # sort out channels and and channel_labels
    channels <- unname(flowCore::colnames(ff)[areSignalCols(ff)])
    
    # add time channel
    timeCh <- findTimeChannel(ff)
    
    channels <- c(channels, timeCh)
    
    chLabels <- lapply(channels,
                       FUN = function(ch){
                         chMk <- flowCore::getChannelMarker(ff, ch)
                         lab <- paste0(chMk$name, " : ", chMk$desc)
                         lab
                       })
    for (inputId in inputIds) {
      updateSelectInput(inputId = inputId,
                        choices = c(" ", chLabels),
                        selected = " ")
    }
    
  } else {
    for (inputId in inputIds) {
      updateSelectInput(inputId = inputId,
                        choices = c(" "))
    }
  }
  
  
}




plotSelectedFlowFrame <- function(experimentName,
                                  whichQueue,
                                  sampleFile,
                                  flowFrameName,
                                  path,
                                  xChannelLabel,
                                  yChannelLabel,
                                  useAllCells,
                                  nDisplayCells,
                                  useMaxValueLinearRange,
                                  maxValueLinearRange
) {
  if (xChannelLabel != " ") {
    message("displaying flow frame plot...")
    #browser()
    pipL <- buildCytoPipelineFromCache(experimentName,
                                       path = path)
    ff <- getCytoPipelineFlowFrame(pipL,
                                   whichQueue = whichQueue,
                                   sampleFile = sampleFile,
                                   objectName = flowFrameName,
                                   path = path)
    
    xChannel <- channelLabel2Name(xChannelLabel, ff)
    if (length(xChannel) == 0) return()
    
    fluoChannels <- flowCore::colnames(ff)[areFluoCols(ff)]
    
    xScale = "linear"
    if (xChannel %in% fluoChannels) {
      xScale = "logicle"
    }
    
    yScale = "linear"
    if (yChannelLabel == " ") {
      yChannel <- NULL
    } else {
      yChannel <- channelLabel2Name(yChannelLabel, ff)
      if (length(yChannel) == 0) return()
      if (yChannel %in% fluoChannels) {
        yScale = "logicle"
      }
    }
    
    if (useMaxValueLinearRange) {
      xLinearRange = c(0, maxValueLinearRange)
      yLinearRange = c(0, maxValueLinearRange)
    } else {
      xLinearRange = NULL
      yLinearRange = NULL
    }
    
    if (useAllCells) {
      nDisplayCells = Inf
    }
        
    p <- ggplotEvents(obj = ff,
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
                      yLinearRange = yLinearRange)
    
    
    theSubtitle <- paste0("nb of events: ", flowCore::nrow(ff))
    
    p <- p + ggplot2::labs(subtitle = theSubtitle)
    p
    
  }
}

plotDiffFlowFrame <- function(experimentNameFrom,
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
                              useMaxValueLinearRange,
                              maxValueLinearRange) {
  
  if (xChannelLabelFrom != " " &&
      yChannelLabelFrom != " " &&
      xChannelLabelTo == xChannelLabelFrom &&
      yChannelLabelTo == yChannelLabelFrom &&
      whichQueueFrom == whichQueueTo &&
      sampleFileFrom == sampleFileTo) {
    message("displaying flow frame comparison plot...")
    #browser()
    pipLFrom <- buildCytoPipelineFromCache(experimentNameFrom,
                                           path = path)
    ffFrom <- getCytoPipelineFlowFrame(pipLFrom,
                                       whichQueue = whichQueueFrom,
                                       sampleFile = sampleFileFrom,
                                       objectName = flowFrameNameFrom,
                                       path = path)
    
    pipLTo <- buildCytoPipelineFromCache(experimentNameTo,
                                         path = path)
    ffTo <- getCytoPipelineFlowFrame(pipLTo,
                                     whichQueue = whichQueueTo,
                                     sampleFile = sampleFileTo,
                                     objectName = flowFrameNameTo,
                                     path = path)
    
    xChannel <- channelLabel2Name(xChannelLabelFrom, ffFrom)
    if (length(xChannel) == 0) return()
    
    fluoChannels <- flowCore::colnames(ffFrom)[areFluoCols(ffFrom)]
    xScale = "linear"
    
    if (xChannel %in% fluoChannels) {
      xScale = "logicle"
    }
    
    yScale = "linear"
    if (yChannelLabelFrom == " ") {
      yChannel <- NULL
    } else {
      yChannel <- channelLabel2Name(yChannelLabelFrom, ffFrom)
      if (length(yChannel) == 0) return()
      if (yChannel %in% fluoChannels) {
        yScale = "logicle"
      }
    }
    
    if (useMaxValueLinearRange) {
      xLinearRange = c(0, maxValueLinearRange)
      yLinearRange = c(0, maxValueLinearRange)
    } else {
      xLinearRange = NULL
      yLinearRange = NULL
    }
    
    
    if (useAllCells) {
      nDisplayCells = Inf
    }
    
    # used to prevent shiny app being frozen by plotly plot
    nEffectiveDisplayCells <- min(10000, nDisplayCells)
    
    p <- ggplotFilterEvents(ffPre = ffFrom,
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
      theSubtitle <- paste0("removed ", round(removedEvents,2),
                            "% of events, ", nEventTo,
                            " good events remaining")
    } else {
      theSubtitle <- paste0("nb events left: ", nEventFrom,
                            ", nb events right: ", nEventTo,
                            "ndiff: ", round(abs(removedEvents), 2), "%")
    }
    
    if (interactive) {
      
      if (is.null(p)) {
        p <- plotly::plotly_empty(type="scatter", mode = "markers")
      } else {
        p <- plotly::layout(p = plotly::ggplotly(p),
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


plotScaleTransformedChannel <- function(ff,
                                         channel,
                                        applyTransform =
                                          c("axis scale only", "data"),
                                        transfoType = 
                                          c("linear", "logicle"),
                                        linA, linB,
                                        negDecades, width, posDecades) {
  applyTransform = match.arg(applyTransform)
  transfoType = match.arg(transfoType)
  if (!is.null(ff)) {
    message("displaying scale transformed channel plot...")
    #browser()
    
    
    if (transfoType == "linear") {
      theTrans <- flowCore::linearTransform(a = linA,
                                            b = linB)
    } else {
      theTrans <- flowCore::logicleTransform(w = width,
                                             m = posDecades + width,
                                             a = negDecades)
    }
    
    theTransList <- flowCore::transformList(from = channel,
                                            tfun = theTrans)
    
    if (applyTransform == "data") {
      runTransforms <- TRUE
      if (transfoType == "logicle") {
        linearRange <- c(0, posDecades + width)
      } else {
        linearRange <- NULL
      }
      
    } else {
      runTransforms <- FALSE
      linearRange <- NULL
    }
    
    ggplotEvents(ff,
                 xChannel = channel,
                 xLinearRange = linearRange,
                 nDisplayCells = 10000,
                 seed = 0,
                 transList = theTransList,
                 runTransforms = runTransforms,
                 fill = "lightgreen",
                 alpha = 0.2)
  }
  
}