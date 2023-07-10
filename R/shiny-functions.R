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

channelLabel2Name <- function(channelLabel, ff) {
    channels <- unname(flowCore::colnames(ff))
    chLabels <- lapply(
        channels,
        FUN = function(ch){
            chMk <- flowCore::getChannelMarker(ff, ch)
            lab <- paste0(chMk$name, " : ", chMk$desc)
            lab
        })
    
    channel <- channels[which(chLabels == channelLabel)]
    return(channel)
}

updateExperiment <- function(
        newExperimentName,
        whichQueueName,
        currentWhichQueueValue,
        sampleListName,
        currentSampleValue,
        path) {
    #browser()
    pipL <- buildCytoPipelineFromCache(
        newExperimentName,
        path = path)
    
    whichQueueChoices <- character()
    
    nPreProcessingSteps <- 
        getNbProcessingSteps(
            pipL, 
            whichQueue = "pre-processing")
    if (nPreProcessingSteps > 0) {
        whichQueueChoices <- 
            c(whichQueueChoices, "pre-processing")
    }
    
    nScaleTransfoSteps <- 
        getNbProcessingSteps(
            pipL,
            whichQueue = "scale transform")
    if (nScaleTransfoSteps > 0) {
        whichQueueChoices <- 
            c(whichQueueChoices, "scale transform")
    }
    
    if (length(whichQueueChoices) == 0)
        stop(
            "experiment without any processing step selected ",
            "=> inconsistency")
    
    newSelection <- currentWhichQueueValue
    if (!(newSelection %in% whichQueueChoices)) {
        newSelection <- NULL
    }
    
    updateSelectInput(
        inputId = whichQueueName,
        choices = whichQueueChoices,
        selected = newSelection)
    
    samples <- sampleFiles(pipL)
    
    if (length(samples) == 0) {
        # specific case when no sample has been found in the cache
        # e.g. scale transform only experiment
        newChoices <- " "
    } else {
        newChoices <- c(" ", samples)
    }
    
    newSelection <- currentSampleValue
    if (!(newSelection %in% newChoices)) {
        newSelection <- " "
    }
    updateSelectInput(
        inputId = sampleListName,
        choices = newChoices,
        selected = newSelection)
}

updateFFList <- function(
        experimentName,
        whichQueue,
        sampleFile,
        path,
        inputId,
        currentValue){
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
        newChoices <- " "
    } else {
        pipL <- buildCytoPipelineFromCache(
            experimentName,
            path = path)
        df <- getCytoPipelineObjectInfos(
            pipL,
            whichQueue = whichQueue,
            sampleFile = sampleFile,
            path = path)
        FFNames <-
            df[
                df$ObjectClass == "flowFrame" | df$ObjectClass == "cytoframe",
                "ObjectName"]
        newChoices <- c(" ", FFNames)
    }
    
    newSelection <- currentValue
    if (!(newSelection %in% newChoices)) {
        newSelection <- " "
    }
    updateSelectInput(
        inputId = inputId, 
        choices = newChoices,
        selected = newSelection)
    
}

updateFFListForTransDisplay <- function(
        experimentName,
        path,
        inputId,
        currentValue){
    pipL <- buildCytoPipelineFromCache(
        experimentName,
        path = path)
    df <- getCytoPipelineObjectInfos(
        pipL,
        whichQueue = "scale transform",
        sampleFile = NULL,
        path = path)
    FFNames <-
        df[
            df$ObjectClass == "flowFrame" | df$ObjectClass == "cytoframe",
            "ObjectName"]
    
    if (length(FFNames) == 0) {
        # no flow frame in the scale transform queue
        # => build synthetic one from pre-processing queue
        df <- getCytoPipelineObjectInfos(
            pipL,
            whichQueue = "pre-processing",
            sampleFile = 1,
            path = path)
        FFNames <- 
            df[
                df$ObjectClass == "flowFrame" | df$ObjectClass == "cytoframe",
                "ObjectName"]
        FFNames <- paste0("Synth. agg. - ", FFNames)
    }
    newChoices <- c(" ", FFNames)
    
    
    newSelection <- currentValue
    if (!(newSelection %in% newChoices)) {
        newSelection <- " "
    }
    updateSelectInput(
        inputId = inputId, 
        choices = newChoices,
        selected = newSelection)
}


updateTransList <- function(
        experimentName,
        path,
        inputId,
        currentValue){
    if (experimentName != " ") {
        pipL <- buildCytoPipelineFromCache(
            experimentName,
            path = path)
        df <- getCytoPipelineObjectInfos(
            pipL,
            whichQueue = "scale transform",
            sampleFile = NULL,
            path = path)
        scaleTransfoNames <-
            df[df$ObjectClass == "transformList", "ObjectName"]
        
        newChoices <- c(" ", scaleTransfoNames)
        
        newSelection <- currentValue
        if (!(newSelection %in% newChoices)) {
            newSelection <- " "
        }
        
        updateSelectInput(
            inputId = inputId, 
            choices = newChoices,
            selected = newSelection)
    }
}

updateChannelMarkerList <- function(
        experimentName,
        whichQueue,
        sampleFile,
        path,
        flowFrameName,
        inputIds,
        currentValues){
    if (flowFrameName != " ") {
        pipL <- buildCytoPipelineFromCache(
            experimentName,
            path = path)
        ff <- getCytoPipelineFlowFrame(
            pipL,
            whichQueue = whichQueue,
            sampleFile = sampleFile,
            objectName = flowFrameName,
            path = path)
        
        # sort out channels and and channel_labels
        channels <- unname(flowCore::colnames(ff)[areSignalCols(ff)])
        
        # add time channel
        timeCh <- findTimeChannel(ff)
        
        channels <- c(channels, timeCh)
        
        chLabels <- lapply(
            channels,
            FUN = function(ch){
                chMk <- flowCore::getChannelMarker(ff, ch)
                lab <- paste0(chMk$name, " : ", chMk$desc)
                lab
            })
        for (i in seq_along(inputIds)) {
            newChoices <- c(" ", chLabels)
            
            newSelection <- currentValues[i]
            if (!(newSelection %in% newChoices)) {
                newSelection <- " "
            }
            
            updateSelectInput(
                inputId = inputIds[i],
                choices = newChoices,
                selected = newSelection)
        }
        
    } else {
        for (inputId in inputIds) {
            updateSelectInput(
                inputId = inputId,
                choices = c(" "))
        }
    }
    
    
}




