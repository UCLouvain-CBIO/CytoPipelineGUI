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


# run CytoPipeline object first

outputDir <- base::tempdir()

if (!interactive()) pdf(NULL)

rawDataDir <-
    system.file("extdata", package = "CytoPipeline")
experimentName <- "OMIP021_PeacoQC"
sampleFiles <- file.path(rawDataDir, list.files(rawDataDir,
                                                pattern = "Donor"))
jsonDir <- system.file("extdata", package = "CytoPipeline")
jsonPath <- file.path(jsonDir, "pipelineParams.json")

pipL2 <- CytoPipeline(jsonPath,
                      experimentName = experimentName,
                      sampleFiles = sampleFiles)

suppressWarnings(execute(pipL2,
                         rmCache = TRUE,
                         path = outputDir))

test_that("plotSelectedWorkflow works", {
    # pre-processing, existing sample file
    expect_no_error(
        plotSelectedWorkflow(
            experimentName = experimentName,
            whichQueue = "pre-processing",
            sampleFile = sampleFiles[1],
            path = outputDir))
    
    # pre-processing, existing sample file, by index
    expect_no_error(
        plotSelectedWorkflow(
            experimentName = experimentName,
            whichQueue = "pre-processing",
            sampleFile = 1,
            path = outputDir))
    
    # pre-processing, defaulted sample file
    expect_no_error(
        plotSelectedWorkflow(
            experimentName = experimentName,
            whichQueue = "pre-processing",
            sampleFile = NULL,
            path = outputDir))
        
    # scale transform => no comment for sample file not used
    expect_no_error(
        plotSelectedWorkflow(
            experimentName = experimentName,
            whichQueue = "scale transform",
            sampleFile = "any",
            path = outputDir))
    
    # pre-processing, sample file not found => no error message
    expect_no_error(
        plotSelectedWorkflow(
            experimentName = experimentName,
            whichQueue = "pre-processing",
            sampleFile = "any",
            path = outputDir))
        
})

test_that("plotSelectedFlowFrame works", {
    
    # TO DO : reactivate vdiffr tests when CytoPipeline dev version has been 
    # updated with the new OMIP021 dataset (version 1.2 in release or dev)
    # and remove expect_no_error()
    
    p1 <- plotSelectedFlowFrame(
        experimentName = experimentName,
        whichQueue = "pre-processing",
        sampleFile = 1,
        flowFrameName = "remove_debris_obj",
        path = outputDir,
        xChannelLabel = "FSC-A : NA",
        yChannelLabel = "SSC-A : NA",
        useAllCells = TRUE,
        nDisplayCells = 0,
        useFixedLinearRange = TRUE,
        linearRange = c(-100, 262144))
    
    vdiffr::expect_doppelganger(
        "pltSelFF-allc-fixlin-notran", fig = p1)  
    
    p2 <- plotSelectedFlowFrame(
        experimentName = experimentName,
        whichQueue = "pre-processing",
        sampleFile = 1,
        flowFrameName = "remove_debris_obj",
        path = outputDir,
        xChannelLabel = "FSC-A : NA",
        yChannelLabel = "SSC-A : NA",
        useAllCells = FALSE,
        nDisplayCells = 100,
        useFixedLinearRange = FALSE,
        linearRange = NULL)
    
    # vdiffr::expect_doppelganger(
    #     "pltSelFF-100c-nofixlin-notran", fig = p2)
    
    p3 <- plotSelectedFlowFrame(
        experimentName = experimentName,
        whichQueue = "pre-processing",
        sampleFile = 1,
        flowFrameName = "remove_debris_obj",
        path = outputDir,
        xChannelLabel = "Comp-670/30Violet-A : BV785 - CD3",
        yChannelLabel = "Comp-780/60Red-A : APCCy7 - CD4",
        useAllCells = TRUE,
        nDisplayCells = 0,
        useFixedLinearRange = FALSE,
        linearRange = NULL,
        transfoListName = "scale_transform_estimate_obj")
    
    vdiffr::expect_doppelganger(
        "pltSelFF-allc-tran", fig = p3)
    
})

test_that("plotSelectedDiffFlowFrame works", {
    
    # TO DO : reactivate vdiffr tests when CytoPipeline dev version has been 
    # updated with the new OMIP021 dataset (version 1.2 in release or dev)
    # and remove expect_no_error()
    
    expect_no_error({
        p1 <- CytoPipelineGUI:::plotDiffFlowFrame(
            experimentNameFrom = experimentName,
            whichQueueFrom = "pre-processing",
            sampleFileFrom = 1,
            flowFrameNameFrom = "remove_doublets_obj",
            xChannelLabelFrom = "FSC-A : NA",
            yChannelLabelFrom = "SSC-A : NA",
            path = outputDir,
            experimentNameTo = experimentName,
            whichQueueTo = "pre-processing",
            sampleFileTo = 1,
            flowFrameNameTo = "remove_debris_obj",
            xChannelLabelTo = "FSC-A : NA",
            yChannelLabelTo = "SSC-A : NA",
            useAllCells = TRUE,
            nDisplayCells = 0,
            useFixedLinearRange = TRUE,
            linearRange = c(-100, 262144))
        
        # vdiffr::expect_doppelganger(
        #     "pltDiffFF-allc-fixlin-notran", fig = p1)  
        
        p2 <- CytoPipelineGUI:::plotDiffFlowFrame(
            experimentNameFrom = experimentName,
            whichQueueFrom = "pre-processing",
            sampleFileFrom = 1,
            flowFrameNameFrom = "remove_doublets_obj",
            xChannelLabelFrom = "FSC-A : NA",
            yChannelLabelFrom = "SSC-A : NA",
            path = outputDir,
            experimentNameTo = experimentName,
            whichQueueTo = "pre-processing",
            sampleFileTo = 1,
            flowFrameNameTo = "remove_debris_obj",
            xChannelLabelTo = "FSC-A : NA",
            yChannelLabelTo = "SSC-A : NA",
            useAllCells = FALSE,
            nDisplayCells = 100,
            useFixedLinearRange = FALSE,
            linearRange = NULL)
        
        # vdiffr::expect_doppelganger(
        #     "pltDiffFF-100c-nofixlin-notran", fig = p2)
        
        p3 <- CytoPipelineGUI:::plotDiffFlowFrame(
            experimentNameFrom = experimentName,
            whichQueueFrom = "pre-processing",
            sampleFileFrom = 1,
            flowFrameNameFrom = "remove_debris_obj",
            xChannelLabelFrom = "FSC-A : NA",
            yChannelLabelFrom = "Comp-525/50Violet-A : L/D Aqua - Viability",
            path = outputDir,
            experimentNameTo = experimentName,
            whichQueueTo = "pre-processing",
            sampleFileTo = 1,
            flowFrameNameTo = "remove_dead_cells_obj",
            xChannelLabelTo = "FSC-A : NA",
            yChannelLabelTo = "Comp-525/50Violet-A : L/D Aqua - Viability",
            useAllCells = TRUE,
            nDisplayCells = 0,
            useFixedLinearRange = FALSE,
            linearRange = NULL,
            transfoListName = "scale_transform_estimate_obj")
        
        # suppressWarnings(vdiffr::expect_doppelganger(
        #     "pltDiffFF-allc-tran", fig = p3))
    })
    

    
})

test_that("plotScaleTransformedChannel works", {
    
    ff <- CytoPipeline::getCytoPipelineFlowFrame(
        pipL2,
        path = outputDir,
        whichQueue = "scale transform",
        objectName = "flowframe_aggregate_obj"
    )
    
    p1 <- plotScaleTransformedChannel(
        ff,
        channel = "FSC-A",
        transfoType = "linear",
        linA = 0.0002,
        linB = -0.5)
    
    vdiffr::expect_doppelganger(
        "pltScaleTransCh-linear", fig = p1)
    
    p2 <- plotScaleTransformedChannel(
        ff,
        channel = "Comp-670/30Violet-A",
        transfoType = "logicle",
        t = 262144,
        m = 4.5,
        w = 0.5,
        a = 1.0
        # negDecades = 1,
        # width = 0.5,
        # posDecades = 4
    )
    
    vdiffr::expect_doppelganger(
        "pltScaleTransCh-logicle", fig = p2)
    
    p3 <- plotScaleTransformedChannel(
        ff,
        channel = "CD3",
        applyTransform = "data",
        transfoType = "logicle",
        t = 262144,
        m = 4.5,
        w = 0.5,
        a = 1.0
        # negDecades = 1,
        # width = 0.5,
        # posDecades = 4
    )
    
    # vdiffr::expect_doppelganger(
    #     "pltScaleTransCh-logicle-data-mker", fig = p3)  
    
    
    
    
})
