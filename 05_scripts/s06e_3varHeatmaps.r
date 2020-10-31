# Purpose: Plots for various contVars by 2 discrete variables
# Author: David Gray Lassiter, PhD
# Date: 2020-Oct-22
# Version:

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
        "htmlwidgets",
        "sqldf",
        "shiny"
    )

installMyPkgs(pkgs)

# 02 Load data for this app if it isn't already loaded ####

if (any(
    !exists("preprocessedData"),
    !exists("discreteVarsForHeatmaps"),
    !exists("conVarsForAggs")
)) {
    source("05_scripts/s06_qaPlotsPrep.R")
}

# 03 load shiny ####
library(shiny)

# 04 Define UI ####
ui <-
    fluidPage(
        titlePanel(
            "Various continuous variables by two discrete variables"
        ),
        sidebarLayout(
            uiOutput("sidebarOutput"),
            mainPanel(uiOutput("heatmapPanel"))
        )
    )

# 05 Define server ####
server <- function(input, output, session) {
    output[["sidebarOutput"]] <-
        renderUI({
            sidebarPanel(
                selectInput(
                    inputId = "colorVar",
                    label = "Select a continuous variable to plot as color",
                    h3("Select box"),
                    choices = conVarsForPlots
                    )
                , selectInput(
                    inputId = "xVar",
                    label = "",
                    h3("Select box"),
                    choices = sort(discreteVarsForHeatmaps)[7]
                    )
                , selectInput(
                    inputId = "yVar",
                    label = "",
                    choices = sort(discreteVarsForHeatmaps)[17]
                    )
            )
        })

    # 06 Updates the second drop-down lists based on the other selections ####
    observe({
        updateSelectInput(
            session,
            "xVar",
            label = "Select a variable for the x-axis",
            h3("Select box"),
            choices = setdiff(sort(discreteVarsForHeatmaps), yVar())
        )
    })
    observe({
        updateSelectInput(
            session,
            "yVar",
            label = "Select a variable for the y-axis",
            h3("Select box"),
            choices = setdiff(sort(discreteVarsForHeatmaps), xVar())
        )
    })

    # 09 Save inputs from user in reactive objects ####
    colorVar <- reactive({
        input[["colorVar"]]
    })

    xVar <- reactive({
        input[["xVar"]]
    })

    yVar <- reactive({
        input[["yVar"]]
    })

    targetedDf <- reactive({
        preprocessedData[, c(xVar(), yVar(),colorVar())]
    })

    # 11 Create heatmap ####
    output[["heatmap"]] <-
        renderPlotly({
            myHeatmap(
                df = targetedDf(),
                x = xVar(),
                y = yVar(),
                color = colorVar(),
                aggfxn = "median",
                title = 
                    paste0("Heatmap: ", colorVar(), " by ",
                    xVar(), " and ", yVar())
                )
        })

    # 14 Render plots ####
    output[["heatmapPanel"]] <-
        renderUI({
            plotlyOutput(outputId = "heatmap")
        })
}

shinyApp(ui = ui, server = server)
