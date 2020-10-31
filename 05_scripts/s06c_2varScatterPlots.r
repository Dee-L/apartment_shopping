# Purpose: Plots for sellingPriceRawData by continuous variable plots
# Author: David Gray Lassiter, PhD
# Date: 2020-oct-04
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
    !exists("preprocessedData")
    , !exists("conVarsForPlots")
    )) {
        source("05_scripts/s06_qaPlotsPrep.R")
    }

# 03 load shiny ####
library(shiny)

# 04 Define UI ####
ui <-
    fluidPage(
        titlePanel(
            "Selling price by continuous variable"
            )
        , sidebarLayout(
            uiOutput("sidebarOutput")
            , mainPanel(uiOutput("scatterPanel"))
            )
        )

# 04 Define server ####
server <- function(input, output) {
    output[["sidebarOutput"]] <-
        renderUI({
            sidebarPanel(
                selectInput(
                    inputId = "xVariable",
                    label = "Select variable to plot on x-axis",
                    h3("Select box"),
                    choices = sort(conVarsForPlots)
                    )
                    , selectInput(
                        inputId = "yVariable",
                        label = "Select variable to plot on y-axis",
                        h3("Select box"),
                        choices = conVarsForPlots
                        )
                )
            })

    # 05 Save inputs from user in reactive objects ####
    xVar <- reactive({
        input[["xVariable"]]
        })

    yVar <- reactive({
        input[["yVariable"]]
        })

    # 06 Create scatter plot ####
    output[["scatterPlot"]] <-
        renderPlotly({
            myScatterPlot(
                df = preprocessedData %>% .[!is.na(.[[xVar()]]), ],
                x = xVar(),
                y = yVar(),
                title = paste0("Scatter plot for ", yVar(), " by ",
                    xVar())
            )
            })

    # 09 Render plot ####
    output[["scatterPanel"]] <-
        renderUI({
            plotlyOutput(outputId = "scatterPlot")
            })
    }

shinyApp(ui = ui, server = server)