# Purpose: Density plots for numerical features in dataset
# Author: David Gray Lassiter, PhD
# Date: 2020-sep-22
# Version:

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# May want to change this to offer two plots or (better) two traces on same plot

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
        "htmlwidgets",
        "sqldf",
        "shiny"
    )

install.packages("shiny")
installMyPkgs(pkgs)

# 02 Load data for this app if it isn't already loaded ####

if (any(
    !exists("preprocessedData")
    , !exists("numericVarsForDensityPlots")
    )) {
        source("05_scripts/s06_qaPlotsPrep.R")
    }


# 03 load shiny ####
library(shiny)

# 04 Define UI ####
ui <-
    fluidPage(
        titlePanel("Density plots of numerical variables")
        , sidebarLayout(
            uiOutput("sidebarOutput")
            , mainPanel(uiOutput("mainPanel"))
            )
        )

# 04 Define server ####
server <- function(input, output) {
    output[["sidebarOutput"]] <-
        renderUI({
            sidebarPanel(
                selectInput(
                    inputId = "continuousVariable",
                    label = "Select a continuous variable to plot",
                    h3("Select box"),
                    choices =
                        c("sellingPriceRawData",
                               numericVarsForDensityPlots %>%
                                 .[. != "sellingPriceRawData"]
                          )
                    )
                )
            })

    # 05 Save inputs from user in reactive objects ####
    contVar <- reactive({
        input[["continuousVariable"]]
    })

    # 06 Create plot ####
    output[["densityPlot"]] <-
        renderPlotly({
            myBasicDensityPlot(
                df = preprocessedData %>% .[!is.na(.[[contVar()]]), ],
                x = contVar(),
                title = paste0("Density plot of ", contVar())
                )
            })

    # 07 Render the plot ####
    output[["mainPanel"]] <-
        renderUI({
            plotlyOutput(outputId = "densityPlot")
            })
    }

shinyApp(ui = ui, server = server)
