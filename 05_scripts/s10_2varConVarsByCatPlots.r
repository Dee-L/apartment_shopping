# Purpose: Plots for sellingPriceRawData by category
# Author: David Gray Lassiter, PhD
# Date: 2020-oct-04
# Version:


# 01 Ensure all pkgs in this script are installed ####
pkgs <-
    c(
        "htmlwidgets",
        "sqldf",
        "shiny"
    )

activatePkgs(pkgs)

# 02 Load data for this app if it isn't already loaded ####

if (any(
    !exists("preprocessedData")
    , !exists("catVarsForPlots")
    , !exists("conVarsForPlots")
    )) {
        source("05_scripts/s07_defineVariableGroups.R")
    }


# 03 load shiny ####s
library(shiny)

# 04 Define UI ####
ui <-
    fluidPage(
        titlePanel(
            "Selling price by category"
            )
        , sidebarLayout(
            uiOutput("sidebarOutput")
            , mainPanel(
                tabsetPanel(type = "tabs",
                    tabPanel(
                        "Violin and Stripchart",
                        uiOutput("violinPanel"),
                        uiOutput("stripchartPanel")
                        ),
                    tabPanel(
                        "Conditional Density",
                        uiOutput("conditionalDensityPanel")
                    )
                    )
                )
            )
        )

# 04 Define server ####
server <- function(input, output) {
    output[["sidebarOutput"]] <-
        renderUI({
            sidebarPanel(
                selectInput(
                    inputId = "categoricalVariable",
                    label = "Select a categorical variable to plot",
                    h3("Select box"),
                    choices = sort(catVarsForPlots)
                    )
                , selectInput(
                    inputId = "continuousVariable",
                    label = "Select a continuous variable to plot",
                    h3("Select box"),
                    choices = conVarsForPlots
                    )
                )
            })

    # 05 Save inputs from user in reactive objects ####
    catVar <- reactive({
        input[["categoricalVariable"]]
    })

    conVar <- reactive({
        input[["continuousVariable"]]
    })

    targetedDf <- reactive({
        targetedDf <- preprocessedData[, c(conVar(),
            catVar())]
        top31InVar <-
            sqldf::sqldf(
                paste0(
                    "select ", eval(catVar()), ", count(*) as count
                from targetedDf
                group by ", eval(catVar()), "
                 order by count desc
                limit 31"
                )
            )
        targetedDf <- sqldf::sqldf(
            paste0(
                "select a.", eval(conVar()), ", a.", eval(catVar()),
                " from targetedDf as a
              join top31InVar as t
                on a.", eval(catVar()), " = t.", eval(catVar())
            )
        )
    })

    # 06 Create violin plot ####
    output[["violinPlot"]] <-
        renderPlotly({
            myViolinPlot(
                df = targetedDf() %>% .[!is.na(.[[catVar()]]), ],
                x = catVar(),
                y = conVar(),
                title = paste0("Violin plot for ", conVar(), " by ",
                    catVar())
            )
            })

    # 07 Create stripchart plot ####
    output[["stripchart"]] <-
        renderPlotly({
            myStripchartPlot(
                df = targetedDf() %>% .[!is.na(.[[catVar()]]), ],
                x = catVar(),
                y = conVar(),
                title = paste0("Stripchart for ", conVar(), " by ",
                    catVar())
            )
        })

    # 08 Create conditionalDensity plot ####
    output[["conditionalDensity"]] <-
        renderPlotly({
            myConditionalDensityPlot(
                df = targetedDf() %>% .[!is.na(.[[catVar()]]), ],
                x = conVar(),
                layers = catVar(),
                title = paste0(
                    "Conditional density plot for ", conVar(), " by "
                    , catVar()
                    )
            )
        })

    # 09 Render plots ####
    output[["violinPanel"]] <-
        renderUI({
            plotlyOutput(outputId = "violinPlot")
            })
    output[["stripchartPanel"]] <-
        renderUI({
            plotlyOutput(outputId = "stripchart")
        })
    output[["conditionalDensityPanel"]] <-
        renderUI({
            plotlyOutput(outputId = "conditionalDensity")
        })
    }

shinyApp(ui = ui, server = server)
