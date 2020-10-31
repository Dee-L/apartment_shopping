# Purpose: Plots for various contVars by category
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
    , !exists("catVarsForAggPlots")
    , !exists("conVarsForAggs")
    )) {
        source("05_scripts/s06_qaPlotsPrep.R")
    }


# 03 load shiny ####
library(shiny)

# 04 Define UI ####
ui <-
    fluidPage(
        titlePanel(
            "Various continuous variables by category"
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

# 05 Define server ####
server <- function(input, output, session) {
    output[["sidebarOutput"]] <-
        renderUI({
            sidebarPanel(
                selectInput(
                    inputId = "categoricalVariable",
                    label = "Select a categorical variable to plot",
                    h3("Select box"),
                    choices = sort(catVarsForAggPlots)
                    )
                , selectInput(
                    inputId = "continuousVariable",
                    label = "",
                    choices = sort(conVarsForAggs)[1]
                    )
                )
            })

    # 06 Updates the second drop-down list based on first selection ####
    observe({
        updateSelectInput(
            session
            , "continuousVariable"
            , label = "Select a continuous variable to plot"
            , h3("Select box"),
            choices =
                sort({
                    # 07 Process first choice for filtering second choice ####
                    groupFactorForAggs <-
                        catVar() %>%
                        left(., nchar(.) - 8)
                    # 08 filter second choice - gets presented to user ####
                        conVarsForAggs %>%
                        stringr::str_subset(groupFactorForAggs)
                    }))
    })

    # 09 Save inputs from user in reactive objects ####
    catVar <- reactive({
        input[["categoricalVariable"]]
    })

    conVar <- reactive({
        input[["continuousVariable"]]
    })

    # 10 Reduce to 31 categories ####
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

    # 11 Create violin plot ####
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

    # 12 Create stripchart plot ####
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

    # 13 Create conditionalDensity plot ####
    output[["conditionalDensity"]] <-
        renderPlotly({
            myConditionalDensityPlot(
                df = targetedDf() %>% .[!is.na(.[[catVar()]]), ],
                x = conVar(),
                layers = catVar(),
                title = paste0(
                    "Conditional density plot for ", conVar(), "  by "
                    , catVar()
                    )
            )
        })

    # 14 Render plots ####
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
