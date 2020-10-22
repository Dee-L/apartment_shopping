# Purpose: Plots for selling_price_rawdata by continuous variable plots
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

install_my_pkgs(pkgs)

# 02 Load data for this app if it isn't already loaded ####

if (any(
    !exists("preprocessed_data")
    , !exists("con_vars_for_plots")
    )) {
        source("05_scripts/s06_qa_plots_prep.R")
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
            , mainPanel(uiOutput("scatter_panel"))
            )
        )

# 04 Define server ####
server <- function(input, output) {
    output[["sidebarOutput"]] <-
        renderUI({
            sidebarPanel(
                selectInput(
                    inputId = "x_variable",
                    label = "Select variable to plot on x-axis",
                    h3("Select box"),
                    choices = sort(con_vars_for_plots)
                    )
                    , selectInput(
                        inputId = "y_variable",
                        label = "Select variable to plot on y-axis",
                        h3("Select box"),
                        choices = con_vars_for_plots
                        )
                )
            })

    # 05 Save inputs from user in reactive objects ####
    x_var <- reactive({
        input[["x_variable"]]
        })

    y_var <- reactive({
        input[["y_variable"]]
        })

    # 06 Create scatter plot ####
    output[["scatter_plot"]] <-
        renderPlotly({
            my_scatter_plot(
                df = preprocessed_data %>% .[!is.na(.[[x_var()]]), ],
                x = x_var(),
                y = y_var(),
                title = paste0("Scatter plot for ", y_var(), " by ",
                    x_var())
            )
            })

    # 09 Render plot ####
    output[["scatter_panel"]] <-
        renderUI({
            plotlyOutput(outputId = "scatter_plot")
            })
    }

shinyApp(ui = ui, server = server)