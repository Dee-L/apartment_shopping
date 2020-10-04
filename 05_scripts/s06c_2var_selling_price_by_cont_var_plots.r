# Purpose: Plots for selling_price by continuous variable plots
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
    , !exists("engnrd_fctr_vars_seeded_nmrc")
    )) {
        source("05_scripts/s06_qa_plots.R")
    }

# 03 load shiny ####
library(shiny)

# 04 Define UI ####
ui <-
    fluidPage(
        titlePanel(
            "Selling_price by category"
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
                    inputId = "continuous_variable",
                    label = "Select a continuous variable to plot",
                    h3("Select box"),
                    choices = engineered_numeric_vars %>%
                            sort
                    )
                )
            })

    # 05 Save inputs from user in reactive objects ####
    cat_var <- reactive({
        input[["continuous_variable"]]
    })

    # 06 Create scatter plot ####
    output[["scatter_plot"]] <-
        renderPlotly({
            my_scatter_plot(
                df = preprocessed_data %>% .[!is.na(.[[cat_var()]]), ],
                x = cat_var(),
                y = "selling_price",
                title = paste0("Scatter plot for selling_price by ", cat_var())
            )
            })


    # 09 Render plot ####
    output[["scatter_panel"]] <-
        renderUI({
            plotlyOutput(outputId = "scatter_plot")
            })
    }

shinyApp(ui = ui, server = server)