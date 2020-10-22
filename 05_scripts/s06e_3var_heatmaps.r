# Purpose: Plots for various cont_vars by two discrete variables
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

install_my_pkgs(pkgs)

# 02 Load data for this app if it isn't already loaded ####

if (any(
    !exists("preprocessed_data"),
    !exists("discrete_vars_for_heatmaps"),
    !exists("con_vars_for_aggs")
)) {
    source("05_scripts/s06_qa_plots_prep.R")
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
            mainPanel(uiOutput("heatmap_panel"))
        )
    )

# 05 Define server ####
server <- function(input, output, session) {
    output[["sidebarOutput"]] <-
        renderUI({
            sidebarPanel(
                selectInput(
                    inputId = "color_var",
                    label = "Select a continuous variable to plot as color",
                    h3("Select box"),
                    choices = sort(con_vars_for_aggs)
                    )
                , selectInput(
                    inputId = "x_var",
                    label = "Select a variable for the x-axis",
                    h3("Select box"),
                    choices = sort(con_vars_for_aggs)
                    )
                , selectInput(
                    inputId = "y_var",
                    label = "",
                    choices = sort(con_vars_for_aggs)[2]
                    )
            )
        })

    # 06 Updates the second drop-down list based on first selection ####
    observe({
        updateSelectInput(
            session,
            "y_var",
            label = "Select a variable for the y-axis",
            h3("Select box"),
            choices = setdiff(sort(con_vars_for_aggs), x_var())
        )
    })

    # 09 Save inputs from user in reactive objects ####
    color_var <- reactive({
        input[["color_var"]]
    })

    x_var <- reactive({
        input[["x_var"]]
    })

    y_var <- reactive({
        input[["y_var"]]
    })

    targeted_df <- reactive({
        targeted_df <-
            preprocessed_data[, c(x_var(), y_var(), color_var())]

    # 11 Create heatmap ####
    output[["heatmap"]] <-
        renderPlotly({
            my_heatmap(
                df = targeted_df(),
                x = x_var(),
                y = y_var(),
                color = color_var(),
                aggfxn = "median",
                title = 
                    paste0("Heatmap: ", color_var(), " by ",
                    x_var(), " and ", y_var())
                )
        })
        })

    # 14 Render plots ####
    output[["heatmap_panel"]] <-
        renderUI({
            plotlyOutput(outputId = "heatmap")
        })
}

shinyApp(ui = ui, server = server)
