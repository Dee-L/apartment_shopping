# Purpose: Plots for various cont_vars by 4 discrete variables
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
            "Various continuous variables by 4 discrete variables"
        ),
        sidebarLayout(
            uiOutput("sidebarOutput"),
            mainPanel(
                width = 12
                , height = 9
                , uiOutput("heatmap_panel"))
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
                    choices = con_vars_for_plots
                ),
                selectInput(
                    inputId = "x_var",
                    label = "",
                    h3("Select box"),
                    choices = sort(discrete_vars_for_heatmaps)[7]
                ),
                selectInput(
                    inputId = "y_var",
                    label = "",
                    choices = sort(discrete_vars_for_heatmaps)[17]
                ),
                selectInput(
                    inputId = "x_facet_var",
                    label = "",
                    h3("Select box"),
                    choices = sort(discrete_vars_for_heatmaps)[13]
                ),
                selectInput(
                    inputId = "y_facet_var",
                    label = "",
                    choices = sort(discrete_vars_for_heatmaps)[20]
                )
            )
        })

    # 06 Updates the second drop-down list based on the other selections ####
    observe({
        updateSelectInput(
            session,
            "x_var",
            label = "Select a variable for the x-axis",
            h3("Select box"),
            choices =
                setdiff(sort(discrete_vars_for_heatmaps)
                , c(y_var(), x_facet_var(), y_facet_var()))
            )
    })
    observe({
        updateSelectInput(
            session,
            "y_var",
            label = "Select a variable for the y-axis",
            h3("Select box"),
            choices =
                setdiff(sort(discrete_vars_for_heatmaps)
                , c(x_var(), x_facet_var(), y_facet_var()))
            )
    })
    observe({
        updateSelectInput(
            session,
            "x_facet_var",
            label = "Select a variable for the x-facet",
            h3("Select box"),
            choices =
                setdiff(sort(discrete_vars_for_heatmaps)
                , c(x_var(), y_var(), y_facet_var()))
            )
    })
    observe({
        updateSelectInput(
            session,
            "y_facet_var",
            label = "Select a variable for the y-facet",
            h3("Select box"),
            choices =
                setdiff(
                    sort(discrete_vars_for_heatmaps),
                    c(x_var(), y_var(), x_facet_var())
                )
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

    x_facet_var <- reactive({
        input[["x_facet_var"]]
    })

    y_facet_var <- reactive({
        input[["y_facet_var"]]
    })

    targeted_df <-
        reactive({preprocessed_data[, c(
            x_var(), y_var(), x_facet_var(), y_facet_var(), color_var())]
    })

    # 11 Create heatmap ####
    output[["heatmap"]] <-
        renderPlotly({
            my_fourvar_heatmap(
                df = targeted_df(),
                x = x_var(),
                y = y_var(),
                x_facet_var = x_facet_var(),
                y_facet_var = y_facet_var(),
                color = color_var(),
                aggfxn = "median",
                title =
                    paste0(
                        "Heatmap: ", color_var(), " by ",
                        x_var(), ", ", y_var(), ", ", x_facet_var(),
                        ", and ", y_facet_var()
                    )
            )
        })

    # 14 Render plots ####
    output[["heatmap_panel"]] <-
        renderUI({
            plotlyOutput(outputId = "heatmap")
        })
}

shinyApp(ui = ui, server = server)
