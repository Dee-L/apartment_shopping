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

install_my_pkgs(pkgs)

# 02 Load data for this app if it isn't already loaded ####

if (any(
    !exists("preprocessed_data")
    , !exists("original_numeric_vars")
    , !exists("engineered_numeric_vars")
    )) {
        source("05_scripts/s06_qa_plots.R")
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
                    inputId = "continuous_variable",
                    label = "Select a continuous variable to plot",
                    h3("Select box"),
                    choices =
                        c(original_numeric_vars, engineered_numeric_vars) %>%
                            sort
                    )
                )
            })

    # 05 Save inputs from user in reactive objects ####
    cont_var <- reactive({
        input[["continuous_variable"]]
    })

    # 06 Create plot ####
    output[["density_plot"]] <-
        renderPlotly({
            my_basic_density_plot(
                df = preprocessed_data %>% .[!is.na(.[[cont_var()]]), ],
                x = cont_var(),
                title = paste0("Density plot of ", cont_var())
                )
            })

    # 07 Render the plot ####
    output[["mainPanel"]] <-
        renderUI({
            plotlyOutput(outputId = "density_plot")
            })
    }

shinyApp(ui = ui, server = server)