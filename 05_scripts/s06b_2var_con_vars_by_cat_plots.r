# Purpose: Plots for selling_price_rawdata by category
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
    , !exists("cat_vars_for_plots")
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
            "Selling price by category"
            )
        , sidebarLayout(
            uiOutput("sidebarOutput")
            , mainPanel(
                tabsetPanel(type = "tabs",
                    tabPanel(
                        "Violin and Stripchart",
                        uiOutput("violin_panel"),
                        uiOutput("stripchart_panel")
                        ),
                    tabPanel(
                        "Conditional Density",
                        uiOutput("cond_density_panel")
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
                    inputId = "categorical_variable",
                    label = "Select a categorical variable to plot",
                    h3("Select box"),
                    choices = sort(cat_vars_for_plots)
                    )
                , selectInput(
                    inputId = "continuous_variable",
                    label = "Select a continuous variable to plot",
                    h3("Select box"),
                    choices = sort(con_vars_for_plots)
                    )
                )
            })

    # 05 Save inputs from user in reactive objects ####
    cat_var <- reactive({
        input[["categorical_variable"]]
    })

    con_var <- reactive({
        input[["continuous_variable"]]
    })

    targeted_df <- reactive({
        targeted_df <- preprocessed_data[, c(con_var(),
            cat_var())]
        top_31_in_var <-
            sqldf::sqldf(
                paste0(
                    "select ", eval(cat_var()), ", count(*) as count
                from targeted_df
                group by ", eval(cat_var()), "
                 order by count desc
                limit 31"
                )
            )
        targeted_df <- sqldf::sqldf(
            paste0(
                "select a.", eval(con_var()), ", a.", eval(cat_var()),
                " from targeted_df as a
              join top_31_in_var as t
                on a.", eval(cat_var()), " = t.", eval(cat_var())
            )
        )
    })

    # 06 Create violin plot ####
    output[["violin_plot"]] <-
        renderPlotly({
            my_violin_plot(
                df = targeted_df() %>% .[!is.na(.[[cat_var()]]), ],
                x = cat_var(),
                y = con_var(),
                title = paste0("Violin plot for ", con_var(), " by ",
                    cat_var())
            )
            })

    # 07 Create stripchart plot ####
    output[["stripchart"]] <-
        renderPlotly({
            my_stripchart_plot(
                df = targeted_df() %>% .[!is.na(.[[cat_var()]]), ],
                x = cat_var(),
                y = con_var(),
                title = paste0("Stripchart for ", con_var(), " by ",
                    cat_var())
            )
        })

    # 08 Create conditional_density plot ####
    output[["conditional_density"]] <-
        renderPlotly({
            my_conditional_density_plot(
                df = targeted_df() %>% .[!is.na(.[[cat_var()]]), ],
                x = con_var(),
                layers = cat_var(),
                title = paste0(
                    "Conditional density plot for ", con_var(), " by "
                    , cat_var()
                    )
            )
        })

    # 09 Render plots ####
    output[["violin_panel"]] <-
        renderUI({
            plotlyOutput(outputId = "violin_plot")
            })
    output[["stripchart_panel"]] <-
        renderUI({
            plotlyOutput(outputId = "stripchart")
        })
    output[["cond_density_panel"]] <-
        renderUI({
            plotlyOutput(outputId = "conditional_density")
        })
    }

shinyApp(ui = ui, server = server)