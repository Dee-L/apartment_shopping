# Purpose: Functions and settings for plotting
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# Revisions:
# Author:
# Date: YYYY-MMM-DD
# Revised Version:

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
  c(
    "ggplot2"
    , "plotly"
    , "htmlwidgets"
    , "sqldf"
    , "forcats"
  )

install_my_pkgs(pkgs)

library("ggplot2")
library("plotly")

# 01 Get plot themes and templates ####

plot_theme <-
  theme_bw() +
  theme(plot.background = element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90,
                                   hjust = 1),
        axis.ticks = element_blank(),
        axis.line = element_line(color = "black"),
        legend.title = element_blank())

# 02 Density plot ####

my_basic_density_plot <- function(df, x, title = "") {
  ggplotly(
    ggplot(df) +
      geom_density(
        mapping = aes(
          x = df[[x]],
          text = paste0("\ndf[[x]] is ", x, "."))) +
      xlab(x) +
      plot_theme +
      scale_x_continuous(labels = scales::scientific) +
      scale_y_continuous(labels = scales::scientific) +
      ggtitle(title)
  )
}

# 03 Layered density plot ####

my_layered_density_plot <- function(df, x, layers, title = "") {
  (ggplotly(
    ggplot(df) +
      geom_density(
        mapping = aes(
          x = df[[x]],
          color = as.factor(df[[layers]]), fill = as.factor(df[[layers]]),
          alpha = 0.01,
          text =
            paste0(
              "\ndf[[x]] is "
              , x
              , ".\nas.factor(df[[layers]]) is "
              , layers
              , "."
              ))) +
      scale_color_viridis_d(option = "magma") +
      scale_fill_viridis_d(option = "magma") +
      xlab(x) +
      plot_theme +
      scale_x_continuous(labels = scales::scientific) +
      scale_y_continuous(labels = scales::scientific) +
      ggtitle(title),
    tooltip = c("x", "fill", "text")
    )
  ) %>% layout(legend = list(x = 100, y = 0.5))
}

# 04 Violin plot ####

my_violin_plot <- function(df, x, y, title = "") {
  (ggplotly(
    ggplot(df) +
      geom_violin(
        mapping = aes(
          x = as.factor(df[[x]]),
          fill = as.factor(df[[x]]),
          y = df[[y]],
          text =
            paste0(
                "\nas.factor(df[[x]]) is "
            , x
            , ".\ndf[[y]]) is "
            , y
            , "."
            ))) +
      scale_fill_viridis_d(option = "magma") +
      xlab(x) +
      ylab(y) +
      plot_theme +
      scale_y_continuous(labels = scales::scientific) +
      ggtitle(title),
    tooltip = c("x", "y", "text")
    )
  ) %>% layout(legend = list(x = 100, y = 0.5))
}

# 05 Stripchart ####

my_stripchart_plot <- function(df, x, y, title = "") {
  (ggplotly(
    ggplot(df) +
      geom_jitter(
        aes(
          x = as.factor(df[[x]]),
          fill = as.factor(df[[x]]),
          color = as.factor(df[[x]]),
          y = df[[y]],
          text =
            paste0(
                "\nas.factor(df[[x]]) is "
            , x
            , ".\ndf[[y]]) is "
            , y
            , "."
            )),
        position = position_jitter(0.2)) +
      scale_fill_viridis_d(option = "magma") +
      scale_color_viridis_d(option = "magma", direction = -1) +
      xlab(x) +
      ylab(y) +
      plot_theme +
      scale_y_continuous(labels = scales::scientific) +
      ggtitle(title),
    tooltip = c("x", "y", "text")
    )
  ) %>% layout(legend = list(x = 100, y = 0.5))
}

# 06 Conditional density ####

my_conditional_density_plot <-
    function(df, x, layers, hlines = "1/3s", vlines = "1/3s", title = "") {
    (ggplotly(
        ggplot(df) +
        geom_density(
            mapping = aes(
            x = df[[x]],
            y = stat(count),
            color = as.factor(df[[layers]]), fill = as.factor(df[[layers]]),
            text =
                paste0(
                    "\ndf[[x]] is "
                , x
                , ".\n as.factor(df[[layers]] is "
                , layers
                , ".")),
            position = "fill") +
        scale_color_viridis_d(option = "magma") +
        scale_fill_viridis_d(option = "magma") + {
            if (hlines[[1]] == "1/2s") {
            geom_hline(yintercept = c(1 / 2), color = "white")
            } else if (hlines[[1]] == "1/3s") {
            geom_hline(yintercept = c(1 / 3, 2 / 3), color = "white")
            } else if (hlines[[1]] == "1/4s") {
            geom_hline(yintercept = c(1 / 4, 2 / 4, 3 / 4), color = "white")
            } else if (hlines[[1]] == "1/5s") {
            geom_hline(
                yintercept = c(1 / 5, 2 / 5, 3 / 5, 4 / 5), color = "white")
            } else if (is.numeric(hlines)) {
            geom_hline(yintercept = hlines, color = "white")
            }
        } + {
            if (vlines[[1]] == "1/2s") {
                geom_vline(
                    xintercept = c(1 / 2) *
                        (max(df[[x]]) - min(df[[x]]))  + min(df[[x]])
                        , color = "white")
            } else if (vlines[[1]] == "1/3s") {
                geom_vline(
                    xintercept = c(1 / 3, 2 / 3) *
                        (max(df[[x]]) - min(df[[x]]))  + min(df[[x]])
                        , color = "white")
            } else if (vlines[[1]] == "1/4s") {
                geom_vline(
                    xintercept = c(1 / 4, 2 / 4, 3 / 4) *
                        (max(df[[x]]) - min(df[[x]]))  + min(df[[x]])
                        , color = "white")
            } else if (vlines[[1]] == "1/5s") {
                geom_vline(
                    xintercept = c(1 / 5, 2 / 5, 3 / 5, 4 / 5) *
                        (max(df[[x]]) - min(df[[x]]))  + min(df[[x]])
                        , color = "white")
            } else if (is.numeric(vlines)) {
                geom_vline(
                    xintercept = vlines, color = "white")
            }
        } +
        xlab(x) +
        plot_theme +
        scale_x_continuous(
            labels = scales::scientific
            , limits = c(min(df[[x]]), max(df[[x]]))
            ) +
        scale_y_continuous(labels = scales::scientific) +
        ggtitle(title),
        tooltip = c("x", "fill", "density", "text")
        )
    ) %>% layout(legend = list(x = 100, y = 0.5))
}

# 07 Scatter plot ####

my_scatter_plot <- function(df, x, y, title = "") {

  lm_fit <- lm(df[[y]] ~ df[[x]], data = df)
  df[["predicted_y"]] <- predict(lm_fit, df)

  (ggplotly(
    ggplot(df) +
      geom_point(
        aes(
          x = df[[x]],
          fill = df[[y]],
          color = df[[y]],
          y = df[[y]],
          text = paste0("\ndf[[x]] is ", x, ".\ndf[[y]]) is ", y, "."))) +
      scale_fill_viridis_c(option = "magma", direction = -1) +
      scale_color_viridis_c(option = "magma") +
      geom_line(aes(x = df[[x]],
                    y = df[["predicted_y"]]),
                color = "black",
                linetype = "dashed"
                ) +
      xlab(x) +
      ylab(y) +
      plot_theme +
      theme(legend.position = "none") +
      scale_x_continuous(labels = scales::scientific) +
      scale_y_continuous(labels = scales::scientific) +
      ggtitle(title),
    tooltip = c("x", "y", "text")
    )
  ) %>% layout(legend = list(x = 100, y = 0.5))
}

# 08 Heatmap ####

my_heatmap <- function(df, x, y, color, aggfxn = "median", title = "") {

  df %<>% select(x, y, color)

  df[[x]] %<>% as.factor
  df[[y]] %<>% as.factor

  df <-
    sqldf::sqldf(
      paste0(
        "select ", x, ", ", y, ", ", aggfxn, "(", color, ")
        from df
        group by 1, 2"))

  ggplotly(
    ggplot(df) +
      geom_tile(
        aes(
          x = df[[x]],
          y = df[[y]],
          fill = df[[paste0(aggfxn, "(", color, ")")]],
          text =
            paste0(
                "\nas.factor(df[[x]]) is "
                , x
                , ".\ndf[[y]]) is "
                , y
                , "\ndf[[color]] is "
                , aggfxn
                , " "
                , color
                , "."
                )
        )
      ) +
      scale_fill_gradient2(midpoint = median(df[[paste0(aggfxn, "(", color, ")")]]),
                           low = "blue", mid = "white",
                           high = "red", space = "Lab") +
      xlab(x) +
      ylab(y) +
      labs(fill = paste0(aggfxn, "(", color, ")")) +
      plot_theme +
      ggtitle(title),
    tooltip = c("x", "y", "fill", "text")
  ) %>% layout(legend = list(x = 100, y = 0.5))
}

# 09 Calendar heatmap ####

my_fourvar_heatmap <-
    function(
        df
        , x_var
        , y_var
        , x_facet_var
        , y_facet_var
        , color_var
        , aggfxn = "median"
        , title = ""
        ) {

    df %<>% select(x_var, y_var, x_facet_var, y_facet_var, color_var)

    df[[x_var]] %<>% as.factor
    df[[y_var]] %<>% as.factor %>% forcats::fct_rev(.)
    df[[x_facet_var]] %<>% as.factor
    df[[y_facet_var]] %<>% as.factor

    df <-
        sqldf::sqldf(
        paste0(
            "select "
            , x_var
            , ", "
            , y_var
            , ", "
            , x_facet_var
            , ", "
            , y_facet_var
            , ", "
            , aggfxn
            , "("
            , color_var
            , ")
            from df
            group by 1, 2, 3, 4"))

    ggplotly(
        ggplot(df) +
        geom_tile(
            aes(
            x = df[[x_var]],
            y = df[[y_var]],
            fill = df[[paste0(aggfxn, "(", color_var, ")")]],
            text =
                paste0(
                    "\nas.factor(df[[x_var]]) is "
                    , x_var
                    , ".\ndf[[y_var]]) is "
                    , y_var
                    , ".\ndf[[x_facet_var]]) is "
                    , x_facet_var
                    , ".\ndf[[y_facet_var]]) is "
                    , y_facet_var
                    , "\ndf[[color_var]] is "
                    , aggfxn
                    , " "
                    , color_var
                    , "."
                    )
            )
        ) +
        facet_grid(df[[y_facet_var]] ~ df[[x_facet_var]]) +
        scale_fill_gradient2(
            midpoint = median(df[[paste0(aggfxn, "(", color_var, ")")]]),
                            low = "blue", mid = "white",
                            high = "red", space = "Lab") +
        xlab(x_var) +
        ylab(y_var) +
        labs(fill = paste0(aggfxn, "(", color_var, ")")) +
        plot_theme +
        ggtitle(title),
        tooltip = c("x", "y", "fill", "text")
    ) %>% layout(legend = list(x = 100, y = 0.5))

}

# 10 Timelapse (continous vs continuous vs time): Need to find work from KRY
# to build this again!

# 11 Old parallel plots - not with new style yet ####

#This function makes parallel plots of pairwise comparisons which are specified
#as a "finding". The "finding" has to take the form of:
#   c("some.long.data.frame",
#   "independent.variable.factor.to.be.plotted.at.x = 1",
#   "independent.variable.factor.to.be.plotted.at.x = 2")

fx.parallel.plots <- function(finding.to.plot,
                              independent.variable.to.be.plotted) {
  
  emf(paste(finding.to.plot[1],
            finding.to.plot[2],
            " vs ",
            finding.to.plot[3],
            ".emf"))
  
  my.plot <- ggplot(subset(get(
    as.character(finding.to.plot[1])),
    get(independent.variable.to.be.plotted) %in%
      c(as.character(finding.to.plot[2]),
        as.character(finding.to.plot[3]))),
    aes(x = get(independent.variable.to.be.plotted),
        y = get(readout),
        group = get(id),
        color_var = get(id))) +
    geom_path() + labs(title = readout, x = "", y = units.measured, color_var = "")
  
  plot(my.plot)
  
  dev.off()
}

# 12 Old point plots - not with new style yet ####

#This function makes point plots of pairwise comparisons which are specified
#as a "finding". The "finding" has to take the form of:
#   c("some.long.data.frame",
#   "independent.variable.factor.to.be.plotted.at.x = 1",
#   "independent.variable.factor.to.be.plotted.at.x = 2")

fx.point.plots <- function(finding.to.plot,
                           independent.variable.to.be.plotted) {
  
  emf(paste(finding.to.plot[1],
            finding.to.plot[2],
            " vs ",
            finding.to.plot[3],
            ".emf"))
  
  my.plot <- ggplot(subset(get(
    as.character(finding.to.plot[1])),
    get(independent.variable.to.be.plotted) %in%
      c(as.character(finding.to.plot[2]),
        as.character(finding.to.plot[3]))),
    aes(x = get(independent.variable.to.be.plotted),
        y = get(readout),
        group = get(id),
        color = get(id))) +
    geom_point() + labs(title = readout, x = "", y = units.measured, color = "")
  
  plot(my.plot)
  
  dev.off()
}


# 13 Save as html if in the destination folder already ####

save_plot_as_html <- function(plot, file_prefix = "plot object name") {
  if (file_prefix == "plot object name") {
    file_prefix <- deparse(substitute(plot)) }
  htmlwidgets::saveWidget(as_widget(plot), paste0(file_prefix, ".html"))
}