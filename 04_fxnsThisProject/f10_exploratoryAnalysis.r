# Purpose: My summary
# Author: David Gray Lassiter, PhD
# Date: 2020-Jul-30
# Version: 1.0

# 01 Ensure all pkgs in this script are installed ####
pkgs <-
  c(
    "ggplot2"
    , "plotly"
    , "htmlwidgets"
    , "sqldf"
    , "forcats"
  )

activatePkgs(pkgs)

# 01 This presents customized summary data from a column of a df ####

mySummary <- function(df, columnToSummarize, limit = 3) {

    writeLines(paste0("\n\nCOLUMN SUMMARIZED:\n\n", columnToSummarize, "\n\n"))

    tempDf <- df[, columnToSummarize] %>% as.data.frame()
    colnames(tempDf)[1] <- "value"

    cat("Default summary:
")
    print(summary(df[[columnToSummarize]]))

    cat("Number of unique levels in this factor:
")

    print(df[[columnToSummarize]] %>% unique() %>% length())

    cat("
Sorted by value ascending:
")
    print(sqldf::sqldf(paste0("select value, count(*) as count
                     from tempDf
                     group by value
                     order by value asc
                     limit ", limit)))

    cat("
Sorted by value descending:
")
    print(sqldf::sqldf(paste0("select value, count(*) as count
                     from tempDf
                     group by value
                     order by value desc
                     limit ", limit)))

    cat("
Sorted by count ascending:
")
    print(sqldf::sqldf(paste0("select value, count(*) as count
                     from tempDf
                     group by value
                     order by count asc
                     limit ", limit)))

    cat("
Sorted by count descending:
")
    print(sqldf::sqldf(paste0("select value, count(*) as count
                     from tempDf
                     group by value
                     order by count desc
                     limit ", limit)))
}


# 01 Get plot themes and templates ####

myPlotTheme <-
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

myBasicDensityPlot <- function(df, x, title = "") {
  ggplotly(
    ggplot(df) +
      geom_density(
        mapping = aes(
          x = df[[x]],
          text = paste0("\ndf[[x]] is ", x, "."))) +
      xlab(x) +
      myPlotTheme +
      scale_x_continuous(labels = scales::scientific) +
      scale_y_continuous(labels = scales::scientific) +
      ggtitle(title)
  )
}

# 03 Layered density plot ####

myLayeredDensityPlot <- function(df, x, layers, title = "") {
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
      myPlotTheme +
      scale_x_continuous(labels = scales::scientific) +
      scale_y_continuous(labels = scales::scientific) +
      ggtitle(title),
    tooltip = c("x", "fill", "text")
    )
  ) %>% layout(legend = list(x = 100, y = 0.5))
}

# 04 Violin plot ####

myViolinPlot <- function(df, x, y, title = "") {
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
      myPlotTheme +
      scale_y_continuous(labels = scales::scientific) +
      ggtitle(title),
    tooltip = c("x", "y", "text")
    )
  ) %>% layout(legend = list(x = 100, y = 0.5))
}

# 05 Stripchart ####

myStripchartPlot <- function(df, x, y, title = "") {
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
      myPlotTheme +
      scale_y_continuous(labels = scales::scientific) +
      ggtitle(title),
    tooltip = c("x", "y", "text")
    )
  ) %>% layout(legend = list(x = 100, y = 0.5))
}

# 06 Conditional density ####

myConditionalDensityPlot <-
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
        myPlotTheme +
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

myScatterPlot <- function(df, x, y, title = "") {

  lmFit <- lm(df[[y]] ~ df[[x]], data = df)
  df[["predictedY"]] <- predict(lmFit, df)

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
                    y = df[["predictedY"]]),
                color = "black",
                linetype = "dashed"
                ) +
      xlab(x) +
      ylab(y) +
      myPlotTheme +
      theme(legend.position = "none") +
      scale_x_continuous(labels = scales::scientific) +
      scale_y_continuous(labels = scales::scientific) +
      ggtitle(title),
    tooltip = c("x", "y", "text")
    )
  ) %>% layout(legend = list(x = 100, y = 0.5))
}

# 08 Heatmap ####

myHeatmap <- function(df, x, y, color, aggfxn = "median", title = "") {

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
      myPlotTheme +
      ggtitle(title),
    tooltip = c("x", "y", "fill", "text")
  ) %>% layout(legend = list(x = 100, y = 0.5))
}

# 09 Calendar heatmap ####

myFiveVarHeatmap <-
    function(
        df
        , xVar
        , yVar
        , xFacetVar
        , yFacetVar
        , colorVar
        , aggfxn = "median"
        , title = ""
        ) {

    df %<>% select(xVar, yVar, xFacetVar, yFacetVar, colorVar)

    df[[xVar]] %<>% as.factor
    df[[yVar]] %<>% as.factor %>% forcats::fct_rev(.)
    df[[xFacetVar]] %<>% as.factor
    df[[yFacetVar]] %<>% as.factor

    df <-
        sqldf::sqldf(
        paste0(
            "select "
            , xVar
            , ", "
            , yVar
            , ", "
            , xFacetVar
            , ", "
            , yFacetVar
            , ", "
            , aggfxn
            , "("
            , colorVar
            , ")
            from df
            group by 1, 2, 3, 4"))

    ggplotly(
        ggplot(df) +
        geom_tile(
            aes(
            x = df[[xVar]],
            y = df[[yVar]],
            fill = df[[paste0(aggfxn, "(", colorVar, ")")]],
            text =
                paste0(
                    "\nas.factor(df[[xVar]]) is "
                    , xVar
                    , ".\ndf[[yVar]]) is "
                    , yVar
                    , ".\ndf[[xFacetVar]]) is "
                    , xFacetVar
                    , ".\ndf[[yFacetVar]]) is "
                    , yFacetVar
                    , "\ndf[[colorVar]] is "
                    , aggfxn
                    , " "
                    , colorVar
                    , "."
                    )
            )
        ) +
        facet_grid(df[[yFacetVar]] ~ df[[xFacetVar]]) +
        scale_fill_gradient2(
            midpoint = median(df[[paste0(aggfxn, "(", colorVar, ")")]]),
                            low = "blue", mid = "white",
                            high = "red", space = "Lab") +
        xlab(xVar) +
        ylab(yVar) +
        labs(fill = paste0(aggfxn, "(", colorVar, ")")) +
        myPlotTheme +
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

myParallelPlot <- function(yVar,
                              xVar) {

  emf(paste(yVar[1],
            yVar[2],
            " vs ",
            yVar[3],
            ".emf"))

  myPlot <- ggplot(subset(get(
    as.character(yVar[1])),
    get(xVar) %in%
      c(as.character(yVar[2]),
        as.character(yVar[3]))),
    aes(x = get(xVar),
        y = get(readout),
        group = get(id),
        colorVar = get(id))) +
    geom_path() + labs(title = readout, x = "", y = units.measured, colorVar = "")

  plot(myPlot)

  dev.off()
}

# 12 Old point plots - not with new style yet ####

#This function makes point plots of pairwise comparisons which are specified
#as a "finding". The "finding" has to take the form of:
#   c("some.long.data.frame",
#   "independent.variable.factor.to.be.plotted.at.x = 1",
#   "independent.variable.factor.to.be.plotted.at.x = 2")

myPointPlot <- function(yVar,
                           xVar) {

  emf(paste(yVar[1],
            yVar[2],
            " vs ",
            yVar[3],
            ".emf"))

  myPlot <- ggplot(subset(get(
    as.character(yVar[1])),
    get(xVar) %in%
      c(as.character(yVar[2]),
        as.character(yVar[3]))),
    aes(x = get(xVar),
        y = get(readout),
        group = get(id),
        color = get(id))) +
    geom_point() + labs(title = readout, x = "", y = units.measured, color = "")

  plot(myPlot)

  dev.off()
}


# 13 Save as html if in the destination folder already ####

savePlotAsHtml <- function(plot, filePrefix = "plot object name") {
  if (filePrefix == "plot object name") {
    filePrefix <- deparse(substitute(plot)) }
  htmlwidgets::saveWidget(as_widget(plot), paste0(filePrefix, ".html"))
}
