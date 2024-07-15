#' @import patchwork
# the import for the complete patchwork package is needed
# to make the `+` operator work in all cases

arrange_table_plot <- function(plotlist, m_options) {

  print(
    knitr::kable(
      plotlist,
      "latex",
      escape = FALSE,
      booktabs = TRUE,
      longtable = TRUE,
      align = "l",
      col.names = kableExtra::linebreak(names(plotlist), align = "l")
    ) %>%
      kableExtra::kable_styling(
        font_size = m_options$font_size,
        latex_options = c("repeat_header", "HOLD_position"),
      )
  )
}

arrange_map_plots <- function(plotlist, m_options) {
  plotlist <- print_highlighted_maps(plotlist, m_options$highlight)

  n_plots <- length(plotlist)

  if (n_plots == 0) {
    return()
  }

  arrange_plots(plotlist, m_options$num_cols, wrap = TRUE)
}

arrange_plots <- function(plotlist, num_cols, wrap) {
  print_plotrow <- function(plotrow) {
    if (!is.null(plotrow))
      print(plotrow + patchwork::plot_layout(nrow = 1, widths = rep(1, num_cols)))
  }
  # wrapping each plot means that patchwork does not
  # try to align plots which is sometimes more robust
  wrap_elements_wrapper <- function(plot) {
    if (wrap)
      return(patchwork::wrap_elements(plot))
    else
      return(plot)
  }
  i <- 1
  n_plots <- length(plotlist)

  # put plots in rows until all are accomodated
  while (i <= n_plots) {
    plotrow <- NULL
    j <- 1
    # create plotrow
    while (j <= num_cols) {
      if (!is.character(plotlist[[i]])) {
        # go here if entry is actual plot
        if (is.null(plotrow))
          plotrow <- wrap_elements_wrapper(plotlist[[i]])
        else
          plotrow <- plotrow + wrap_elements_wrapper(plotlist[[i]])
        j <- j + 1
      } else {
        # go here if entry a "control signal" e.g. newline
        # print (prossibly unfinished) row, print the control signal
        # and start a new row
        print_plotrow(plotrow)
        plotrow <- NULL
        cat(plotlist[[i]])
        j <- 1
      }
      i <- i + 1
      if (i > n_plots) break
    }
    print_plotrow(plotrow)
    plotrow <- NULL
  }
}

print_highlighted_maps <- function(plotlist, highlight) {
  highlight_plots <- rep(FALSE, length(plotlist))
  # select all plots with names that contain one of the highlight strings
  for (plot_name in highlight) {
    # all plot names containing the plot_name string are set to true
    plots_to_add <- stringr::str_detect(names(plotlist), plot_name)
    highlight_plots <- highlight_plots | plots_to_add
  }

  # print the highlighted plots
  for (plot in which(highlight_plots)) {
    plotlist[[plot]] <- plotlist[[plot]] + ggplot2::theme(
      legend.position = "right",
      legend.key.width = ggplot2::unit(0.4, "cm"),
      legend.key.height = ggplot2::unit(0.8, "cm")
    )
    print(plotlist[[plot]])
    cat("\\newline")
  }
  plotlist <- plotlist[!highlight_plots]
  return(plotlist)
}

arrange_timeseries_plots <- function(plotlist, m_options) {
  n_plots <- length(plotlist)
  # get first real plot (not a control signal)
  j <- 1
  while (is.character(plotlist[[j]]) && j < 10^6)
    j <- j + 1
  g <- ggplot2::ggplotGrob(plotlist[[j]])
  legend <- g$grobs[[which(vapply(g$grobs, function(x) x$name, character(1)) == "guide-box")]]
  for (i in 1:(n_plots)) {
    # remove legend
    if (!is.character(plotlist[[i]]))
      plotlist[[i]] <-
        plotlist[[i]] + ggplot2::theme(legend.position = "none")
  }

  grid::grid.newpage()
  grid::grid.draw(legend)
  cat("\n\n")
  arrange_plots(plotlist, m_options$num_cols, wrap = FALSE)
}
