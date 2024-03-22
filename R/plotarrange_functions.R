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
  add_highlighted_maps(plotlist, m_options$highlight)

  n_plots <- length(plotlist)

  if (n_plots == 0) {
    return()
  }
  # modify all plots
  for (i in 1:n_plots) {
    plotlist[[i]] <-
      plotlist[[i]] + ggplot2::theme(
        legend.text = ggplot2::element_text(size = 7),
        plot.margin = ggplot2::unit(c(0.01, 0.01, 0.01, 0.01), "inches")
      )
  }

  # remove axis text for all but the first plot
  if (n_plots > 1) {
    for (i in 2:(n_plots)) {
      plotlist[[i]] <-
        plotlist[[i]] + ggplot2::theme(
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank()
        )
    }
  }

  # arrange plots
  for (i in seq(1, n_plots, 2)) {
    if (i < n_plots) {
      print(plotlist[[i]] +  plotlist[[i + 1]] +
              patchwork::plot_layout(ncol = 2, widths = c(0.5, 0.5)))
      cat("\\newline")
    } else {
      print(plotlist[[i]] + patchwork::plot_layout(ncol = 2))
      cat("\\newline")
    }
  }
}

add_highlighted_maps <- function(plotlist, highlight) {
  highlight_plots <- rep(FALSE, length(plotlist))
  for (plot_name in highlight) {
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
    plotlist[[plot]] <- NULL
  }
}

arrange_timeseries_plots <- function(plotlist) {
  n_plots <- length(plotlist)

  patch <- patchwork::guide_area() +
    plotlist[[1]] +
    ggplot2::scale_x_date(position = "bottom") +
    patchwork::plot_layout(guides = "collect",
                           ncol = 2)

  print(patch)
  cat("\\newline")
  if (n_plots > 1) {
    for (i in 2:(n_plots)) {
      # remove legend
      plotlist[[i]] <-
        plotlist[[i]] + ggplot2::theme(legend.position = "none")
    }

    for (i in seq(2, n_plots, 2)) {
      if (i < n_plots) {
        print(plotlist[[i]] +
                plotlist[[i + 1]] +
                patchwork::plot_layout(ncol = 2, width = c(0.5, 0.5)))
        cat("\\newline")
      } else {
        print(plotlist[[i]] + patchwork::plot_layout(ncol = 2))
        cat("\\newline")
      }
    }
  }
}
