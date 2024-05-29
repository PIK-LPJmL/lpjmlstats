# The following import is to provide visible bindings for dplyr semantics,
# which avoids lintr warnings.
#' @importFrom rlang .data


# ----- table plot -----

create_table_plot <- function(var_grp_list,
                              m_options) {

  table <- var_grp_list_to_table(var_grp_list)

  # convert to character using scientific notation
  table <- table %>% dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                format, digits = m_options$decimal_places))

  # insert linebreaks
  table <- table %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character),
                                insert_linebreaks)) %>%
    dplyr::mutate_all(kableExtra::linebreak)

  # escape characters
  # uses function from knitr that is not exported, which could be problematic
  # if an error is thrown here the internal function(name) may have changed
  table <- table %>%
    dplyr::mutate_all(knitr:::escape_latex_table)
  colnames(table) <- knitr:::escape_latex_table(colnames(table))

  return(table)
}

var_grp_list_to_table <- function(var_grp_list) {
  tibble_list <- lapply(var_grp_list, var_grp_to_table)
  dplyr::bind_rows(tibble_list)
}

var_grp_to_table <- function(var_grp) {
  table_list <- var_grp_apply(var_grp, lpjml_calc_to_table)

  # join all tibbles of the list to one tibble
  join <- function(x, y) {        # define binary join
    dplyr::inner_join(x, y, by = c("variable"), unmatched = "error")
  }
  table <- Reduce(join, table_list)  # successively apply binary join to join all

  # add unit
  table <- dplyr::mutate(table,
                         unit = prettify_units(var_grp$baseline$meta$unit),
                         .after = variable)

  return(table)
}

lpjml_calc_to_table <- function(lpjml_calc, type, compare_item = NULL) {

    # get band names and short band names
    band_name_vec <-
      dimnames(lpjml_calc$.data_with_unit)[["band"]]
    band_name_vec_short <- shorten_names(band_name_vec)

    # get band values in list
    band_values <- list()
    for (i in seq_along(band_name_vec)) {
      band_name <- band_name_vec[i]
      # extract band value
      # !! assumes the lpjml_calc contains only a single value per band !!
      band_value <- lpjml_calc$.data_with_unit[1, 1, band_name]
      row_name <- paste0(lpjml_calc$meta$variable,
                         ifelse(length(band_name_vec) == 1, "",
                                paste0("$", band_name_vec_short[i])))
      band_values[[row_name]] <- band_value
    }

    # create table
    table <- tibble::tibble(variable = names(band_values))
    # add values column to table
    col_name <- paste0(type, ifelse(!is.null(compare_item), "_", ""),
                          compare_item, ":\n ", lpjml_calc$get_sim_identifier())
    table <- tibble::add_column(table, !!col_name := unname(unlist(band_values)))

    return(table)
}



# ----- map plot -----
create_map_plots <- function(var_grp_list,
                             colorbar_length = 1.4,
                             m_options) {

  create_single_map <- function(lpjml_calc,
                                var_name,
                                band,
                                band_names,
                                band_names_short,
                                compare_item) {
    compare_band <- subset(lpjml_calc, band = band_names[band])
    if (length(band_names) > 1) {
      new_var_name <-
        paste0(var_name, "$", band_names_short[band])
    } else {
      new_var_name <- paste0(var_name)
    }
    plot_title <- paste0(
      lpjml_calc$get_sim_identifier(),
      " ",
      compare_item,
      "; ",
      new_var_name,
      " ",
      prettify_units(lpjml_calc$meta$unit)
    )
    tibble <- prepare_tibble_for_map(compare_band)
    plot <- create_ggplot_map(tibble,
                              plot_title,
                              font_size = m_options$font_size,
                              n_breaks = m_options$n_breaks,
                              limits = limits,
                              colorbar_length = colorbar_length)
    return(list(plot = plot, plot_title = plot_title))
  }
  # prepare empty list of all plots
  plot_list <- list()

  # loop over all eval groups
  for (var_grp in var_grp_list) {
    limits <- var_grp$get_limits(quantiles = m_options$quantiles)

    # use the first comparison object to get the band names
    band_names <- var_grp$get_band_names()
    band_names_short <- shorten_names(band_names, m_options$name_trunc)

    for (band in seq_along(band_names)) {
      # add baseline plot if there is data
      if (!is.null(var_grp$baseline)) {
        lpjml_calc <- var_grp$baseline
        map <- create_single_map(lpjml_calc,
                                 var_grp$var_name,
                                 band,
                                 band_names,
                                 band_names_short,
                                 "")
        plot_list[[map$plot_title]] <- map$plot
      }

      # add under test plots
      for (lpjml_calc in var_grp$under_test) {
        map <- create_single_map(lpjml_calc,
                                 var_grp$var_name,
                                 band,
                                 band_names,
                                 band_names_short,
                                 "")
        plot_list[[map$plot_title]] <- map$plot
      }

      # add comparison plots
      for (compare_item in names(var_grp$compare)) {
        for (lpjml_calc in var_grp$compare[[compare_item]]) {
          map <- create_single_map(lpjml_calc,
                                   var_grp$var_name,
                                   band,
                                   band_names,
                                   band_names_short,
                                   compare_item)
          plot_list[[map$plot_title]] <- map$plot
        }
      }
    }
  }
  return(plot_list)
}

# Function to create raster plots based on lpjml data using ggplot2
# with country borders
create_ggplot_map <-
  function(tibble,
           title,
           colorbar_length = 1.4,
           font_size = 9,
           n_breaks = n_breaks,
           limits = NULL) {
    # get world map
    world <- ggplot2::map_data("world")

    # get lat lon range of data
    x_range <- range(tibble$x, na.rm = TRUE)
    y_range <- range(tibble$y, na.rm = TRUE)

    # crop world map to data range
    world <- world %>%
      dplyr::filter(.data$long >= x_range[1] & .data$long <= x_range[2]) %>%
      dplyr::filter(.data$lat >= y_range[1] & .data$lat <= y_range[2])


    # setup breaks
    breaks <- function(limits) {
      if (limits[1] != limits[2])
        breaks <-
          ((-n_breaks - 0.5):(n_breaks + 0.5)) / (n_breaks + 0.5) *
          max(abs(limits))
      else
        breaks <- c(limits[1] - 1, limits[1] + 1)
      return(breaks)
    }

    # create ggplot with color scale
    p <- ggplot2::ggplot() +
      ggplot2::geom_tile(data = tibble,
                         ggplot2::aes(
                           x = .data$x,
                           y = .data$y,
                           fill = .data$value
                         )) +
      colorspace::scale_fill_binned_divergingx(
        palette = "spectral",               # alternative: "RdBu"
        na.value = NA,                      # make NA values transparent
        rev = TRUE,                         # reverse color scale
        labels = function(x) signif(x, 3),  # reduce number of digits in labels
        breaks = breaks,                    # use custom breaks
        c2 = 0,                             # make neutral color white
        p1 = 0.6,                           # increase saturation of pos. colors
        limits = limits                     # set limits
      )  +
      ggplot2::labs(title = title)


    # remove unnecessary elements from plot
    p <- p + ggplot2::coord_fixed(xlim = x_range, ylim = y_range) +
      # remove padding around plot
      ggplot2::scale_x_continuous(limits = x_range,
                                  expand = c(0, 0),
                                  # position x axis labels on top
                                  position = "top") +
      ggplot2::scale_y_continuous(limits = y_range, expand = c(0, 0)) +
      # make background white
      ggplot2::theme_minimal() +
      ggplot2::theme(
        # remove axis text
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank(),
        # remove text above legend
        legend.title = ggplot2::element_blank(),
        # position legend below plot
        legend.position = "bottom",
        # stretch the legend to width of plot
        legend.key.width = ggplot2::unit(colorbar_length, "cm"),
        legend.key.height = ggplot2::unit(0.2, "cm"),
        text = ggplot2::element_text(size = font_size)
      )


    # add country border overlay
    p <- p + ggplot2::geom_polygon(
      data = world,
      ggplot2::aes(x = .data$long, y = .data$lat, group = .data$group),
      # reduce line thickness
      size = 0.12,
      fill = NA,
      # transparent filling
      color = "darkgray"  # color of border
    )


    return(p)
  }

prepare_tibble_for_map <- function(lpjml_calc) {
  # Convert to raster
  data_ras <- lpjmlkit::as_raster(lpjml_calc)

  # Prepare data for ggplot
  tibble <-
    tibble::tibble(raster::as.data.frame(data_ras, xy = TRUE))

  oldname <- lpjml_calc$meta$variable

  tibble <-
    tibble %>%  dplyr::rename_with(~ "value", dplyr::all_of(oldname))

  return(tibble)
}

# ----- time series plot -----
create_time_series_plots <- function(var_grp_list,
                                     m_options) {

  plot_list <- list()

  for (var_grp in var_grp_list) {
    limits_ut <- var_grp$get_limits("under_test")
    limits_baseline <- var_grp$get_limits("baseline")
    limits <- c(min(limits_ut[1], limits_baseline[1]),
                max(limits_ut[2], limits_baseline[2]))

    # get band names of variable
    # take the bandnames of the first baseline object, assuming the bands
    # of all compare objects are the same
    band_names <- dimnames(var_grp$baseline$data)[["band"]]
    cells <- dimnames(var_grp$baseline$data)[[1]]
    band_names_short <- shorten_names(band_names, m_options$name_trunc)
    for (band in seq_along(band_names)) {
      for (cell in cells) {
        if (length(band_names) == 1) {
          plot_title <- var_grp$var_name
        } else {
          plot_title <-
            paste0(var_grp$var_name, "$", band_names_short[band])
        }
        if (length(cells) > 1) {
          plot_title <- paste0(plot_title, " ", cell)
        }

        # add unit
        plot_title <- paste0(plot_title, " ",
                             prettify_units(var_grp$baseline$meta$unit))

        tibble <-
          prepare_tibble_for_timeseries(var_grp, band_names[band], cell)

        p <- create_ggplot_timeseries(tibble,
                                      plot_title,
                                      limits = limits,
                                      font_size = m_options$font_size)

        # add plot to list
        plot_list[[plot_title]] <- p
      }

    }
  }
  return(plot_list)
}


create_ggplot_timeseries <- function(tibble,
                                     title,
                                     font_size = 9,
                                     limits = NULL) {

  # create lineplot
  p <-
    ggplot2::ggplot(tibble,
                    ggplot2::aes(
                      x = .data$times,
                      y = .data$value,
                      linetype = .data$type,
                      colour = .data$sim_name
                    )) +
    ggplot2::geom_line()

  if (!is.null(limits)) {
    p <- p + ggplot2::ylim(limits)
  }

  # adjust plot
  p <- p + ggplot2::labs(x = "time", y = "value") +
    ggplot2::ggtitle(title) +
    ggplot2::theme_minimal() +
    # remove legend title and axis titles
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      text = ggplot2::element_text(size = font_size)
    )

  return(p)

}

prepare_tibble_for_timeseries <- function(var_grp, band = 1, cell = 1) {
  create_tibble_lpjml_calc <- function(type, lpjml_calc, band, cell) {
    # get data from lpjml_calc
    data <- lpjml_calc$data

    # convert times to standard date format
    if (!is.null(var_grp$baseline$meta$time_aggregation))
      times <- as.Date(dimnames(data)[["time"]], format = "%Y")
    else
      times <- as.Date(dimnames(data)[["time"]], format = "%Y-%m-%d")

    # get sim_name
    sim_name <- lpjml_calc$get_sim_identifier()

    # combine to tibble
    tibble <- tibble::tibble(
      type = type,
      sim_name = sim_name,
      times = times,
      value = data[cell, , band]
    )

    return(tibble)
  }

  # create tibble for baseline
  tibble <- create_tibble_lpjml_calc("baseline", var_grp$baseline, band, cell)

  # add tibble for under test
  for (under_test in var_grp$under_test) {
    tibble <-
      dplyr::bind_rows(tibble,
                       create_tibble_lpjml_calc("under_test",
                                                under_test,
                                                band,
                                                cell))
  }

  return(tibble)
}

# ------ utility functions ------

# function to insert linebreaks if the text is too long
insert_linebreaks <- function(text_vect, max_length = 18) {
  for (k in seq_along(text_vect)) {

    text <- text_vect[k]
    words <- stringr::str_split(text, " ")[[1]]
    result <- words[1]
    num <- nchar(result)
    if (length(words) > 1) {
      for (i in 2:length(words)) {
        word <- words[i]
        if (num + nchar(word) > max_length) {
          result <- paste0(result, "\\\\", word)
          num <- nchar(word)
        } else {
          result <- paste0(result, " ", word)
          num <- num + nchar(word)
        }
      }
    }
    text_vect[k] <- result

  }
  return(text_vect)
}


# NTODO: needs refactoring
shorten_names <- function(names, trunc = 9) {

  # find index until which all strings are equal
  stop <- FALSE
  i <- 0
  while (stop == FALSE) {
    i <- i + 1
    if (length(unique(substr(
      x = names,
      start = 1,
      stop = i
    ))) > 1) {

      stop <- TRUE
    }

    if (i > max(stringr::str_length(names))) {
      stop <- TRUE
    }
  }
  i <- i - 1

  if (i > trunc + 9) {
    front_parts <- names %>% stringr::str_sub(1, 4)
    back_parts <- names %>% stringr::str_sub(max(i - 3, 6))
    short_colnames <- paste0(front_parts, "[..]", back_parts)
  } else {
    short_colnames <- names
  }

  # find index from which all remaining truncated strings are unique
  stop <- FALSE
  i <- 0
  while (stop == FALSE) {
    i <- i + 1
    if (length(unique(substr(
      x = short_colnames,
      start = 1,
      stop = i
    ))) == length(unique(short_colnames
    ))) {

      stop <- TRUE
    }
  }

  trunc <- max(i + 6, trunc + 6)

  short_colnames <- stringr::str_trunc(short_colnames, trunc, ellipsis = "[..]")

  return(short_colnames)
}

prettify_units <- function(unit_vec) {
  str_units <- as.character(unit_vec)

  # set pattern to empty regex
  str_units <- stringr::str_replace_all(str_units,
                                        # regex that matches only empty strings
                                        pattern = "^$",
                                        replacement = " - ")

  return(paste0("[", str_units, "]"))
}

var_grp_apply <- function(var_grp, fun) {
  list <- list()
  if (!is.null(var_grp$baseline)) {
    list <- append(list, list(fun(var_grp$baseline, "baseline")))
  }
  if (!is.null(var_grp$under_test)) {
    for (lpjml_calc in var_grp$under_test) {
      list <- append(list, list(fun(lpjml_calc, "under_test")))
    }
  }
  if (!is.null(var_grp$compare)) {
    for (item in seq_along(var_grp$compare)) {
      item_name <- names(var_grp$compare[item])
      for (lpjml_calc in var_grp$compare[[item]])
        list <- append(list, list(
          fun(lpjml_calc, "compare", compare_item = item_name)
        ))
    }
  }
  return(list)
}
