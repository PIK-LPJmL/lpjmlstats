# The following import is to provide visible bindings for dplyr semantics
#' @importFrom rlang .data


# ----- table plot -----

create_table_plot <- function(var_grp_list,
                              m_options) {
  # ------ prepare table in tidy format, that is rows are observations
  table <- prepare_tibble_for_table(var_grp_list)

  # ------  change the appearance of table to be more readable
  # most importantly transpose the table such that the variables are in the
  # rows to allow for displaying a large amount of variables

  # round all numerics of table
  table <- table %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                round, digits = m_options$decimal_places))

  # merge the columns type and sim_name
  # will be the colnames of the displayed tibble
  # e.g. diff to baseline: sim 1
  type_and_simname <- table %>%
    dplyr::mutate(type_and_simname = paste(.data$type,
                                           .data$sim_name,
                                           sep = ":\n")) %>%
    dplyr::pull(type_and_simname)

  # the type and sim_name columns are not needed anymore for the plot
  # as they are now in type_and_simname
  table <- table %>% dplyr::select(-c(.data$type, .data$sim_name))

  # save the units and varnames of the columns
  # will be lost after transpose
  var_names <- table %>% names()
  col_units <- table %>%
    dplyr::summarise(dplyr::across(dplyr::where(is.numeric),
                                   units::deparse_unit))
  col_units <- prettify_units(unlist(unname(col_units)))


  # transpose table
  # use type_and_simname as new column names
  rownames(table) <- type_and_simname  # will convert to colnames
  table <- tibble::as_tibble(t(table))

  # add var_names and col_units as columns to the table
  table <- table %>%
    dplyr::mutate(variable = var_names,
                  unit = col_units)

  # put variable and unit in the first two columns
  table <- table %>%
    dplyr::select(.data$variable, .data$unit, dplyr::everything())

  # insert linebreaks
  table <- table %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character),
                                insert_linebreaks)) %>%
    dplyr::mutate_all(kableExtra::linebreak)

  # escape all underscores in the table
  # NTODO: Make esacaping characters systematic, i.e. refactor in function
  table <- table %>%
    dplyr::mutate_all(function(x) stringr::str_replace_all(x, "_", "\\\\_")) %>%
    dplyr::mutate_all(function(x) stringr::str_replace_all(x, "\\$", "\\\\$"))
  colnames(table) <- stringr::str_replace_all(colnames(table), "_", "\\\\_")

  return(table)
}

# This function creates a table given a var_grp_list
# that contain space and time aggregated values for different variables,
# e.g. global sums of soil carbon, vegetation carbon etc.
# Each row corresponds to an LPJmL run or a comparison of two LPJmL runs,
# following the tibble logic of having one row per observation of each variable.
# Example of created tibble:
#   type       sim_name  soiln   vegc   ...
#   baseline     sim1    0.1     0.2    ...
#   under_test 1 sim2    0.2     0.3    ...
#   under_test 2 sim3    0.3     0.4    ...
#   difference 1 sim2    0.1     0.1    ...
#   difference 2 sim3    0.2     0.2    ...
# Note that in the report the tibble is displayed in transposed form
# to fit the larger number of variables in the document.
prepare_tibble_for_table <- function(var_grp_list) {
  table <- list()

  # loop through variables of var_grp_list and create a list with tibbles
  # for each variable
  for (var_grp in var_grp_list) {
    tibble_for_var <- prepare_var_grp_tibble(var_grp)
    table[[var_grp$var_name]] <-  tibble_for_var
  }


  # join all tibbles of the list to one tibble by matching the
  # "type" and "sim_name" columns
  join <- function(x, y) {        # define binary join
    dplyr::inner_join(x,
                      y,
                      by = c("type", "sim_name"),
                      unmatched = "error")
  }
  table <- Reduce(join, table)  # successively apply binary join to join all

  return(table)
}


# This function creates a tibble for a var_group,
# containing all values and comparisons for the respective variable.
# We assume that the data is aggregated to a single item
# in both the time and space dimension (e.g. "global", "simulation_period")
# Thus, we only have one value per band for each simulation/
# simulation-comparison.
# The columns of the tibble are:
# - "type" specifying if it is a "baseline", "under_test" or
#   a comparison (e.g. "difference")
# - "sim_name", i.e. the name of the simulation
#   (for the comparison values the name of the under test simulation is used)
# - for each band of a variable a column with the values of that band
# Example:
#   type    sim_name    soiln_layer: 200    soiln: layer 500    ...
#   baseline     sim1    0.1                 0.2    ...
#   under_test 1 sim2    0.2                 0.3    ...
#   under_test 2 sim3    0.3                 0.4    ...
#   difference 1 sim2    0.1                 0.1    ...
#   difference 2 sim3    0.2                 0.2    ...
prepare_var_grp_tibble <- function(var_grp) {
  # Note that a var_grp contains multiple lpjml_calc objects for the different
  # outputs of that variable of different simulations as well as
  # the comparisons of these outputs. Each of these lpjml_calc objects,
  # gets a row for each of its bands in the var_grp_tibble
  # This is done for a single lpjml_calc object
  # by the following fundamental function for generating the table.
  lpjml_calc_to_rows <-
    function(type,       # type of the row, i.e. "baseline", "under_test" ...
             lpjml_calc, # lpjml_calc object containing the data
             var_name) {   # name of the variable (e.g. "soilc"))

      # create list with all band value of simulation output/ comparison
      # prepare list
      band_values <- list()
      # get band names
      band_name_vec <-
        dimnames(lpjml_calc$.data_with_unit)[["band"]]

      # define short band names
      band_name_vec_short <- shorten_names(band_name_vec)

      for (i in seq_along(band_name_vec)) {
        band_name <- band_name_vec[i]
        # extract band value, assuming the lpjml_calc contains only a
        # single value per band
        band_value <- lpjml_calc$.data_with_unit[1, 1, band_name]
        if (band_name == 1) {
          # go here if there is only one band
          # in this case we do not add the band name to the colname
          band_values[[lpjml_calc$meta$variable]] <- band_value
        } else {
          # go here if there are multiple bands
          # in this case we add the band name to the var colname
          new_var_name <-
            paste0(lpjml_calc$meta$variable, "$", band_name_vec_short[i])
          band_values[[new_var_name]] <- band_value
        }
      }

      # create row by adding type and sim_name to band values
      # note that band_values has already defined colnames
      sim_name <- lpjml_calc$get_sim_identifier()
      row <- tibble::as_tibble(c(list(type = type,
                                      sim_name = sim_name),
                                 band_values))

      # bind newly created row to tibble
      return(row)
    }

  # Get variable name of the var_grp
  var_name <- var_grp$var_name

  tibble_for_var <- NULL

  # add baseline value to var_grp_tibble
  baseline_lpjml_calc <- var_grp$baseline
  tibble_for_var <-
    lpjml_calc_to_rows("baseline", baseline_lpjml_calc, var_name)

  # add under test values to var_grp_tibble
  for (under_test_lpjml_calc in var_grp$under_test) {
    tibble_for_var <- dplyr::bind_rows(
      tibble_for_var,
      lpjml_calc_to_rows("under_test", under_test_lpjml_calc, var_name)
    )
  }

  # add compare items to var_grp_tibble
  # typical compare items are "difference", "rel. difference"
  for (i in seq_along(var_grp$compare)) {
    # loop compare items
    named_item <- var_grp$compare[i]
    item_name <- names(named_item)
    compare_item <- named_item[[1]]  # is a list of lpjml_calc objects
    for (compare_lpjml_calc in compare_item) {
      # loop under test runs
      new_row <-
        lpjml_calc_to_rows(item_name, compare_lpjml_calc, var_name)

      # a tibble must have the same unit for all col values
      # the following workaround is used to ensure this,
      # effectively loosing the unit of the compare values
      use_baseline_unit <- function(x) {
        base_unit <- baseline_lpjml_calc$meta$unit
        units::drop_units(x) %>% units::set_units(base_unit)
      }
      new_row <-
        new_row %>% dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                                use_baseline_unit))

      tibble_for_var <- dplyr::bind_rows(tibble_for_var, new_row)
    }
  }

  return(tibble_for_var)
}


# ----- map functions -----
create_map_plots <- function(var_grp_list,
                             m_options,
                             modification_descr) {

  create_single_map <- function(lpjml_calc, var_name, band, band_names, band_names_short,
                                limits = NULL, modification_descr = modification_descr) {
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
      modification_descr,
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
                              limits = limits)
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
      # add comparison plots
      for (compare_item in var_grp$compare) {
        for (lpjml_calc in compare_item) {
          map <- create_single_map(lpjml_calc, var_grp$var_name, band, band_names, band_names_short,
                                   limits = limits)
          plot_list[[map$plot_title]] <- map$plot
        }
      }
      # add under test plots
      for (lpjml_calc in var_grp$under_test) {
        map <- create_single_map(lpjml_calc, var_grp$var_name, band, band_names, band_names_short,
                                 limits = var_grp$get_limits(type = "under_test", 
                                 quantiles = m_options$quantiles), modification_descr = "")
        plot_list[[map$plot_title]] <- map$plot
      }
      # add baseline plot if there is data
      if (!is.null(var_grp$baseline)) {
        lpjml_calc <- var_grp$baseline
        map <- create_single_map(lpjml_calc, var_grp$var_name, band, band_names, band_names_short,
                                 limits = var_grp$get_limits(type = "baseline",
                                 quantiles = m_options$quantiles), modification_descr = "")
        plot_list[[map$plot_title]] <- map$plot
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

    # Create ggplot with color scale
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
        # strech the legend to width of plot
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

# ----- time series functions -----
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
    band_names_short <- shorten_names(band_names, m_options$name_trunc)
    for (band in seq_along(band_names)) {
      if (length(band_names) == 1) {
        plot_title <- var_grp$var_name
      } else {
        plot_title <-
          paste0(var_grp$var_name, "$", band_names_short[band])
      }

      # add unit
      plot_title <- paste0(plot_title, " ",
                           prettify_units(var_grp$baseline$meta$unit))

      tibble <-
        prepare_tibble_for_timeseries(var_grp, band_names[band])

      p <- create_ggplot_timeseries(tibble,
                                    plot_title,
                                    limits = limits,
                                    font_size = m_options$font_size)

      # add plot to list
      plot_list[[plot_title]] <- p

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

  # create plot
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

prepare_tibble_for_timeseries <- function(var_grp, band = 1) {
  create_tibble_lpjml_calc <- function(type, lpjml_calc, band) {
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
      value = data[1, , band]
    )

    return(tibble)
  }

  # create tibble for baseline
  tibble <- create_tibble_lpjml_calc("baseline", var_grp$baseline, band)

  # add tibble for under test
  for (under_test in var_grp$under_test) {
    tibble <-
      dplyr::bind_rows(tibble,
                       create_tibble_lpjml_calc("under_test", under_test, band))
  }

  return(tibble)
}

# ------ utitliy function ------

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
