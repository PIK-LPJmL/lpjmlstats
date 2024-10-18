# The following import is to provide visible bindings for dplyr semantics,
# which avoids lintr warnings.
#' @importFrom rlang .data
#' @importFrom rlang :=
#' @importFrom tidyselect matches
#' @importFrom rnaturalearth ne_countries

# ----- table plot -----

create_table_plot <- function(var_grp_list, m_options) {
  table <- var_grp_list_to_table(var_grp_list)

  # convert to character using scientific notation
  table <- table %>%
    dplyr::rowwise() %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric),
                                format, digits = m_options$disp_digits))

  # escape characters
  # uses function from knitr that is not exported, which could be problematic
  # if an error is thrown here the internal function(name) may have changed
  table <- table %>%
    dplyr::mutate_all(asNamespace("knitr")$escape_latex)
  colnames(table) <- asNamespace("knitr")$escape_latex(colnames(table))

  # insert linebreaks
  table <- table %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), insert_linebreaks)) %>%
    dplyr::mutate_all(kableExtra::linebreak)

  return(table)
}

var_grp_list_to_table <- function(var_grp_list) {
  tibble_list <- lapply(var_grp_list, var_grp_to_table)
  dplyr::bind_rows(tibble_list)
}

var_grp_to_table <- function(var_grp) {
  table_list <- var_grp$apply_to_lpjml_calcs(lpjml_calc_to_table)

  # join all tibbles of the list to one tibble
  join <- function(x, y) {
    # define binary join
    dplyr::inner_join(x, y, by = c("variable"), unmatched = "error")
  }
  # successively apply binary join to join all
  table <- Reduce(join, table_list)

  # add unit
  table <- dplyr::mutate(table,
                         unit = prettify_units(var_grp$baseline$meta$unit),
                         .after = tidyselect::matches("variable"))

  return(table)
}

# For this function the lpjml_calc is required to only have a
# single item in space and time dimension.
lpjml_calc_to_table <- function(lpjml_calc) {
  # ---- get band values in list named by variables ----
  band_values <- list()
  for (band_name in dimnames(lpjml_calc$.data_with_unit)[["band"]]) {
    # extract band value
    # !! assumes the lpjml_calc contains only a single value per band !!
    lpjml_calc_sub <- subset(lpjml_calc, band = band_name)
    band_value <- lpjml_calc_sub$.data_with_unit[1, 1, 1]
    band_values[[lpjml_calc_sub$meta$var_and_band_disp]] <- band_value
  }

  # ---- create table ----
  # add variable column
  table <- tibble::tibble(variable = tolower(names(band_values)))
  # add value column
  pos_in_var_grp <- lpjml_calc$meta$pos_in_var_grp
  col_name <- paste0(
    ifelse(!pos_in_var_grp$type == "compare", pos_in_var_grp$type, ""),
    pos_in_var_grp$compare_item,
    ":\n ",
    lpjml_calc$meta$sim_ident
  )
  table <- tibble::add_column(table, !!col_name := unname(unlist(band_values))) # nolint: object_usage_linter

  return(table)
}


# ----- map plot -----

create_map_plots <- function(var_grp_list,
                             m_options,
                             colorbar_length = 1.4) {
  plot_list <- list()

  for (var_grp in var_grp_list) {
    if (m_options$band_subheading)
      plot_list <- add_seperator(plot_list, paste("##", var_grp$get_var_name()))
    limits <- get_type_limits(var_grp, m_options)
    band_names <- var_grp$get_band_names()
    for (band in band_names) {
      if (length(band_names) > 1 && m_options$band_subheading)
        plot_list <- add_seperator(plot_list, paste("###", band))
      var_grp_band <- var_grp$deep_clone()
      var_grp_band$transform_lpjml_calcs(function(x) {
        subset(x, band = band)
      })
      band_plot_list <- var_grp_band$apply_to_lpjml_calcs(lpjml_calc_to_map,
                                                          m_options,
                                                          limits,
                                                          colorbar_length)
      plot_list <- c(plot_list, band_plot_list)
    }
  }
  return(plot_list)
}

# Function to get limits for the different types of data ("under_test", "baseline", "compare")
# in the var_grp, depending
# on the options set in m_options.
get_type_limits <- function(var_grp, m_options) {
  if (!is.null(m_options$sep_cmp_lims) && m_options$sep_cmp_lims) {
    var_grp_cmp <- var_grp$deep_clone()
    var_grp_cmp$under_test <- NULL
    var_grp_cmp$baseline <- NULL
    limits_cmp <- var_grp_cmp$get_limits(quantiles = m_options$quantiles)

    var_grp_non_cmp <- var_grp$deep_clone()
    var_grp_non_cmp$compare <- NULL
    limits_non_cmp <- var_grp_non_cmp$get_limits(quantiles = m_options$quantiles)

    rm(var_grp_cmp, var_grp_non_cmp)
    return(list(baseline = limits_non_cmp, under_test = limits_non_cmp, compare = limits_cmp))
  } else {
    limits_all <- var_grp$get_limits(quantiles = m_options$quantiles)
    return(list(baseline = limits_all, under_test = limits_all, compare = limits_all))
  }
}

# For this function the lpjml_calc is required to have only a single item
# in band and time dimension.
lpjml_calc_to_map <- function(lpjml_calc,
                              m_options,
                              limits,
                              colorbar_length = 1.4) {
  pos_in_var_grp <- lpjml_calc$meta$pos_in_var_grp
  # select the limits that are defined for the type (e.g. "under_test")
  limits_plot <- limits[[pos_in_var_grp$type]]
  plot_title <- paste_custom(
    ifelse(!m_options$var_subheading, lpjml_calc$meta$variable, ""),
    ifelse(!m_options$band_subheading && !is.null(lpjml_calc$meta$band_names),
           lpjml_calc$meta$band_names[[1]], ""),
    lpjml_calc$meta$sim_ident,
    ifelse(!is.null(pos_in_var_grp$compare_item), pos_in_var_grp$compare_item, ""),
    prettify_units(lpjml_calc$meta$unit),
    sep = "; "
  )
  tibble <- lpjml_calc_to_map_tibble(lpjml_calc)
  plot <- map_tibble_to_ggplot(
    tibble,
    plot_title,
    font_size = m_options$font_size,
    n_breaks = m_options$n_breaks,
    limits = limits_plot,
    colorbar_length = colorbar_length
  )
  attr(plot, "listname") <- plot_title
  return(plot)
}

# Function to create raster plots based on lpjml data using ggplot2
# with country borders
map_tibble_to_ggplot <-
  function(tibble,
           title,
           colorbar_length = 1.4,
           font_size = 9,
           n_breaks = 3,
           limits = NULL) {

    # Crop values to limits and drop NA pixels
    if (!is.null(limits)) {
      tibble <- tibble %>%
        dplyr::mutate(value = pmax(pmin(.data$value, limits[2]), limits[1])) %>%
        dplyr::filter(!is.na(.data$value))
    }

    # get range
    x_range <- range(tibble$x, na.rm = TRUE)
    y_range <- range(tibble$y, na.rm = TRUE)

    x_range <- c(x_range[1] - 1, x_range[2] + 1) # add 1 deg of padding
    y_range <- c(y_range[1] - 1, y_range[2] + 1)

    # get world map
    world <- rnaturalearth::ne_countries(returnclass = "sf", scale = "small", type = "countries")

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

    # create basic plot
    p <- ggplot2::ggplot() +
      ggplot2::geom_raster(data = tibble, ggplot2::aes(
        x = .data$x,
        y = .data$y,
        fill = .data$value
      )) +
      colorspace::scale_fill_binned_divergingx(
        palette = "spectral",
        # alternative: "RdBu"
        na.value = NA,
        # make NA values transparent
        rev = TRUE,
        # reverse color scale
        labels = function(x) signif(x, 3),
        # reduce number of digits in labels
        breaks = breaks,
        # use custom breaks
        c2 = 0,
        # make neutral color white
        p1 = 0.6,
        # increase saturation of pos. colors
        limits = limits                     # set limits
      ) +
      ggplot2::geom_sf(data = world, fill = NA, linewidth = 0.12) +
      ggplot2::coord_sf(crs = sf::st_crs(4326))

    # adjust plot
    p <- p +
      # remove padding around plot
      ggplot2::scale_x_continuous(limits = x_range,
                                  expand = c(0, 0),
                                  # position x axis labels on top
                                  position = "top") +
      ggplot2::scale_y_continuous(limits = y_range, expand = c(0, 0)) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        # position legend below plot
        legend.position = "bottom",
        # stretch the legend to width of plot
        legend.key.width = ggplot2::unit(colorbar_length, "cm"),
        legend.key.height = ggplot2::unit(0.2, "cm"),
        aspect.ratio = (y_range[2] - y_range[1]) / (x_range[2] - x_range[1])
      )

    p <- p + benchmark_theme(p, font_size) + ggplot2::ggtitle(title)

    return(p)
  }

lpjml_calc_to_map_tibble <- function(lpjml_calc) {
  lpjml_calc$add_grid()
  grid <- lpjml_calc$grid$subset(cell = dimnames(lpjml_calc)[["cell"]])
  lpjml_calc$.__set_grid__(grid)

  data_ras <- lpjmlkit::as_raster(lpjml_calc)
  tibble <- tibble::tibble(raster::as.data.frame(data_ras, xy = TRUE))

  # use generic name for third column storing the values
  names(tibble)[3] <- "value"

  return(tibble)
}


# ----- time series plot -----

create_time_series_plots <- function(var_grp_list, m_options) {
  plot_list <- list()
  for (var_grp in var_grp_list) {
    if (m_options$var_subheading)
      plot_list <- add_seperator(plot_list, paste("##", var_grp$get_var_name()))
    limits <- var_grp$get_limits("all")
    spatial_units <- dimnames(var_grp$baseline$data)[[1]]
    spatial_dim <- names(dimnames(var_grp$baseline$data)[1])
    band_names <-
      var_grp$apply_to_any_lpjml_calc(function(x) dimnames(x$data)[["band"]])
    for (band in band_names) {
      if (length(band_names) > 1 && m_options$band_subheading)
        plot_list <- add_seperator(plot_list, paste("###", band))

      for (spatial_unit in spatial_units) {
        var_grp_band <- var_grp$deep_clone()
        # subsetting needs to be done with do.call as the spatial dimension is
        # the key that can change
        args <- list(band = band)
        args[[spatial_dim]] <- spatial_unit
        var_grp_band$transform_lpjml_calcs(function(x) {
          do.call("subset", c(list(x = x), args))
        })
        var_name <- var_grp_band$apply_to_any_lpjml_calc(function(x) x$meta$variable)
        plot_title <- paste_custom(
          ifelse(!m_options$var_subheading, var_name, ""),
          ifelse(!m_options$band_subheading && length(band_names) > 1, band, ""),
          ifelse(spatial_unit != "global", spatial_unit, ""),
          prettify_units(var_grp_band$baseline$meta$unit),
          sep = "; "
        )
        tibble_list <-
          var_grp_band$apply_to_lpjml_calcs(lpjml_calc_to_timeseries_tibble)
        time_series_tibble <- dplyr::bind_rows(tibble_list)
        p <- timeseries_tibble_to_ggplot(
          time_series_tibble,
          plot_title,
          limits = limits,
          font_size = m_options$font_size
        )
        plot_list <- c(plot_list, list(plot_title = p))
      }
    }
  }
  return(plot_list)
}


timeseries_tibble_to_ggplot <- function(tibble,
                                        title,
                                        font_size = 9,
                                        limits = NULL) {
  # create lineplot
  p <-
    ggplot2::ggplot(
      tibble,
      ggplot2::aes(
        x = .data$times,
        y = .data$value,
        linetype = .data$type,
        colour = .data$sim_name
      )
    ) +
    ggplot2::geom_line()

  if (!is.null(limits)) {
    p <- p + ggplot2::ylim(limits)
  }

  # adjust plot
  p <- p + ggplot2::labs(x = "time", y = "value") + ggplot2::theme_minimal()
  p <- p + benchmark_theme(p, font_size) + ggplot2::ggtitle(title)

  return(p)
}

# For this function the lpjml_calc is required to only have a single item in
# cell and band dimension.
lpjml_calc_to_timeseries_tibble <- function(lpjml_calc) {
  data <- lpjml_calc$data

  # convert times to standard date format
  if (!is.null(lpjml_calc$meta$time_aggregation))
    times <- as.Date(dimnames(data)[["time"]], format = "%Y")
  else
    times <- as.Date(dimnames(data)[["time"]], format = "%Y-%m-%d")

  # get sim_name
  sim_name <- lpjml_calc$meta$sim_ident

  # combine to tibble
  tibble <- tibble::tibble(
    type = lpjml_calc$meta$pos_in_var_grp$type,
    sim_name = sim_name,
    times = times,
    value = data[1, , 1]
  )

  return(tibble)
}


# ------ utility functions ------

# can be used if text is too long to fit on single line
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
          result <- paste0(result, "\n", word)
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

# This function seperates the plots of the plot_list into meaningful groups (e.g. of different variables)
add_seperator <- function(plot_list, seperator) {
  plot_list <- c(plot_list, list(seperator = "\n\n\n"))
  plot_list <- c(plot_list, list(seperator = seperator))
  plot_list <- c(plot_list, list(seperator = "\n\n\n"))
  return(plot_list)
}

paste_custom <- function(..., sep = "; ") {
  args <- list(...)
  # Filter out NULL values
  args <- args[!vapply(args, function(x) nchar(x) == 0, logical(1))]
  # Concatenate the remaining arguments with the specified separator
  paste(args, collapse = sep)
}

prettify_units <- function(unit_vec) {
  str_units <- as.character(unit_vec)

  # set pattern to empty regex; i.e. regex matches only empty strings
  str_units <- stringr::str_replace_all(str_units,
                                        pattern = "^$", replacement = " - ")

  return(paste0("[", str_units, "]"))
}

benchmark_theme <- function(plot, font_size) {
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),
    axis.title.y = ggplot2::element_blank(),
    text = ggplot2::element_text(size = font_size),
    plot.title = ggplot2::element_text(size = font_size * 1.2),
    plot.margin = ggplot2::unit(c(0.01, 0.01, 0.01, 0.01), "inches")
  )
}
