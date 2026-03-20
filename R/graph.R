dmy_style <- lubridate::stamp('1 March 2021', orders = '%0d %B %Y')

#' Graph temperature and RH for single or multiple stores
#'
#' @param envdata A dataframe returned from parse_datalogger.
#' @param store A string matching a single store or a group of stores with a matching pattern.
#' @param exclude_stores A string or vector to exclude a group of stores.
#' @param graph_title Title of graph, else 'All stores at site' or first location if `store` specified.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param date_format Format of dates on x axis, like '%d/%m'.
#' @param breaks Breaks on x axis, like '2 weeks'.
#' @param standard Standard to use for temp/RH guidelines.
#' @param min_temp Minimum temperature guideline.
#' @param max_temp Maximum temperature guideline.
#' @param min_RH Minimum RH guideline.
#' @param max_RH Maximum RH guideline.
#' @param max_axis_temp Maximum of temperature axis.
#' @param max_axis_RH Maximum of RH axis.
#' @param col_temp Colour for temperature line, as named HTML colour or hex.
#' @param col_RH Colour for RH line, as named HTML colour or hex.
#'
#' @returns ggplot2 Object containing temperature and RH graph.
#'
#' @examples graph_store(envdata_exhib, graph_title = 'Exhibition cases')
graph_store <- function(envdata,
                        store = FALSE,
                        exclude_stores = FALSE,
                        graph_title = FALSE,
                        start_date = FALSE,
                        end_date = FALSE,
                        date_format = '%m/%Y',
                        breaks = '2 months',
                        standard = 'BS 4971',
                        min_temp = FALSE,
                        max_temp = FALSE,
                        min_RH = FALSE,
                        max_RH = FALSE,
                        max_axis_temp = 40,
                        max_axis_RH = 100,
                        col_temp = 'red',
                        col_RH = 'blue') {
  message('Graphing T&RH')
  subset <- subset_readings(
    envdata,
    store = store,
    exclude_stores = exclude_stores,
    start_date = start_date,
    end_date = end_date
  )

  start_date <- min(subset$datetime)
  end_date <- max(subset$datetime)
  minmax <- set_minmax(standard, min_temp, max_temp, min_RH, max_RH)
  trh_ratio <- (max_axis_RH / max_axis_temp)

  # store is identifying part of location, does not have to match whole string
  if (store != FALSE) {
    message('Graphing single store')
    line_alpha <- 1
    if (graph_title == FALSE) {
      graph_title <- subset$location[1]
    }
  }

  if (store == FALSE) {
    message('Graphing all stores')
    line_alpha <- 0.5
    if (graph_title == FALSE) {
      graph_title <- paste('All stores at', subset$site[1])
    }
  }


  graph_subtitle <- paste(stringr::str_remove(dmy_style(min(subset$datetime)), '^0'),
                          'to',
                          stringr::str_remove(dmy_style(max(subset$datetime)), '^0'))

  # Create graph
  # with time on x axis, temperature on left y axis, and RH on right y axis
  # Y scales 0-40º and 0-100% by default
  # Dotted lines indicate BS4971 storage guidelines
  return(
    subset |> ggplot2::ggplot(mapping = aes(x = datetime, group = location)) +
      geom_hline(
        yintercept = minmax[1],
        color = col_temp,
        linetype = 'dotted',
        alpha = 0.8
      ) +
      geom_hline(
        yintercept = minmax[2],
        color = col_temp,
        linetype = 'dotted',
        alpha = 0.8
      ) +
      geom_hline(
        yintercept = minmax[3] / 2.5,
        color = col_RH,
        linetype = 'dotted',
        alpha = 0.6
      ) +
      geom_hline(
        yintercept = minmax[4] / 2.5,
        color = col_RH,
        linetype = 'dotted',
        alpha = 0.6
      ) +
      geom_line(
        aes(y = temp),
        color = col_temp,
        size = 0.25,
        #linetype = linetype,
        alpha = line_alpha
      ) +
      geom_line(
        aes(y = RH / trh_ratio),
        color = col_RH,
        size = 0.25,
        #linetype = linetype,
        alpha = line_alpha
      ) +
      scale_x_datetime(
        name = 'Date',
        date_breaks = breaks,
        date_labels = date_format
      ) +
      labs(title = graph_title, subtitle = graph_subtitle) +
      scale_y_continuous(
        name = 'Temperature (º C, red)',
        limits = c(0, max_axis_temp),
        sec.axis = sec_axis(
          ~ . * trh_ratio,
          name = 'Rel. Humidity (%, blue)',
          breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
        )
      ) +
      theme(
        axis.title.y.left = element_text(color = col_temp),
        axis.title.y.right = element_text(color = col_RH)
      )
  )
}

# Graph max/min/mean summary ----
#' Graph temperature and RH summary for single or multiple stores
#'
#' @param envdata A dataframe returned from parse_datalogger.
#' @param type 'annual', 'monthly', or 'daily'
#' @param store A string matching a single store or a group of stores with a matching pattern.
#' @param exclude_stores A string or vector to exclude a group of stores.
#' @param graph_title Title of graph, else 'All stores at site' or first location if `store` specified.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param date_format Format of dates on x axis, like '%d/%m'.
#' @param breaks Breaks on x axis, like '2 weeks'.
#' @param standard Standard to use for temp/RH guidelines.
#' @param min_temp Minimum temperature guideline.
#' @param max_temp Maximum temperature guideline.
#' @param min_RH Minimum RH guideline.
#' @param max_RH Maximum RH guideline.
#' @param max_axis_temp Maximum of temperature axis.
#' @param max_axis_RH Maximum of RH axis.
#' @param col_temp Colour for temperature line, as named HTML colour or hex.
#' @param col_RH Colour for RH line, as named HTML colour or hex.
#'
#' @returns ggplot2 Object containing temperature and RH graph with mean line and min/max ribbons
#'
#' @examples graph_summary(envdata_exhib, graph_title = 'Exhibition cases', type = 'daily')
graph_summary <- function(envdata,
                          graph_title = FALSE,
                          type = 'monthly',
                          store = FALSE,
                          exclude_stores = FALSE,
                          start_date = FALSE,
                          end_date = FALSE,
                          date_format = '%m/%y',
                          breaks = '2 months',
                          standard = 'BS 4971',
                          min_temp = FALSE,
                          max_temp = FALSE,
                          min_RH = FALSE,
                          max_RH = FALSE,
                          max_axis_temp = 40,
                          max_axis_RH = 100,
                          col_temp = 'red',
                          col_RH = 'blue') {
  message('Graphing max/min/mean')

  trh_ratio <- (max_axis_RH / max_axis_temp)
  # Set min/max by standard where not specified
  minmax <- set_minmax(standard, min_temp, max_temp, min_RH, max_RH)

  # Filter readings to given stores and timeframe
  subset <- subset_readings(
    envdata,
    store = store,
    exclude_stores = exclude_stores,
    start_date = start_date,
    end_date = end_date
  )
  site_summary <- summarise_site(subset, type = type)


  # Add datetime column for graphing
  if (type == 'daily') {
    site_summary <-  dplyr::mutate(site_summary, datetime = as.POSIXct(paste0(year, '-', month, '-', day), format = '%Y-%m-%d'))
  }
  if (type == 'monthly') {
    site_summary <-  dplyr::mutate(site_summary, datetime = as.POSIXct(paste0(year, '-', month, '-01'), format = '%Y-%m-%d'))
  }
  if (type == 'annual') {
    site_summary <-  dplyr::mutate(site_summary, datetime = as.POSIXct(paste0(year, '-01-01'), format = '%Y-%m-%d'))
  }
  # Graph single store
  # store is identifying part of location, does not have to match whole string
  if (store != FALSE) {
    message('Graphing single store')
    line_alpha <- 1
    fill_alpha <- 0.2
    if (!graph_title) {
      graph_title <- subset$location[1]
    }
  }

  # Graph all stores
  if (store == FALSE) {
    message('Graphing all stores')
    line_alpha <- 0.7
    fill_alpha <- 0.05
    if (!graph_title) {
      graph_title <- paste('All stores at', subset$site[1])
    }
  }

  graph_subtitle <- paste(stringr::str_remove(dmy_style(min(subset$datetime)), '^0'),
                          'to',
                          stringr::str_remove(dmy_style(max(subset$datetime)), '^0'))

  #Create graph ----
  # with time on x axis, temperature on left y axis, and RH on right y axis
  # Y scales 0-40º and 0-100%
  # Dotted lines indicate storage guidelines
  return(
    site_summary |> ggplot2::ggplot(mapping = aes(x = datetime, group = location)) +
      geom_hline(
        yintercept = minmax[1],
        color = col_temp,
        linetype = 'dotted',
        alpha = 0.8
      ) +
      geom_hline(
        yintercept = minmax[2],
        color = col_temp,
        linetype = 'dotted',
        alpha = 0.8
      ) +
      geom_hline(
        yintercept = minmax[3] / trh_ratio,
        color = col_RH,
        linetype = 'dotted',
        alpha = 0.6
      ) +
      geom_hline(
        yintercept = minmax[4] / trh_ratio,
        color = col_RH,
        linetype = 'dotted',
        alpha = 0.6
      ) +
      geom_line(
        aes(y = mean_temp),
        color = col_temp,
        size = 0.25,
        alpha = line_alpha
      ) +
      geom_ribbon(
        aes(ymin = min_temp, ymax = max_temp),
        fill = col_temp,
        size = 0.25,
        alpha = 0.1
      ) +
      geom_ribbon(
        aes(ymin = p01_temp, ymax = p99_temp),
        fill = col_temp,
        size = 0.25,
        alpha = 0.1
      ) +
      geom_line(
        aes(y = mean_RH / trh_ratio),
        color = col_RH,
        size = 0.25,
        alpha = line_alpha
      ) +
      geom_ribbon(
        aes(ymin = min_RH / trh_ratio, ymax = max_RH / trh_ratio),
        fill = col_RH,
        size = 0.25,
        alpha = 0.1
      ) +
      geom_ribbon(
        aes(ymin = p01_RH / trh_ratio, ymax = p99_RH / trh_ratio),
        fill = col_RH,
        size = 0.25,
        alpha = 0.1
      ) +
      scale_x_datetime(
        name = 'Date',
        date_breaks = breaks,
        date_labels = date_format
      ) +
      labs(title = graph_title, subtitle = graph_subtitle) +
      scale_y_continuous(
        name = 'Temperature (º C)',
        limits = c(0, max_axis_temp),
        sec.axis = sec_axis(
          ~ . * trh_ratio,
          name = 'Rel. Humidity (%)',
          breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
        )
      ) +
      theme(
        axis.title.y.left = element_text(color = col_temp),
        axis.title.y.right = element_text(color = col_RH)
      )
  )
}

# Graph light ----
#' Graph light data
#'
#' @param envdata A dataframe returned from parse_datalogger.
#' @param store A string matching a single store or a group of stores with a matching pattern.
#' @param exclude_stores A string or vector to exclude a group of stores.
#' @param graph_title Title of graph, else 'All stores at site' or first location if `store` specified.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param breaks Breaks on x axis, like '2 weeks'.
#' @param date_format Format of dates on x axis, like '%d/%m' or '12/24'.
#' @param max_lux Value of maximum lux guideline.
#' @param max_UV Value of maximum UV guideline
#' @param col_lux Colour for visible light line, as named HTML colour or hex.
#' @param col_UV Colour for UV line, as named HTML colour or hex.
#'
#' @returns ggplot2 Object containing visible light and UV graph
#'
#' @examples graph_light(envdata_exhib, graph_title = 'Exhibition cases')
graph_light <- function(envdata,
                        store = FALSE,
                        exclude_stores = FALSE,
                        graph_title = FALSE,
                        start_date = FALSE,
                        end_date = FALSE,
                        breaks = '2 months',
                        date_format = '%m/%y',
                        max_lux = 50,
                        max_UV = 0,
                        col_lux = 'darkgreen',
                        col_UV = 'darkorange') {
  message('Graphing lux/UV')
  subset <- subset_readings(
    envdata,
    store = store,
    exclude_stores = exclude_stores,
    start_date = start_date,
    end_date = end_date
  ) |>
    dplyr::filter(!is.na(lux) & !is.na(UV))

  start_date <- min(subset$datetime)
  end_date <- max(subset$datetime)

  # Graph single store
  # store is identifying part of location, does not have to match whole string
  if (store != FALSE) {
    message('Graphing single store')
    line_alpha <- 1
    if (!graph_title) {
      graph_title <- subset$location[1]
    }
  }

  # Graph all stores ----
  if (store == FALSE) {
    message('Graphing all stores')
    line_alpha <- 0.5
    if (!graph_title) {
      graph_title <- paste('All stores at', subset$site[1])
    }
  }

  graph_subtitle <- paste(stringr::str_remove(dmy_style(subset$start_period[1]), '^0'),
                          'to',
                          stringr::str_remove(dmy_style(subset$end_period[1]), '^0'))

  #Create graph ----
  # with time on x axis, light on left y axis, and UV on right y axis
  #Dotted line indicates max lux for display
  return(
    subset |> ggplot2::ggplot(mapping = aes(x = datetime, group = location)) +
      geom_hline(
        yintercept = max_lux,
        color = col_lux,
        linetype = 'dotted',
        alpha = 0.8
      ) +
      geom_hline(
        yintercept = max_UV,
        color = col_UV,
        linetype = 'dotted',
        alpha = 0.8
      ) +
      geom_line(
        aes(y = lux),
        color = col_lux,
        size = 0.25,
        alpha = line_alpha
      ) +
      geom_line(
        aes(y = UV / 2.5),
        color = col_UV,
        size = 0.25,
        alpha = line_alpha
      ) +
      scale_x_datetime(
        name = 'Date',
        date_breaks = breaks,
        date_labels = date_format
      ) +
      labs(title = graph_title, subtitle = graph_subtitle) +
      scale_y_continuous(
        name = 'Visible light (lux)',
        sec.axis = sec_axis(~ . / max(subset$lux), name = 'UV (μW/lumen)')
      ) +
      theme(
        axis.title.y.left = element_text(color = col_lux),
        axis.title.y.right = element_text(color = col_UV)
      )
  )
}

#' Graph light data summary
#'
#' @param envdata A dataframe returned from parse_datalogger.
#' @param type 'annual', 'monthly', or 'daily'
#' @param store A string matching a single store or a group of stores with a matching pattern.
#' @param exclude_stores A string or vector to exclude a group of stores.
#' @param graph_title Title of graph, else 'All stores at site' or first location if `store` specified.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param date_format Format of dates on x axis, like '%d/%m' or '12/24'.
#' @param breaks Breaks on x axis, like '2 weeks'.
#' @param max_lux Value of maximum lux guideline.
#' @param max_UV Value of maximum UV guideline
#' @param col_lux Colour for visible light line and ribbon, as named HTML colour or hex.
#' @param col_UV Colour for UV line and ribbon, as named HTML colour or hex.
#'
#' @returns ggplot2 Object containing visible light and UV graph with mean and min/max ribbons
#'
#' @examples graph_light_summary(envdata_exhib, graph_title = 'Exhibition cases', type = 'daily')
graph_light_summary <- function(envdata,
                                graph_title = FALSE,
                                store = FALSE,
                                exclude_stores = FALSE,
                                start_date = FALSE,
                                end_date = FALSE,
                                type = 'monthly',
                                breaks = '2 months',
                                date_format = '%m/%Y',
                                max_lux = 50,
                                max_UV = 0,
                                col_lux = 'darkgreen',
                                col_UV = 'darkorange') {
  message('Graphing max/min/mean')
  subset <- subset_readings(
    envdata,
    store = store,
    exclude_stores = exclude_stores,
    start_date = start_date,
    end_date = end_date
  )
  if (type == 'monthly') {
    subset <-  dplyr::mutate(subset, datetime = as.POSIXct(paste0(year, '-', month, '-01'), format = '%Y-%m-%d'))
  }

  # Graph single store
  # store is identifying part of location, does not have to match whole string
  if (store != FALSE) {
    message('Graphing single store')
    line_alpha <- 1
    fill_alpha <- 0.2
    if (!graph_title) {
      graph_title <- subset$location[1]
    }
  }

  # Graph all stores
  if (store == FALSE) {
    message('Graphing all stores')
    line_alpha <- 0.7
    fill_alpha <- 0.05
    if (!graph_title) {
      graph_title <- paste('All stores at', subset$site[1])
    }
  }

  date_style <- stamp(date_format)
  graph_subtitle <- paste(dmy_style(min(subset$datetime)), 'to', dmy_style(max(subset$datetime)))

  #Create graph ----
  # with time on x axis, lux on left y axis, and UV on right y axis
  #Dotted line indicates max 50 lux for display
  return(
    subset |> ggplot2::ggplot(mapping = aes(x = datetime, group = location)) +
      geom_hline(
        yintercept = max_lux,
        color = col_lux,
        linetype = 'dotted',
        alpha = 0.8
      ) +
      geom_line(
        aes(y = mean_lux),
        color = col_lux,
        size = 0.25,
        alpha = line_alpha
      ) +
      geom_ribbon(
        aes(ymin = min_lux, ymax = max_lux),
        fill = col_lux,
        size = 0.25,
        alpha = 0.2
      ) +
      geom_ribbon(
        aes(ymin = p01_lux, ymax = p99_lux),
        fill = col_lux,
        size = 0.25,
        alpha = 0.1
      ) +
      geom_line(
        aes(y = mean_UV / 2.5),
        color = col_UV,
        size = 0.25,
        alpha = line_alpha
      ) +
      geom_ribbon(
        aes(ymin = min_UV, ymax = max_UV),
        fill = col_UV,
        size = 0.25,
        alpha = 0.1
      ) +
      geom_ribbon(
        aes(ymin = p01_UV, ymax = p99_UV),
        fill = col_UV,
        size = 0.25,
        alpha = 0.1
      ) +
      scale_x_datetime(
        name = 'Date',
        date_breaks = breaks,
        date_labels = date_format
      ) +
      labs(title = graph_title, subtitle = graph_subtitle) +
      scale_y_continuous(name = 'Visible light (lux)', sec.axis = sec_axis(name = 'UV (μW/lumen)')) +
      theme(
        axis.title.y.left = element_text(color = col_lux),
        axis.title.y.right = element_text(color = col_UV)
      )
  )
}

#' Graph standard compliance
#'
#' @param envdata A dataframe returned from parse_datalogger
#' @param type 'o' for overall graph, 't' for temperature, or 'r' for RH.
#' @param standard A string for graph titles and to set minimum and maximum values if recognised.
#' @param exclude_stores A string or vector to exclude a group of stores.
#' @param graph_title Title of graph, else 'All stores at site' or first location if `store` specified.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param min_temp Minimum temperature guideline.
#' @param max_temp Maximum temperature guideline.
#' @param min_RH Minimum RH guideline.
#' @param max_RH Maximum RH guideline.
#' @param col_low Colour of low bar for temp or RH graph, as named HTML colour or hex.
#' @param col_good Colour of good bar for temp or RH graph, as named HTML colour or hex.
#' @param col_high Colour of high bar for temp or RH graph, as named HTML colour or hex.
#' @param grad_bad Colour of low end of gradient for overall graph, as named HTML colour or hex.
#' @param grad_mid Colour of midpoint of gradient for overall graph, as named HTML colour or hex.
#' @param grad_good Colour of high end of gradient for overall graph, as named HTML colour or hex.
#'
#' @returns ggplot2 Object containing bar graph of overall or temp/RH rating for each space in dataframe
#'
#' @examples graph_compliance(envdata, 't', standard = 'Icon')
graph_compliance <- function(envdata,
                             type = 'o',
                             standard = 'BS 4971',
                             exclude_stores = FALSE,
                             graph_title = FALSE,
                             start_date = FALSE,
                             end_date = FALSE,
                             min_temp = FALSE,
                             max_temp = FALSE,
                             min_RH = FALSE,
                             max_RH = FALSE,
                             col_low = '#336699',
                             col_good = '#669933',
                             col_high = '#993322',
                             grad_bad = '#990000',
                             grad_mid = '#CC6600',
                             grad_good = '#006600') {
  message('Graphing standard compliance')
  # Subset readings
  subset <- subset_readings(
    envdata,
    start_date = start_date,
    end_date = end_date,
    exclude_stores = exclude_stores
  )
  # Get rating data
  rated <- compliance(
    subset,
    standard = standard,
    min_temp = min_temp,
    max_temp = max_temp,
    min_RH = min_RH,
    max_RH = max_RH
  )
  # Check what kind of graph it should be and set the search term
  grep_str <- switch(type,
                     'o' = 'standard',
                     't' = 'temp',
                     'r' = 'RH')
  # Filter the rating data for the relevant rows
  rated <-  dplyr::filter(rated, grepl(grep_str, rated$rating))
  if (type == 't' || type == 'r') {
    rated <- dplyr::filter(rated, !grepl('mean', rated$rating))
  }
  # Assign the minimum and maximum by standard or manually
  # (supports BS 4971, PAS 198, Icon, and Bizot)
  minmax <- set_minmax(standard, min_temp, max_temp, min_RH, max_RH)
  # Check for title
  if (graph_title == FALSE) {
    graph_title <- paste(subset$site[1], standard, 'compliance')
  }

  # Assign values for the subtitle and legend, and check
  if (grepl('t', type, ignore.case = T)) {
    message('Graphing temperature compliance')
    graph_subtitle <- paste(
      'Temperature rating',
      stringr::str_remove(dmy_style(min(
        subset$datetime
      )), '^0'),
      'to',
      stringr::str_remove(dmy_style(max(
        subset$datetime
      )), '^0')
    )
    high <- paste0('Above ', minmax[2], 'C')
    low <- paste0('Below ', minmax[1], 'C')
  }
  if (grepl('r', type, ignore.case = T)) {
    message('Graphing RH compliance')
    graph_subtitle <- paste('RH rating',
                            stringr::str_remove(dmy_style(min(
                              subset$datetime
                            )), '^0'),
                            'to',
                            stringr::str_remove(dmy_style(max(
                              subset$datetime
                            )), '^0'))
    high <- paste0('Above ', minmax[4], '%')
    low <- paste0('Below ', minmax[3], '%')
  }
  if (grepl('o', type, ignore.case = T)) {
    message('Graphing overall compliance')
    graph_subtitle <- paste(
      standard,
      'compliance',
      stringr::str_remove(dmy_style(min(
        subset$datetime
      )), '^0'),
      'to',
      stringr::str_remove(dmy_style(max(
        subset$datetime
      )), '^0')
    )
  }

  if (!grepl('o', type)) {
    message('Creating graph')
    return(
      ggplot2::ggplot(rated, aes(
        x = factor(location, levels = rev(levels(
          factor(location)
        ))),
        y = value,
        fill = rating
      )) +
        geom_col() +
        theme(axis.text.x = element_text(angle = 90)) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        labs(
          title = graph_title,
          subtitle = graph_subtitle,
          x = 'Store',
          y = paste('Time within', standard, 'range')
        ) +
        scale_fill_manual(
          name = 'Rating',
          labels = c(high, 'Good', low),
          values = c(col_high, col_good, col_low)
        ) +
        coord_flip()
    )
  }

  if (grepl('o', type)) {
    message('Creating graph')
    # % in BS4971,
    # for descending x = reorder(location,value)
    # for alphabetical x = factor(location,
    # levels = rev(levels(factor(location))))
    return(
      ggplot2::ggplot(rated, aes(
        #for alphabetical
        x = factor(location, levels = rev(levels(
          factor(location)
        ))),
        #for descending
        #x = reorder(location,value),
        y = value
      )) +
        geom_col(aes(fill = value)) +
        scale_fill_gradient2(
          low = grad_bad,
          mid = grad_mid,
          high = grad_good,
          midpoint = .5,
          limits = c(0, 1),
          labels = scales::label_percent()
        ) +
        scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
        theme(axis.text.x = element_text(angle = 90)) +
        labs(
          title = graph_title,
          subtitle = graph_subtitle,
          fill = 'Percent',
          x = 'Store',
          y = paste0('Time within ', standard, ' range')
        ) +
        coord_flip()
    )

  }
}

#' Splice temp and RH graph on specified date
#'
#' @param envdata1 Dataframe returned from parse_datalogger for left of graph
#' @param envdata2 Dataframe returned from parse_datalogger for right of graph
#' @param store1 String to search location in envdata1
#' @param store2 String to search location in envdata2
#' @param move_date Date to splice data
#' @param graph_title Title of graph, otherwise 'Store 1 to Store 2'.
#' @param start_date A string to use as minimum date.
#' @param end_date A string to use as maximum date.
#' @param date_format Format of dates on x axis, like '%d/%m'.
#' @param breaks Breaks on x axis, like '2 weeks'.
#' @param standard Standard to use for temp/RH guidelines.
#' @param min_temp Minimum temperature guideline.
#' @param max_temp Maximum temperature guideline.
#' @param min_RH Minimum RH guideline.
#' @param max_RH Maximum RH guideline.
#' @param max_axis_temp Maximum of temperature axis.
#' @param max_axis_RH Maximum of RH axis.
#' @param col_temp Colour for temperature line, as named HTML colour or hex.
#' @param col_RH Colour for RH line, as named HTML colour or hex.
#' @param col_line Colour for vertical line at splice date
#'
#' @returns ggplot2 Object containing temp and RH graph of two sets of data spliced on `move_date`.
#'
#' @examples graph_move(envdata_old, envdata_new, store1 = 'Archive 1', store2 = 'Archive A', move_date = '2025-01-01')
graph_move <- function(envdata1,
                       envdata2,
                       store1,
                       store2,
                       move_date,
                       graph_title = FALSE,
                       start_date = FALSE,
                       end_date = FALSE,
                       date_format = '%m/%Y',
                       breaks = '2 months',
                       standard = 'BS 4971',
                       min_temp = FALSE,
                       max_temp = FALSE,
                       min_RH = FALSE,
                       max_RH = FALSE,
                       max_axis_temp = 40,
                       max_axis_RH = 100,
                       col_temp = 'red',
                       col_RH = 'blue',
                       col_line = 'darkgrey') {
  premove <- subset_readings(
    envdata1,
    start_date = start_date,
    end_date = move_date,
    store = store1
  )
  postmove <- subset_readings(
    envdata2,
    start_date = move_date,
    end_date = end_date,
    store = store2
  )
  move <- dplyr::bindrows(premove, postmove)
  end_date <- max(move$datetime)
  if (graph_title == FALSE) {
    graph_title <- paste(premove$location[1], 'to', postmove$location[1])
  }
  graph_subtitle <- paste0(
    stringr::str_remove(dmy_style(subset$start_period[1]), '$0'),
    'to',
    stringr::str_remove(dmy_style(subset$end_period[1]), '$0'),
    ', moved ',
    stringr::str_remove(dmy_style(move_date), '^0')
  )

  #Create graph with time on x axis, temperature on left y axis, and RH on right y axis
  #Y scales 0-40º and 0-100%
  return(
    move |> ggplot2::ggplot(mapping = aes(x = datetime)) +
      geom_hline(
        yintercept = min_temp,
        color = col_temp,
        linetype = 'dotted',
        alpha = 0.8
      ) +
      geom_hline(
        yintercept = max_temp,
        color = col_temp,
        linetype = 'dotted',
        alpha = 0.8
      ) +
      geom_hline(
        yintercept = min_RH / trh_ratio,
        color = col_RH,
        linetype = 'dotted',
        alpha = 0.6
      ) +
      geom_hline(
        yintercept = max_RH / trh_ratio,
        color = col_RH,
        alpha = 0.6
      ) +
      geom_vline(xintercept = as.POSIXct(move_date), color = col_line) +
      geom_line(aes(y = temp), color = col_temp, size = 0.25) +
      geom_line(aes(y = RH / trh_ratio), color = col_RH, size = 0.25) +
      scale_x_datetime(name = 'Date') +
      labs(title = graph_title, subtitle = graph_subtitle) +
      scale_y_continuous(
        name = 'Temperature (º C)',
        limits = c(0, max_axis_temp),
        sec.axis = sec_axis( ~ . * trh_ratio, name = 'Rel. Humidity (%)')
      ) +
      theme(
        axis.title.y.left = element_text(color = col_temp),
        axis.title.y.right = element_text(color = col_RH)
      )
  )
}
