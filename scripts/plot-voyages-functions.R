# plot-voyages.R

string_collapse_with_descriptor <- function(vec, n = 10, type_of_entity = NA) {
  # INPUT: Vector of strings
  # OUTPUT: Single string, with type of entity appended, if applicable
  
  string_collapse <- function(vec, n = 10) {
    # INPUT: Vector of strings
    # OUTPUT: Single string, converted to title case
    # PRECONDITIONS: glue package loaded
    
    if(length(vec) <= n) {
      glue_collapse(str_to_title(sort(vec)), sep = ", ", last = " and ")
    } else {
      len <- length(vec)
      glue("{glue_collapse(str_to_title(head(sort(vec), n=n)), sep = ', ')} and {len - n} more")
    }
  }
  
  st <- string_collapse(vec, n)
  
  if(str_detect(st, "more$") & !is.na(type_of_entity)) {
    st_return <- paste0(st, " ", type_of_entity)
  } else {
    st_return <- st
  }
  
  st_return
  
}

# test
# vec <- c("hellevoetsluis", "middelburg", "texel", "rotterdam", "amsterdam", "den helder", "vlissingen")
# n <- 4
# string_collapse(vec, 3)


###### get_ship_observations function ######

get_voyage_details <- function(ports = NA, from = NA, to = NA, ships = NA, 
                               range_lon = NA, range_lat= NA, range_dates = NA) {
  # INPUT: One or more string or vector for one or more parameters
  #        port is a port2 value (lower case)
  #        from is a port2 value (lower case)
  #        to is a port2 value (lower case)
  #        ships is a ShipName (upper case)
  #        range_dates are strings in form like c("1788-12-01", "1790-01-01")
  #        range_lon are two numbers -180 <= lon <= 180 like c(-12.0, -20.5)
  #        range_lat are two numbers -90 <= lat <= 90 like c(45, 90)

  # input values must be exact strings found in df_voyages
  # 
  # RETURN: dataframe with observations that meet all parameters' criteria
  
  # browser()
  
  df <- df_voyages # from global environment
  
  # silently drop bad function parameter values
  # Note: NA has length 1, and intersect doesn't seem to be consistent in setting either NA or null value
  ports <- intersect(ports, c(unique(df$port_from), unique(df$port_to)))
  if ( length(ports) == 0 | any(is.na(ports))) {
    ports <- character(0)
  }
  from <- intersect(from, unique(df$port_from))
  if ( length(from) == 0 | any(is.na(from))) {
    from <- character(0)
  }
  to <- intersect(to, unique(df$port_to))
  if ( length(to) == 0 | any(is.na(to))) {
    to <- character(0)
  }
  ships <- intersect(ships, c(unique(df$ShipName)))
  if ( length(ships) == 0 | any(is.na(ships))) {
    ships <- character(0)
  }
  if ( length(range_dates) == 0 | any(is.na(range_dates))) {
    range_dates <- double(0)
  } else {
    # TODO: add bounds checking and type checking here
  }
  if ( length(range_lon) == 0 | any(is.na(range_lon))) {
    range_lon <- double(0)
  } else {
    # TODO: add bounds checking here
  }
  if ( length(range_lat) == 0 | any(is.na(range_lat))) {
    range_lat <- double(0)
  } else {
    # TODO: add bounds checking here
  }
  
  if ( length(ports) + length(ships) + length(from) + length(to) + length(range_lon) + length(range_lat) == 0)  {
    message("Error: No known ports or ship names provided",
            "\n  usage: plot_voyages(ports = NA, from = NA, to = NA, ships = NA, range_lon = NA, range_lat = NA, range_dates = NA)")
    return()
  }
  
  # filter ports
  if ( length(ports) > 0 ) {
    df <- df %>%
      filter(port_from %in% ports | port_to %in% ports) # was VoyageFrom, VoyageTo
  }
  # filter from/to
  if ( length(from) > 0 & length(to) > 0 ) {
    df <- df %>%
      filter(port_from %in% from | port_to %in% to) # & port_from != port_to
  } else {
    if ( length(from) > 0 ) {
      df <- df %>%
        filter(port_from %in% from)
    } else {
      if ( length(to) > 0 ) {
        df <- df %>%
          filter(port_to %in% to)
      }
    }
  }
  # filter ships
  if ( length(ships) > 0 ) {
    df <- df %>%
      filter(ShipName %in% ships)
  }
  # filter by lon/lat
  if ( length(range_lon) == 2 ) {
    df <- df %>%
      filter(longitude >= min(range_lon) & longitude <= max(range_lon))
  } else {
    if ( length(range_lon) != 0 ) { # | !is.na(range_lon)
      message("Warning: range_lon must be two numbers; ignoring range_lon",
              "\n  usage: plot_voyages(ports = NA, from = NA, to = NA, ships = NA, range_lon = NA, range_lat = NA, range_dates = NA)")
    }
  }
  if ( length(range_lat) == 2 ) {
    df <- df %>%
      filter(latitude >= min(range_lat) & latitude <= max(range_lat))
  } else {
    if ( length(range_lat) != 0 ) { # | !is.na(range_lat)
      message("Warning: range_lat must be two numbers; ignoring range_lat",
              "\n  usage: plot_voyages(ports = NA, from = NA, to = NA, ships = NA, range_lon = NA, range_lat = NA, range_dates = NA)")
    }
  }
  #filter dates
  if ( length(range_dates) == 2 ) {
    df <- df %>%
      filter(ObsDate >= min(ymd(range_dates)) & ObsDate <= max(ymd(range_dates)))
  } else {
    if ( length(range_dates) != 0 ) { # | !is.na(range_lat)
      message("Warning: range_dates must be two dates; ignoring range_lat",
              "\n  usage: plot_voyages(ports = NA, from = NA, to = NA, ships = NA, range_lon = NA, range_lat = NA, range_dates = NA)")
    }
  }
  
  if ( nrow(df) == 0 ) {
    message("Error: No recognized ports or ship match the parameters provided",
            "\n  usage: plot_voyages(ports = NA, from = NA, to = NA, ships = NA, range_lon = NA, range_lat = NA, range_dates = NA)")
    return()
  }
  
  df
  
}

###### summarize_ship_observations function ######

summarize_voyages <- function(ports = NA, from = NA, to = NA, ships = NA, 
                              range_lon = NA, range_lat = NA, range_dates = NA) {
  
  df <- get_voyage_details(ports = ports, from = from, to = to, ships = ships, 
                           range_lon = range_lon, range_lat = range_lat, range_dates = range_dates)
  if ( length(df) == 0 | any(is.na(df$ShipName)) ) {
    return()
  }
  
  my_tbl <- df %>%
    group_by(ShipName, VoyageIni, VoyageFrom, port_from, country_from, VoyageTo, port_to, country_to, Nationality, Company) %>%
    summarize(date_first = min(ObsDate),
              date_last = max(ObsDate),
              n_days = as.integer(difftime(date_last, date_first, units = "days")),
              n_obs = n()) %>%
    ungroup() %>%
    arrange(VoyageIni, ShipName)
  
  my_gt <- my_tbl %>%
    gt() %>%
    tab_source_note(
      source_note = glue("{nrow(distinct(my_tbl, ShipName))} ships and {nrow(my_tbl)} voyages;",
                         " ships from {nrow(distinct(my_tbl, Nationality))} nationalities")
    ) %>%
    tab_options(table.font.names = c("Arial Narrow", default_fonts()),
                table.font.size = 10)
    # opt_table_font(
    #   font = c(
    #     "Arial Narrow",
    #     default_fonts()
    #   )
    # )
 
    my_gt
  
}

###### plot_voyages_function ######

plot_voyages <- function(ports = NA, from = NA, to = NA, ships = NA, 
                         range_lon = NA, range_lat = NA, range_dates = NA, color_var = NA) {
  # INPUT: One or more string or vector for one or more parameters
  #        port is a port2 value (lower case)
  #        from is a port2 value (lower case)
  #        to is a port2 value (lower case)
  #        ships is a ShipName (upper case)
  #        color_var is the variable to use for coloring the voyage lines (can be "nationality" or "year")
  # input values must be exact strings found in df_voyages
  # 
  # RETURN: nothing (so call directly or call with walk() rather than map() )
  # SIDE EFFECT: print one ggplot plot of all voyages to/from the list of ports for relevant ships in the relevant area
  
  ELEMENTS_TITLE_LENGTH <- 6
  ELEMENTS_CAPTION_LENGTH <- 10
  WRAP_TITLE_LENGTH <- 70 # 40
  WRAP_CAPTION_LENGTH <- 120 # 60
  
  # test
  # ports = NA
  # from = NA
  # to = NA
  # ships = NA
  # range_lon = NA
  # range_lat = NA
  # range_dates = NA
  # color_var = NA
  # voyage_list <- df_voyages %>%
  #   filter(port_from == "hampton road" | port_to == "hampton road")
  # ships = voyage_list$ShipName
  # from = "hampton road"
  # to = "hampton road"
  

  # browser()
  
  df <- get_voyage_details(ports = ports, from = from, to = to, ships = ships, 
                           range_lon = range_lon, range_lat= range_lat, range_dates = range_dates)
  
  if ( length(df) == 0 | any(is.na(df$ShipName)) ) {
    return()
  }
  # if ( nrow(df) == 0 || is.na(df) ) {
  #   return()
  # }

    # repeat this here, so params are the right length
  # silently drop bad function parameter values
  # Note: NA has length 1, and intersect doesn't seem to be consistent in setting either NA or null value
  ports <- intersect(ports, c(unique(df$port_from), unique(df$port_to)))
  if ( length(ports) == 0 | any(is.na(ports))) {
    ports <- character(0)  # not sure why this is needed; it's not for ships
  }
  from <- intersect(from, unique(df$port_from))
  if ( length(from) == 0 | any(is.na(from))) {
    from <- character(0)  # not sure why this is needed; it's not for ships
  }
  to <- intersect(to, unique(df$port_to))
  if ( length(to) == 0 | any(is.na(to))) {
    to <- character(0)  # not sure why this is needed; it's not for ships
  }
  ships <- intersect(ships, c(unique(df$ShipName)))
  if ( length(ships) == 0 | any(is.na(ships))) {
    ships <- character(0)  # not sure why this is needed; it's not for ships
  }
  
  if ( length(range_lon) == 0 | any(is.na(range_lon))) {
    range_lon <- double(0)  # not sure why this is needed; it's not for ships
  } else {
    # TODO: add bounds checking here
  }
  if ( length(range_lat) == 0 | any(is.na(range_lat))) {
    range_lat <- double(0)  # not sure why this is needed; it's not for ships
  } else {
    # TODO: add bounds checking here
  }
  if ( length(range_dates) == 0 | any(is.na(range_dates))) {
    range_dates <- double(0)  # not sure why this is needed; it's not for ships
  } else {
    # TODO: add bounds checking and type checking here
  }
   if ( !color_var %in% c("nationality", "year")) {
    color_var <- "nationality" # the default
  }
  
  voyages_for_plot_temp <- df %>%
    group_by(ShipName, VoyageIni, VoyageFrom, VoyageTo, ShipType, Nationality, color_route) %>%
    mutate(voyage_duration_days = as.integer(max(ObsDate) - min(ObsDate))) %>%
    ungroup() %>%
    filter(voyage_duration_days >= MIN_VOYAGE_DURATION) %>%
    mutate(
      direction = if_else(VoyageFrom %in% ports, "origin", "destination"), # TODO: is this too simplistic?
      #direction = if_else(VoyageFrom %in% ports, "A", "B"), # TODO: is this too simplistic?
      direction = if_else(port_from %in% from, "origin", direction),
      direction = if_else(port_to %in% to, "destination", direction),
      VoyageInfo = glue("{year(ymd(VoyageIni))} ({voyage_duration_days}) {str_to_title(VoyageFrom)} - {str_to_title(VoyageTo)}")
    )
  
  ###### make plot strings ######
  
  my_ship_string <- string_collapse_with_descriptor(
    sort(unique(voyages_for_plot_temp$ShipName)), 
    ELEMENTS_TITLE_LENGTH,
    type_of_entity = "ship(s)"
  )
  
  my_ports_string <- string_collapse_with_descriptor(
    sort(unique(c(voyages_for_plot_temp$VoyageFrom, 
                  voyages_for_plot_temp$VoyageTo))), 
    ELEMENTS_CAPTION_LENGTH,
    type_of_entity = "port(s)"
  )
  
  my_ports_from_string <- string_collapse_with_descriptor(
    sort(unique(voyages_for_plot_temp$VoyageFrom)), 
    ELEMENTS_TITLE_LENGTH,
    type_of_entity = "port(s)"
  )
  
  my_ports_to_string <- string_collapse_with_descriptor(
    sort(unique(voyages_for_plot_temp$VoyageTo)), 
    ELEMENTS_TITLE_LENGTH,
    type_of_entity = "port(s)"
  )
  
  my_port2_string <- string_collapse_with_descriptor(
    sort(unique(c(voyages_for_plot_temp$port_from, 
                  voyages_for_plot_temp$port_to))), 
    ELEMENTS_TITLE_LENGTH,
    type_of_entity = "port(s)"
  )
  
  my_port2_from_string <- string_collapse_with_descriptor(
    sort(unique(voyages_for_plot_temp$port_from)), 
    ELEMENTS_TITLE_LENGTH, 
    type_of_entity = "port(s)"
  )
  
  my_port2_to_string <- string_collapse_with_descriptor(
    sort(unique(voyages_for_plot_temp$port_to)), 
    ELEMENTS_TITLE_LENGTH,
    type_of_entity = "port(s)"
  )
  
  my_title <- "Voyages"
  
  if ( length(ships) > 0 ) {
    my_title <- paste0(my_title, glue(" of {my_ship_string}"))
  }
  if ( length(ports) > 0 ) {
    my_title = paste0(my_title, glue(" to/from {my_port2_string}"))
  }
  if ( length(from) > 0 ) {
    my_title <- paste0(my_title, glue(" from {my_port2_from_string}"))
  }
  if ( length(to) > 0 ) {
    my_title <- paste0(my_title, glue(" to {my_port2_to_string}"))
  }
  if ( length(range_lon) == 2 | length(range_lat) == 2 ) {
    my_title <- paste0(my_title, " in area")
  }
  if ( length(range_lon) == 2 ) {
    my_title <- paste0(my_title, " lon(", glue_collapse(range_lon, sep = ", "), ")")
  }
  if ( length(range_lat) == 2 ) {
    my_title <- paste0(my_title, " lat(", glue_collapse(range_lat, sep = ", "), ")")
  }
  # not including date range in the title, since it's in the subtitle
  # if ( length(range_dates) == 2 ) {
  #   my_title <- paste0(my_title, " dates", glue_collapse(range_lat, sep = " - "), ")")
  # }

  my_title <- str_wrap(my_title, WRAP_TITLE_LENGTH) # one last time in case we added lon() or lat()
  
  
  ###### points, lines, bounding box ######
  
  # use points or lines?
  # TODO: would be better if only the voyages crossing 180 use points
  #       or even better, handle the case gracefully and use lines in all cases
  # if (min(voyages_for_plot_temp$longitude) < -175 | max(voyages_for_plot_temp$longitude) > 175) {
  #   # crosses 180 longitude, so leave data as points
  #   voyages_for_plot <- voyages_for_plot_temp %>%
  #     st_as_sf(coords = c("longitude", "latitude"),
  #              crs = "WGS84")
  # } else {
    # we can cast to lines, since we don't cross 180 longitude
    voyages_for_plot <- voyages_for_plot_temp %>%
      st_as_sf(coords = c("longitude", "latitude"),
               crs = "WGS84") %>%
      # following st_cast example at
      # https://stackoverflow.com/questions/50908771/create-multilines-from-points-grouped-by-id-with-sf-package
      group_by(ShipName, VoyageInfo, VoyageIni, VoyageFrom, VoyageTo, ShipType, 
               Nationality, color_route, direction) %>%
      summarise(do_union = FALSE) %>%
      st_cast("LINESTRING") %>%
      ungroup() %>%
      st_wrap_dateline(options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
                       quiet = TRUE)
  # }
  
  # Expand cropping box
  my_bbox <- c(xmin = -179.9, ymin = -89.9, xmax = 179.9, ymax = 89.9)
  
  xx <- voyages_for_plot %>%
    st_transform(crs = my_proj) %>%
    # st_coordinates()
    st_bbox()
  
  #my_xmin = min(xx[, 1]) * 1.1
  if(xx$xmin > 0) {
    my_xmin = xx$xmin * 0.91
  } else {
    my_xmin = xx$xmin * 1.1
  }
  # my_xmax = max(xx[, 1]) * 1.1
  if(xx$xmax < 0) {
    my_xmax = xx$xmax * 0.91
  } else {
    my_xmax = xx$xmax * 1.1
  }
  # my_ymin = min(xx[, 2]) * 1.2
  if(xx$ymin > 0) {
    my_ymin = xx$ymin * 0.9
  } else {
    my_ymin = xx$ymin * 1.2
  }
  # my_ymax = max(xx[, 2]) * 1.2
  if(xx$ymax < 0) {
    my_ymax = xx$ymax * 0.9
  } else {
    my_ymax = xx$ymax * 1.2
  }
  
  my_bbox[[1]] <- my_xmin
  my_bbox[[2]] <- my_ymin
  my_bbox[[3]] <- my_xmax
  my_bbox[[4]] <- my_ymax
  
  # browser()
  
  # distance of one degree lat is constant; not true for lon
  # my_x_range <- my_xmax - my_xmin 
  my_y_range <- my_ymax - my_ymin
  my_buffer_dist <- 0.01 * my_y_range / 111111 # https://www.quora.com/How-many-meters-is-1-degree-latitude?share=1
  
  my_ports_buffer_temp <- df_ports %>%
    filter(port2 %in% df$port_from | port2 %in% df$port_to) %>%
    distinct(port2, .keep_all = TRUE)
  
  if(!any(is.na(my_ports_buffer_temp$lon)) & !any(is.na(my_ports_buffer_temp$lat))) {
    my_ports_buffer <- 
      st_as_sf(my_ports_buffer_temp, coords = c("lon", "lat")) %>%
      st_set_crs("WGS84") %>%
      st_buffer(dist = my_buffer_dist) %>%
      distinct(geometry, .keep_all = TRUE)
    show_port <- TRUE
  } else {
    my_ports_buffer <- NA
    show_port <- FALSE
  }
  
  ###### plot ######
  
  # test
  # color_var <- "year"
  
  p <- ggplot() +
    geom_sf(data = water_outline %>%
              #st_transform(crs = my_proj) %>%
              st_crop(my_bbox),
            fill = "#000000" #fill = "#FFFFFF") + #"#f7fbff") +  # "#56B4E950" blue-coloured water
    ) +
    geom_sf(data = st_graticule(lat = c(-89.9, seq(-80, 80, 20), 89.9)) %>%
              st_transform(crs = my_proj)  %>%
              st_crop(my_bbox),
            color = "gray80", size = 0.25/.pt
    ) +
    geom_sf(data = world %>%
              #st_transform(crs = my_proj)  %>%
              st_crop(my_bbox),
            fill = "#E69F00B0", # "tan",   #"sandy brown"  # "#E69F00B0"
            color = "white smoke", #"snow"
            size = 0.2
    ) +
    { if(show_port) {
      geom_sf(data = my_ports_buffer %>%
                st_transform(crs = my_proj)  %>%
                st_crop(my_bbox),
              color = "yellow", fill = "yellow")
    }
    } +
    { if ( str_to_lower(color_var) == "nationality" ) {
      geom_sf(data = voyages_for_plot %>%
                st_transform(crs = my_proj) %>%
                st_crop(my_bbox),
              size = 0.2, alpha = 1,
              aes(
                color = color_route, #Nationality,
                fill  = color_route,
                linetype = direction)
      )
    }
    } +
    { if ( str_to_lower(color_var) == "nationality" ) {
      scale_color_identity(labels = str_to_title(color_routes$Nationality),
                           breaks = color_routes$color_route,
                           guide = "legend")
    }
    } +
    { if ( str_to_lower(color_var) == "nationality" ) {
      scale_fill_identity(labels = str_to_title(color_routes$Nationality),
                           breaks = color_routes$color_route,
                           guide = "legend")
    }
    } +
    { if ( str_to_lower(color_var) == "year" ) {
      geom_sf(data = voyages_for_plot %>%
                st_transform(crs = my_proj) %>%
                st_crop(my_bbox),
              size = 0.2, alpha = 1,
              aes(
                color = year(VoyageIni),
                fill = year(VoyageIni),
                linetype = direction)
      )
    }
    } +
    { if ( str_to_lower(color_var) == "year" ) {
      scale_color_gradient2(low = "red", mid = "yellow", high = "lime green",
                            midpoint = 1783)
    }
    } +
    { if ( str_to_lower(color_var) == "year" ) {
      scale_fill_gradient2(low = "red", mid = "yellow", high = "lime green",
                            midpoint = 1783)
    }
    } +
    cowplot::theme_map() + 
    guides(color = "none",
           linetype = "none") +
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
    ) +
    labs(title = my_title,
         subtitle = glue("{nrow(distinct(as.data.frame(voyages_for_plot), ShipName, VoyageIni))} voyages",
                         " {min(voyages_for_plot$VoyageIni)} to {max(voyages_for_plot$VoyageIni)}"), # removed "starting"
         color = NULL,
         fill = NULL,
         linetype = NULL,
         caption = str_wrap(glue("Includes the following logbook ports: {my_ports_string}"), WRAP_CAPTION_LENGTH)
    )
  
  p
  
}
