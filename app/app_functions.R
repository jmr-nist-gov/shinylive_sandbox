#' Get a base layer for a leaflet map
#'
#' This should be used as the basic layer for a leaflet proxy. It is mostly a
#' convenience function.
#'
#' @param center_long Int: Scalar to center the map longitudinally
#' @param center_lat Int: Scalar to center the map latitudinally
#' @param start_zoom Int: Scalar for the starting zoom level
#'
#' @return A leaflet object
#' @export
#'
#' @examples
#' create_us_map(center_long = -120, center_lat = 50, start_zoom = 4)
create_us_map <- function(center_long, center_lat, start_zoom) {
  return(
    leaflet() %>%
      setView(lng = center_long,
              lat = center_lat,
              zoom = start_zoom) %>%
      addTiles(layerId = 'base')
  )
}

#' Converge multiple date observations
#'
#' At times, date observations arrive as simple date stamps across multiple
#' sensors or observational groups. Converting this easily to a time series
#' object for time series analysis or plotting can be difficult. This is made especially difficult if the observations are raw and there may be multiple instances per day
#' 
#' Character columns will be converted directly via lubridate::as_date. Numeric and
#' integer columns will be assumed to be year values and coerced to a Date
#' format via base::ISOdate(x, 12, 31) as the end of the year. Boundaries
#' provided to date_range are inclusive. If numeric or integer columns do NOT
#' represent the year, convert them properly prior to using this function.
#'
#' @param x Data.frame: Observations of dates
#' @param date_range (Optional) Date vector of length 2 for filtering. The order
#'   provided should be chronological (start first, end second)
#' @param date_format (Optional) Chr vector: Known format(s) for these dates. If
#'   not provided, will check against 24 common formats.
#' @param cumulative Bool: Whether to use direct (FALSE, default) or cumulative
#'   counts.
#'
#' @return An object of class xts with columns matching input x with counts for
#'   each date.
#' @export
#'
#' @examples
create_timeline <- function(x, date_range = NULL, date_format = NULL, cumulative = FALSE) {
  ## This statement is mostly for Shiny integration when a reactive is triggered prior to data being available.
  if (ncol(x) == 0) return(NULL)
  # Ensure argument 'x' is a data frame and attempt to coerce to such if not.
  if (!"data.frame" %in% class(x)) {
    x <- try(as.data.frame(x))
    if (class(x) == 'try-error') stop("Argument 'x' must be of class 'data.frame' or coercible to one.")
  }
  # Package dependencies
  require(xts)
  require(dplyr)
  require(stringi)
  # Establish formats to try for date conversion if necessary.
  if (is.null(date_format)) {
    common_formats <- c("%Y-%m-%d", "%Y/%m/%d", "%m-%d-%Y", "%m/%d/%Y", "%m-%d-%y", "%m/%d/%y",
                        "%B %d, %Y", "%b %d, %Y", "%d %B %Y", "%d %b %Y", "%d %B %y", "%d %b %y",
                        "%d%B%Y", "%d%b%Y", "%d%B%y", "%d%b%y", "%A, %d %B %Y", "%A, %d %b %Y",
                        "%a, %d %B %Y", "%a, %d %b %Y", "%A, %B %d, %Y", "%A, %b %d, %Y", "%a, %B %d, %Y", "%a, %b %d, %Y")
    format_feedback <- paste0("Cannot match these data to any of the following formats:\n", paste0(common_formats, collapse = "\n"))
  } else {
    common_formats <- date_format
    format_feedback <- paste0("Cannot match these data to ", date_format)
  }
  # Apply class conversions by coercing to dates. Assume integers and numerics are years.
  x <- lapply(x, function(x) {
    switch (class(x),
            "factor" = as.Date(as.character(x), tryFormats = common_formats),
            "character" = as.Date(x, tryFormats = common_formats),
            "numeric" = as.Date(ISOdate(x, 12, 31)),
            "integer" = as.Date(ISOdate(x, 12, 31)),
            "Date" = x,
            as.Date(x)
    )
  })
  x <- as.data.frame(x)
  these_dates <- as.Date(sort(unique(unlist(x))))
  if (!is.null(date_range)) {
    if (!class(date_range) == "Date") {
      if (Sys.Date() %in% date_range) {
        sys.date_index <- which(date_range %in% Sys.Date())
        date_range[sys.date_index] <- toString(as.Date(as.numeric(date_range[sys.date_index])))
      }
      date_range <- tryCatch(as.Date(date_range, tryFormats = common_formats))
      if (class(date_range) == "try-error") {
        stop("Parameter 'date_range' is not coercible to class 'Date.' Please provide an applicable 'date_format' argument.")
      }
    }
    if (length(date_range) == 0) {
      stop("Please provide a valid date range.")
    } else if (length(date_range) == 1) {
      warning("Only a single value was provided to 'date_range'. This was assumed to be the start date. Using maximum observed date.")
      these_dates <- these_dates[which(these_dates >= date_range)]
    } else if (length(date_range > 1)) {
      these_dates <- these_dates[which(these_dates >= date_range[1] & these_dates <= date_range[2])]
      if (length(date_range) > 2) warning("More than two values provided to 'date_range'; only the first two will be used.")
    }
  }
  if (length(these_dates) == 0) {
    stop("No dates match the 'date_range' provided.")
  }
  out <- data.frame(date_index = these_dates) %>%
    left_join(x %>%
                gather("event", "date_index") %>%
                group_by(event) %>%
                count(date_index) %>%
                spread(event, n) %>%
                mutate_if(is.numeric, function(x) ifelse(is.na(x), 0, x))
    )
  if (cumulative) {
    out <- out %>%
      mutate_if(is.numeric, cumsum)
  }
  names(out) <- stri_trans_totitle(gsub("[[:punct:]]", " ", names(out)))
  return(xts(out[, -1], out[, 1]))
}

trim_table_cols <- function(df, cols) {
  df <- df |>
    select(
      -starts_with("FK"),
      -starts_with("PK"),
      all_of(unlist(unname(cols)))
    ) |>
    select(
      where(\(x) !all(is.na(x)))
    ) |>
    mutate(
      across(
        where(is.character),
        as.factor
      )
    )
  out <- sapply(cols, \(x) x[x %in% names(df)])
  return(list(df = df, cols = out))
}