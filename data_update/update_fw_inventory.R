library(DBI)
library(odbc)
library(dplyr)
library(readxl)

# Set things up
now <- function() format(lubridate::now("UTC"), "%Y-%m-%d %H:%M:%S UTC")
use_dir <- file.path("D:", "Projects", "shinylive", "biorepo")
log_file <- file.path(
  "D:",
  "Projects",
  "shinylive",
  "biorepo",
  "data_update",
  "log.txt"
)
write_log <- function(msg = log) {
  cat(
    paste0(msg, collapse = "\n"),
    append = TRUE,
    file = log_file
  )
}
log_break <- strrep("-", 70)

# Remove caches older than three months (this runs each weekend)
cache_limit <- 365/4 + 1
expired_files <- file.path(use_dir, "data_update") |>
  list.files(pattern = ".feather", full.names = TRUE) |>
  file.info() |>
  filter(ctime < lubridate::today() - cache_limit)
if (nrow(expired_files) > 0) {
  log <- c(log, sprintf("[%s]\tRemoved %d cache files older than %s (%.0f days).", now(), nrow(expired_files), lubridate::  today() - cache_limit, cache_limit))
  file.remove(rownames(expired_files))
}

# Begin data refresh
start_time <- now()
log <- sprintf("[%s]\tStarting data refresh from Freezerworks.", start_time)

con <- try(dbConnect(odbc(), "Freezerworks64"))
if (inherits(con, "try-error")) {
  log <- c(
    log,
    sprintf("[%s]\tFreezerworks was unavailable. Aborting data refresh.", now()),
    log_break
  )
  write_log()
  dbDisconnect(con)
  stop("Freezerworks unavailable. Aborting data refresh.")
}
use_udf_path <- file.path("O:", "Data Tool Development", "Freezerworks", "UDFs_for_live_inventory.xlsx")
entities <- c("Samples", "Aliquots")


for (entity in entities) {
  log <- c(log, sprintf("[%s]\tReading sheet '%s' from .xlsx file at '%s'.", now(), entity, use_udf_path))
  xl_file <- use_udf_path |>
    readxl::read_excel(sheet = entity)
  xl_file_updated <- names(xl_file)[ncol(xl_file)] |>
    stringr::str_extract("[0-9]{4}-[0-9]{2}-[0-9]{2}]*") |>
    lubridate::as_date()
  log <- c(log, sprintf("[%s]\t'%s' was last updated on %s.", now(), entity, xl_file_updated))
  xl_file_udfs <- xl_file |>
    select(1:2) |>
    filter(INCLUDE) |>
    pull(UDF)
  log <- c(log, sprintf("[%s]\t'%s' includes %d requested UDFs.", now(), entity, length(xl_file_udfs)))
  fw_data <- try(
    tbl(con, entity) |>
      select(any_of(xl_file_udfs)) |>
      collect()
  )
  if (inherits(fw_data, "try-error")) {
    log <- c(log, sprintf("[%s]\t[error]\tCould not read table '%s' from Freezerworks. Trying again.", now(), entity))
    fw_data <- try(
      dbReadTable(con, entity) |>
        select(any_of(xl_file_udfs))
    )
    if (inherits(fw_data, "try-error")) {
      log <- c(log, sprintf("[%s]\t[error]\tCould not read table '%s' from Freezerworks. Aborting data refresh.", now(), entity))
	  log <- c(log, log_break)
      write_log()
      dbDisconnect(con)
      stop("Unable to read from Freezerworks. Aborting data refresh.")
    }
  }
  log <- c(log, sprintf("[%s]\t'%s' includes %s records.", now(), entity, format(nrow(fw_data), big.mark = ",")))
  attr(fw_data, "as_of") <- Sys.time()
  attr(fw_data, "udf_list_updated") <- xl_file_updated
  fname <- gsub("-", "", sprintf("%s_%s.feather", entity, Sys.Date()))
  log <- c(log, sprintf("[%s]\tCaching timestamped '%s' records to ./data_update/%s.", now(), entity, fname))
  feather::write_feather(
    fw_data,
    file.path(use_dir, "data_update", fname)
  )
  fname <- sprintf("%s.feather", entity)
  log <- c(log, sprintf("[%s]\tRefreshing '%s' cache in ./app/%s.", now(), entity, fname))
  feather::write_feather(
    fw_data,
    file.path(use_dir, "app", "data", fname)
  )
  Sys.sleep(2)
}
log <- c(log, sprintf("[%s]\tClosing Freezerworks connection.", now()))
dbDisconnect(con)
log <- c(
  log,
  sprintf("[%s]\tData refresh required %.2f seconds.", now(), difftime(now(), start_time, units = "secs")),
  log_break
)
write_log()
