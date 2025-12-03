##--------------------------------------------------------------
## Convert FLOOD-XML CSV outputs to NetCDF + reader
##--------------------------------------------------------------

rm(list = ls())

library(ncdf4)
library(lubridate)

mainDir <- 'D:/PC_Cornell/Data/DFO_floods_damages'
setwd(mainDir)

csv_files <- list.files("./Data/output.data.files",
                        pattern = "\\.csv$",
                        full.names = TRUE)

write_nc_from_csv <- function(csv_path) {
  message("Processing: ", csv_path)
  df <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
  n_events <- nrow(df)
  if (n_events == 0) {
    warning("Empty CSV: ", csv_path)
    return(invisible(NULL))
  }
  
  # Output nc name
  out_nc <- sub("\\.csv$", ".nc", csv_path)
  
  ##--------------------------
  ## Dimensions
  ##--------------------------
  event_dim <- ncdim_def("event", "index", vals = 1:n_events, create_dimvar = TRUE)
  
  var_list <- list()
  
  ##--------------------------
  ## Time handling
  ##--------------------------
  began_date <- if ("Began" %in% names(df)) {
    suppressWarnings(as.Date(df$Began, format = "%Y/%m/%d"))
  } else NULL
  
  ended_date <- if ("Ended" %in% names(df)) {
    suppressWarnings(as.Date(df$Ended, format = "%Y/%m/%d"))
  } else NULL
  
  add_num_var <- function(name, units, longname) {
    if (name %in% names(df)) {
      v <- ncvar_def(name, units, list(event_dim),
                     missval = NA,
                     longname = longname,
                     prec = "double")
      var_list[[length(var_list) + 1]] <<- v
    }
  }
  
  # Core numeric columns
  add_num_var("duration", "days", "Event duration in days")
  add_num_var("deaths",  "persons", "Number of deaths")
  add_num_var("area",    "km2",     "Affected area")
  add_num_var("gdp",     "USD",     "Country-level annual GDP, current USD")
  add_num_var("lat",     "degrees_north", "Latitude of event centroid")
  add_num_var("lon",     "degrees_east",  "Longitude of event centroid")
  
  # Damage estimates (ML models)
  add_num_var("lm_USD",   "USD", "Estimated flood damages from linear model")
  add_num_var("rf_USD",   "USD", "Estimated flood damages from random forest")
  add_num_var("xgb_USD",  "USD", "Estimated flood damages from XGBoost")
  add_num_var("svr_USD",  "USD", "Estimated flood damages from support vector regression")
  add_num_var("brnn_USD", "USD", "Estimated flood damages from Bayesian regularized neural network")
  add_num_var("knn_USD",  "USD", "Estimated flood damages from k-NN regression")
  
  ##--------------------------
  ## Time variables
  ##--------------------------
  time_vars <- list()
  if (!is.null(began_date)) {
    time_start <- as.numeric(began_date - as.Date("1970-01-01"))
    v <- ncvar_def("time_start",
                   "days since 1970-01-01 00:00:00",
                   list(event_dim),
                   longname = "Event start date",
                   prec = "double")
    var_list[[length(var_list) + 1]] <- v
    time_vars$time_start <- time_start
  }
  if (!is.null(ended_date)) {
    time_end <- as.numeric(ended_date - as.Date("1970-01-01"))
    v <- ncvar_def("time_end",
                   "days since 1970-01-01 00:00:00",
                   list(event_dim),
                   longname = "Event end date",
                   prec = "double")
    var_list[[length(var_list) + 1]] <- v
    time_vars$time_end <- time_end
  }
  
  ##--------------------------
  ## Create NetCDF file
  ##--------------------------
  nc <- nc_create(out_nc, vars = var_list, force_v4 = TRUE)
  
  # numeric vars
  for (v in var_list) {
    vname <- v$name
    if (vname %in% names(df)) {
      ncvar_put(nc, vname, df[[vname]])
    }
  }
  
  # time vars
  for (nm in names(time_vars)) {
    ncvar_put(nc, nm, time_vars[[nm]])
  }
  
  ##--------------------------
  ## Global attributes / metadata
  ##--------------------------
  ncatt_put(nc, 0, "title",
            "FLOOD-XML (Flood LOss and Observed Damage using eXplainable Machine Learning)")
  ncatt_put(nc, 0, "summary",
            "A Multi-Model Ensemble Dataset of Global Flood Damages Since the 1980s Based on Explainable Machine Learning Frameworks.")
  ncatt_put(nc, 0, "github_repo",
            "https://github.com/nassernajibi/FLOOD-XML")
  ncatt_put(nc, 0, "source_csv", basename(csv_path))
  ncatt_put(nc, 0, "institution",
            "University of Florida, Agricultural and Biological Engineering (ABE)")
  ncatt_put(nc, 0, "creator_name",  "Nasser Najibi")
  ncatt_put(nc, 0, "creator_email", "nnajibi@ufl.edu")
  ncatt_put(nc, 0, "creator_url",   "https://abe.ufl.edu/people/faculty/nasser-najibi")
  ncatt_put(nc, 0, "project",       "FLOOD-XML global flood damage modeling")
  ncatt_put(nc, 0, "history",
            paste0("Created on ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
  
  # Per-event country and main-cause stored as metadata
  # (same length as other event variables; no unique())
  if ("country" %in% names(df)) {
    ncatt_put(
      nc, 0, "country_events",
      paste(df$country, collapse = "; ")
    )
    ncatt_put(
      nc, 0, "country_events_note",
      "Semicolon-separated list of country names, one per event, in event dimension order."
    )
  }
  if ("MainCause" %in% names(df)) {
    ncatt_put(
      nc, 0, "maincause_events",
      paste(df$MainCause, collapse = "; ")
    )
    ncatt_put(
      nc, 0, "maincause_events_note",
      "Semicolon-separated list of main causes, one per event, in event dimension order."
    )
  }
  
  # Per-event dismagvalue for EM-DAT (only if column exists in that CSV)
  if ("dismagvalue" %in% names(df)) {
    ncatt_put(
      nc, 0, "dismagvalue_events",
      paste(df$dismagvalue, collapse = "; ")
    )
    ncatt_put(
      nc, 0, "dismagvalue_events_note",
      "Semicolon-separated list of EM-DAT disaster magnitude values (Dis.Mag.Value), one per event, in event dimension order."
    )
  }
  
  nc_close(nc)
  message("Wrote NetCDF: ", out_nc)
}

# Run for all CSVs -> NC
invisible(lapply(csv_files, write_nc_from_csv))

##--------------------------------------------------------------
## Reader: load & organize NetCDF back to data.frame
##--------------------------------------------------------------

read_floodxml_nc <- function(nc_path) {
  nc <- nc_open(nc_path)
  on.exit(nc_close(nc))
  
  # number of events
  n_events <- nc$dim$event$len
  
  ## -------------------------
  ## 1. Event-wise numeric vars
  ## -------------------------
  event_vars <- names(nc$var)[
    vapply(
      nc$var,
      function(v) length(v$dim) == 1 && v$dim[[1]]$name == "event",
      logical(1)
    )
  ]
  
  dat <- lapply(event_vars, function(vname) ncvar_get(nc, vname))
  names(dat) <- event_vars
  df <- as.data.frame(dat, stringsAsFactors = FALSE)
  
  ## -------------------------
  ## 2. Convert time_start / time_end to Date (if present)
  ## -------------------------
  if ("time_start" %in% names(df)) {
    df$Began <- as.Date(df$time_start, origin = "1970-01-01")
  }
  if ("time_end" %in% names(df)) {
    df$Ended <- as.Date(df$time_end, origin = "1970-01-01")
  }
  
  ## -------------------------
  ## 3. Rebuild country / MainCause / dismagvalue from GLOBAL ATTRIBUTES
  ## -------------------------
  split_attr <- function(nc, att_name) {
    att <- ncatt_get(nc, 0, att_name)
    if (!att$hasatt) {
      return(rep(NA_character_, n_events))
    }
    out <- strsplit(att$value, ";", fixed = TRUE)[[1]]
    out <- trimws(out)
    if (length(out) != n_events) {
      warning(
        "Length of ", att_name, " (", length(out),
        ") != n_events (", n_events, ") in file: ", nc_path
      )
    }
    out
  }
  
  country_vec    <- split_attr(nc, "country_events")
  maincause_vec  <- split_attr(nc, "maincause_events")
  dismag_vec_chr <- split_attr(nc, "dismagvalue_events")
  
  df$country    <- country_vec[seq_len(n_events)]
  df$MainCause  <- maincause_vec[seq_len(n_events)]
  # dismagvalue is numeric in EM-DAT; convert if present
  df$dismagvalue <- suppressWarnings(as.numeric(dismag_vec_chr[seq_len(n_events)]))
  
  df
}

## Examples usage:
nc_file <- "./Data/output.data.files/EMDAT21_damages_wt_EMDAT21.nc"
df_flood <- read_floodxml_nc(nc_file)
head(df_flood[, c("duration", "deaths", "area", "gdp",
                  "lat", "lon",
                  "lm_USD", "rf_USD","xgb_USD","svr_USD","brnn_USD","knn_USD",
                  "Began", "Ended",
                  "country", "dismagvalue")]) # note here

nc_file <- "./Data/output.data.files/DFO21_damages_wt_EMDAT21.nc"
df_flood <- read_floodxml_nc(nc_file)
head(df_flood[, c("duration", "deaths", "area", "gdp",
                  "lat", "lon",
                  "lm_USD", "rf_USD","xgb_USD","svr_USD","brnn_USD","knn_USD",
                  "Began", "Ended",
                  "country", "MainCause")]) # note here
