`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || identical(x, "")) y else x
}

read_env <- function(name, default = NULL) {
  value <- Sys.getenv(name, unset = "")
  if (!nzchar(value)) default else value
}

normalize_clinic_id <- function(value) {
  clinic_id <- tolower(trimws(value %||% "main"))
  if (!clinic_id %in% c("main", "dental")) {
    clinic_id <- "main"
  }
  clinic_id
}

clinic_label_for <- function(clinic_id) {
  switch(
    clinic_id,
    main = "Main Clinic",
    dental = "Dental Clinic",
    "Main Clinic"
  )
}

survey_config <- function(session = NULL) {
  clinic_id <- normalize_clinic_id(read_env("SURVEY_CLINIC", "main"))

  if (!is.null(session)) {
    query_string <- shiny::isolate(session$clientData$url_search %||% "")
    query <- shiny::parseQueryString(query_string)
    if (nzchar(query$clinic %||% "")) {
      clinic_id <- normalize_clinic_id(query$clinic)
    }
  }

  rotation_start <- as.Date(read_env("SURVEY_ROTATION_START", "2026-04-01"))
  if (is.na(rotation_start)) {
    rotation_start <- as.Date("2026-04-01")
  }

  rotation_days <- suppressWarnings(as.integer(read_env("SURVEY_ROTATION_DAYS", "14")))
  if (is.na(rotation_days) || rotation_days < 1) {
    rotation_days <- 14L
  }

  thank_you_seconds <- suppressWarnings(as.numeric(read_env("SURVEY_THANK_YOU_SECONDS", "4")))
  if (is.na(thank_you_seconds) || thank_you_seconds < 1) {
    thank_you_seconds <- 4
  }

  clinic_label <- read_env("SURVEY_CLINIC_LABEL", clinic_label_for(clinic_id))

  list(
    clinic_id = clinic_id,
    clinic_label = clinic_label,
    organization_label = read_env("SURVEY_ORGANIZATION_LABEL", "Radius"),
    rotation_start = rotation_start,
    rotation_days = rotation_days,
    thank_you_seconds = thank_you_seconds,
    storage_mode = tolower(read_env("SURVEY_STORAGE_MODE", "local_csv")),
    google_sheet_id = read_env("SURVEY_GOOGLE_SHEET_ID", ""),
    google_worksheet = read_env("SURVEY_GOOGLE_WORKSHEET", "responses"),
    google_service_json_b64 = read_env("SURVEY_GOOGLE_SERVICE_JSON_B64", ""),
    supabase_url = sub("/$", "", read_env("SURVEY_SUPABASE_URL", "")),
    supabase_key = read_env("SURVEY_SUPABASE_KEY", ""),
    supabase_table = read_env("SURVEY_SUPABASE_TABLE", "survey_responses"),
    time_zone = read_env("SURVEY_TIME_ZONE", "America/Los_Angeles")
  )
}
