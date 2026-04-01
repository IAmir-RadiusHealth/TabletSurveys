append_local_csv <- function(response_row, file_path = "responses/survey_responses.csv") {
  dir.create(dirname(file_path), showWarnings = FALSE, recursive = TRUE)

  response_df <- as.data.frame(response_row, stringsAsFactors = FALSE)
  write_header <- !file.exists(file_path)

  utils::write.table(
    response_df,
    file = file_path,
    sep = ",",
    row.names = FALSE,
    col.names = write_header,
    append = !write_header,
    qmethod = "double"
  )

  invisible(file_path)
}

decode_service_json <- function(encoded_json) {
  if (!nzchar(encoded_json)) {
    stop("Missing SURVEY_GOOGLE_SERVICE_JSON_B64.", call. = FALSE)
  }

  jsonlite::base64_dec(encoded_json)
}

append_google_sheet <- function(response_row, config) {
  if (!requireNamespace("googlesheets4", quietly = TRUE)) {
    stop("Package 'googlesheets4' is required for google_sheet storage mode.", call. = FALSE)
  }

  if (!nzchar(config$google_sheet_id)) {
    stop("Missing SURVEY_GOOGLE_SHEET_ID.", call. = FALSE)
  }

  temp_key <- tempfile(fileext = ".json")
  on.exit(unlink(temp_key), add = TRUE)

  writeBin(decode_service_json(config$google_service_json_b64), temp_key)

  googlesheets4::gs4_auth(path = temp_key)
  googlesheets4::sheet_append(
    ss = config$google_sheet_id,
    sheet = config$google_worksheet,
    data = as.data.frame(response_row, stringsAsFactors = FALSE)
  )

  invisible(config$google_sheet_id)
}

append_supabase <- function(response_row, config) {
  if (!requireNamespace("httr2", quietly = TRUE)) {
    stop("Package 'httr2' is required for supabase storage mode.", call. = FALSE)
  }

  if (!nzchar(config$supabase_url)) {
    stop("Missing SURVEY_SUPABASE_URL.", call. = FALSE)
  }

  if (!nzchar(config$supabase_key)) {
    stop("Missing SURVEY_SUPABASE_KEY.", call. = FALSE)
  }

  if (!nzchar(config$supabase_table)) {
    stop("Missing SURVEY_SUPABASE_TABLE.", call. = FALSE)
  }

  endpoint <- sprintf("%s/rest/v1/%s", config$supabase_url, utils::URLencode(config$supabase_table, reserved = TRUE))

  httr2::request(endpoint) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      apikey = config$supabase_key,
      Authorization = paste("Bearer", config$supabase_key),
      Prefer = "return=minimal"
    ) |>
    httr2::req_body_json(response_row, auto_unbox = TRUE) |>
    httr2::req_perform()

  invisible(config$supabase_table)
}

record_response <- function(response_row, config) {
  mode <- tolower(config$storage_mode %||% "local_csv")

  switch(
    mode,
    local_csv = append_local_csv(response_row),
    google_sheet = append_google_sheet(response_row, config),
    supabase = append_supabase(response_row, config),
    none = invisible(NULL),
    stop(sprintf("Unsupported SURVEY_STORAGE_MODE '%s'.", mode), call. = FALSE)
  )
}
