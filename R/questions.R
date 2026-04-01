load_question_bank <- function(path = "data/question_bank.csv") {
  question_bank <- utils::read.csv(
    path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  question_bank$question_order <- as.integer(question_bank$question_order)
  question_bank
}

scale_options <- function(scale_id) {
  switch(
    scale_id,
    agreement = data.frame(
      value = 1:5,
      label = c(
        "Strongly disagree",
        "Disagree",
        "Neutral",
        "Agree",
        "Strongly agree"
      ),
      stringsAsFactors = FALSE
    ),
    ease = data.frame(
      value = 1:5,
      label = c(
        "Very difficult",
        "Difficult",
        "Neither difficult nor easy",
        "Easy",
        "Very easy"
      ),
      stringsAsFactors = FALSE
    ),
    likelihood = data.frame(
      value = 1:5,
      label = c(
        "Definitely no",
        "Probably no",
        "Not sure",
        "Probably yes",
        "Definitely yes"
      ),
      stringsAsFactors = FALSE
    ),
    stop(sprintf("Unknown scale_id: %s", scale_id), call. = FALSE)
  )
}

select_active_question <- function(
  question_bank,
  clinic_id,
  today = Sys.Date(),
  rotation_start = as.Date("2026-04-01"),
  rotation_days = 14L
) {
  clinic_questions <- question_bank[question_bank$clinic_id == clinic_id, , drop = FALSE]
  clinic_questions <- clinic_questions[order(clinic_questions$question_order), , drop = FALSE]

  if (nrow(clinic_questions) == 0) {
    stop(sprintf("No questions configured for clinic_id '%s'.", clinic_id), call. = FALSE)
  }

  slot_index <- floor(as.numeric(today - rotation_start) / rotation_days)
  if (is.na(slot_index) || slot_index < 0) {
    slot_index <- 0
  }

  row_index <- (slot_index %% nrow(clinic_questions)) + 1
  meta <- clinic_questions[row_index, , drop = FALSE]

  list(
    meta = meta,
    options = scale_options(meta$scale_id[[1]]),
    slot_index = as.integer(slot_index),
    row_index = as.integer(row_index)
  )
}
