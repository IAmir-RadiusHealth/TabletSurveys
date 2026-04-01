library(shiny)
library(bslib)
library(jsonlite)

if (file.exists(".Renviron")) {
  readRenviron(".Renviron")
}

source("R/config.R", local = TRUE)
source("R/questions.R", local = TRUE)
source("R/storage.R", local = TRUE)

question_bank <- load_question_bank("data/question_bank.csv")

scale_end_labels <- function(scale_id) {
  switch(
    scale_id,
    agreement = list(start = "\u2190 No", end = "Yes \u2192"),
    ease = list(start = "\u2190 Difficult", end = "Easy \u2192"),
    likelihood = list(start = "\u2190 No", end = "Yes \u2192"),
    list(start = "", end = "")
  )
}

question_screen_ui <- function(config, active_question) {
  buttons <- lapply(seq_len(nrow(active_question$options)), function(i) {
    shiny::actionButton(
      inputId = paste0("choice_", i),
      label = shiny::tags$div(
        class = "survey-option-inner",
        shiny::tags$div(
          class = "survey-option-number",
          active_question$options$value[[i]]
        ),
        shiny::tags$div(
          class = "survey-option-label",
          active_question$options$label[[i]]
        )
      ),
      class = "survey-option"
    )
  })

  shiny::tags$div(
    class = "survey-shell",
    shiny::tags$div(
      class = "survey-banner",
      "Patient Experience Survey"
    ),
    shiny::tags$div(
      class = "survey-header",
      shiny::tags$div(
        class = "survey-category",
        sprintf("%s | %s", config$clinic_label, active_question$meta$category[[1]])
      )
    ),
    shiny::tags$div(
      class = "survey-main",
      shiny::tags$h1(
        class = "survey-question",
        active_question$meta$question_text[[1]]
      ),
      shiny::tags$p(
        class = "survey-instruction",
        "Tap one answer to submit."
      ),
      shiny::tags$div(
        class = "survey-scale-guide",
        shiny::tags$div(
          class = "survey-scale-end survey-scale-start",
          active_question$scale_start[[1]]
        ),
        shiny::tags$div(
          class = "survey-scale-cards-wrap",
          shiny::tags$div(
            class = "survey-options",
            buttons
          )
        ),
        shiny::tags$div(
          class = "survey-scale-end survey-scale-endcap",
          active_question$scale_end[[1]]
        )
      )
    )
  )
}

thank_you_screen_ui <- function(config) {
  shiny::tags$div(
    class = "survey-shell survey-thanks-shell",
    shiny::tags$div(
      class = "survey-thanks-card",
      shiny::tags$h1(class = "survey-thanks-title", "Thank you."),
      shiny::tags$p(class = "survey-thanks-copy", "Your response was recorded.")
    )
  )
}

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bg = "#f2eee5",
    fg = "#173038",
    primary = "#173b6c"
  ),
  title = "Patient Survey",
  fillable_mobile = TRUE,
  shiny::tags$head(
    shiny::tags$style(
      shiny::HTML(
        "
        html, body {
          width: 100%;
          height: 100%;
          overflow: hidden;
        }
        body {
          background:
            radial-gradient(circle at top left, rgba(23, 59, 108, 0.18), transparent 26%),
            linear-gradient(135deg, #f7f2e8 0%, #efe6d6 100%);
          font-family: \"Times New Roman\", Times, serif;
        }
        .bslib-page-fill {
          padding: 0;
        }
        .survey-shell {
          min-height: 100vh;
          display: flex;
          flex-direction: column;
          padding: 0 2.2rem 1.8rem;
          gap: 1rem;
        }
        .survey-banner {
          background: #173b6c;
          color: #ffffff;
          text-align: center;
          font-size: clamp(1.55rem, 2.8vw, 2.4rem);
          font-weight: 700;
          padding: 1.1rem 1.25rem;
          border-radius: 0;
          letter-spacing: 0.01em;
          width: calc(100% + 4.4rem);
          margin-left: -2.2rem;
          margin-right: -2.2rem;
          margin-bottom: 0.8rem;
        }
        .survey-header {
          display: flex;
          align-items: center;
          justify-content: center;
          gap: 1rem;
        }
        .survey-category {
          font-size: 1rem;
          font-weight: 700;
          letter-spacing: 0.04em;
          text-transform: uppercase;
          color: #173b6c;
          text-align: center;
        }
        .survey-main {
          flex: 1;
          display: flex;
          flex-direction: column;
          justify-content: center;
          gap: 1rem;
        }
        .survey-question {
          margin: 0;
          font-size: clamp(2rem, 3.5vw, 3rem);
          line-height: 1.15;
          max-width: none;
          text-align: center;
          font-weight: 700;
        }
        .survey-instruction {
          margin: 0;
          font-size: 1rem;
          color: rgba(23, 48, 56, 0.75);
          text-align: center;
        }
        .survey-scale-guide {
          display: grid;
          grid-template-columns: auto 1fr auto;
          align-items: center;
          gap: 0.8rem;
          color: #173b6c;
          font-size: 1rem;
          font-weight: 700;
          margin-top: 0.25rem;
        }
        .survey-scale-start {
          text-align: left;
          white-space: nowrap;
        }
        .survey-scale-endcap {
          text-align: right;
          white-space: nowrap;
        }
        .survey-scale-cards-wrap {
          min-width: 0;
        }
        .survey-options {
          display: grid;
          grid-template-columns: repeat(5, minmax(0, 1fr));
          gap: 0.7rem;
          margin-top: 0;
        }
        .survey-option {
          width: 100%;
          min-height: 8.2rem;
          border: 2px solid #173b6c;
          border-radius: 1rem;
          padding: 0.7rem 0.55rem;
          background: #ffffff;
          color: #173038;
          font-size: 0.98rem;
          font-weight: 700;
          line-height: 1.2;
          box-shadow: 0 10px 24px rgba(23, 48, 56, 0.08);
          transition: transform 120ms ease, box-shadow 120ms ease, background 120ms ease;
        }
        .survey-option:hover,
        .survey-option:focus,
        .survey-option:active {
          transform: translateY(-2px);
          background: #e8f0fb;
          box-shadow: 0 16px 32px rgba(23, 59, 108, 0.16);
        }
        .survey-option-inner {
          display: flex;
          min-height: 100%;
          flex-direction: column;
          align-items: center;
          justify-content: center;
          gap: 0.45rem;
          white-space: normal;
        }
        .survey-option-number {
          font-size: 2.15rem;
          line-height: 1;
          color: #173b6c;
          font-weight: 700;
        }
        .survey-option-label {
          font-size: 0.92rem;
          line-height: 1.15;
          text-align: center;
          color: #173038;
        }
        .survey-thanks-shell {
          align-items: center;
          justify-content: center;
        }
        .survey-thanks-card {
          width: min(56rem, 100%);
          padding: 3rem;
          border-radius: 2rem;
          background: rgba(255, 255, 255, 0.88);
          box-shadow: 0 18px 48px rgba(23, 48, 56, 0.12);
          text-align: center;
        }
        .survey-kicker {
          color: #173b6c;
          font-size: 1rem;
          font-weight: 700;
          text-transform: uppercase;
          letter-spacing: 0.04em;
        }
        .survey-thanks-title {
          margin: 0.35rem 0 0.5rem;
          font-size: clamp(2.4rem, 5vw, 4.2rem);
        }
        .survey-thanks-copy {
          margin: 0;
          font-size: 1.2rem;
          color: rgba(23, 48, 56, 0.78);
        }
        @media (max-width: 1024px) {
          .survey-shell {
            padding: 0 1.5rem 1.5rem;
          }
          .survey-banner {
            width: calc(100% + 3rem);
            margin-left: -1.5rem;
            margin-right: -1.5rem;
          }
          .survey-scale-guide {
            grid-template-columns: 1fr;
            gap: 0.6rem;
          }
          .survey-scale-start,
          .survey-scale-endcap {
            text-align: center;
          }
          .survey-options {
            grid-template-columns: repeat(5, minmax(0, 1fr));
          }
        }
        @media (max-width: 700px) {
          .survey-header {
            align-items: center;
          }
          .survey-shell {
            padding: 0 1rem 1rem;
          }
          .survey-banner {
            width: calc(100% + 2rem);
            margin-left: -1rem;
            margin-right: -1rem;
          }
          .survey-scale-guide {
            grid-template-columns: 1fr;
          }
          .survey-options {
            grid-template-columns: repeat(2, minmax(0, 1fr));
          }
          .survey-option {
            min-height: 7rem;
          }
        }
        "
      )
    )
  ),
  uiOutput("screen")
)

server <- function(input, output, session) {
  config <- survey_config(session)
  screen_state <- reactiveValues(name = "question", reset_at = NULL)

  minute_timer <- reactiveTimer(60000, session)

  active_question <- reactive({
    minute_timer()
    question <- select_active_question(
      question_bank = question_bank,
      clinic_id = config$clinic_id,
      today = as.Date(format(Sys.time(), tz = config$time_zone, usetz = FALSE)),
      rotation_start = config$rotation_start,
      rotation_days = config$rotation_days
    )

    ends <- scale_end_labels(question$meta$scale_id[[1]])
    question$scale_start <- ends$start
    question$scale_end <- ends$end
    question
  })

  submit_response <- function(option_index) {
    option_row <- active_question()$options[option_index, , drop = FALSE]
    meta <- active_question()$meta

    response_row <- list(
      created_at = format(Sys.time(), tz = "UTC", usetz = TRUE),
      clinic_id = config$clinic_id,
      clinic_label = config$clinic_label,
      question_id = meta$question_id[[1]],
      question_order = meta$question_order[[1]],
      rotation_slot = active_question()$slot_index,
      category = meta$category[[1]],
      question_text = meta$question_text[[1]],
      scale_id = meta$scale_id[[1]],
      response_value = option_row$value[[1]],
      response_label = option_row$label[[1]],
      session_id = session$token,
      user_agent = session$request$HTTP_USER_AGENT %||% NA_character_
    )

    tryCatch(
      record_response(response_row, config),
      error = function(err) {
        warning(
          sprintf("Failed to record response: %s", conditionMessage(err)),
          call. = FALSE
        )
      }
    )

    screen_state$name <- "thanks"
    screen_state$reset_at <- Sys.time() + config$thank_you_seconds
  }

  for (i in 1:5) {
    local({
      idx <- i
      observeEvent(input[[paste0("choice_", idx)]], {
        submit_response(idx)
      }, ignoreInit = TRUE)
    })
  }

  observe({
    invalidateLater(250, session)
    if (
      identical(screen_state$name, "thanks") &&
      !is.null(screen_state$reset_at) &&
      Sys.time() >= screen_state$reset_at
    ) {
      screen_state$name <- "question"
      screen_state$reset_at <- NULL
    }
  })

  output$screen <- renderUI({
    if (identical(screen_state$name, "thanks")) {
      thank_you_screen_ui(config)
    } else {
      question_screen_ui(config, active_question())
    }
  })
}

shinyApp(ui, server)
