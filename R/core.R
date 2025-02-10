#' @export
Bids <- R6Class( # nolint: object_name_linter.
  "Bids",
  public = list(
    root = NULL,
    index = NULL,
    readonly = NULL,
    initialize = function(root, readonly = TRUE) {
      .single_character_check(root, "root")
      .bool_check(readonly, "readonly")

      if (!dir.exists(root)) {
        if (readonly) {
          stop(sprintf("Root directory `%s` does not exist.", root))
        } else {
          warning(
            sprintf("Directory `%s` does not exist. Creating directory...", root)
          )
          dir.create(root, recursive = TRUE)
        }
      }

      self$root <- fs::path_abs(root)
      self$readonly <- readonly
      self$build_index()
      self$print()
    },
    build_index = function() {
      pattern_tsv <- "\\.tsv$"
      all_tsv_files <- list.files(
        path = self$root,
        recursive = TRUE,
        full.names = TRUE,
        pattern = pattern_tsv
      )
      file_names <- basename(all_tsv_files)

      pattern <- paste0(
        "^",
        "(?:sub-(?<subject>[[:alnum:]]+))?",
        "(?:_ses-(?<session>[[:alnum:]]+))?",
        "(?:_task-(?<task>[[:alnum:]]+))?",
        "(?:_tracksys-(?<tracksys>[[:alnum:]]+))?",
        "(?:_acq-(?<acq>[[:alnum:]]+))?",
        "(?:_run-(?<run>[0-9]+))?",
        "_?",
        "(?<datatype>[[:alnum:]]+)\\.tsv$"
      )

      extracted_data <- stringr::str_match(file_names, pattern)

      bids_index <- tibble::tibble(
        file_path = all_tsv_files,
        subject = extracted_data[, "subject"],
        session = extracted_data[, "session"],
        task = extracted_data[, "task"],
        tracksys = extracted_data[, "tracksys"],
        acq = extracted_data[, "acq"],
        run = extracted_data[, "run"],
        datatype = extracted_data[, "datatype"]
      )

      self$index <- bids_index %>%
        dplyr::mutate(across(everything(), ~ tidyr::replace_na(.x, NA_character_)))
    },
    load_participant = function() {
      file_path <- .is_participants_exists(self)
      readr::read_tsv(file_path)
    },
    load_motion = function() {

    },
    load_logs = function() {
      logs_files <- self$index %>%
        dplyr::filter(datatype == "events")

      if (nrow(logs_files) == 0) {
        warning("No log files found.")
        return(NULL)
      }

      data_list <- lapply(seq_len(nrow(logs_files)), function(i) {
        df <- readr::read_tsv(logs_files$file_path[i])
        df$participant_id <- logs_files$subject[i]
        df
      })

      result <- dplyr::bind_rows(data_list)
      return(result)
    },
    print = function() {
      cat("\nBIDS Dataset Summary\n")
      cat(strrep("=", 22), "\n\n")
      cat(sprintf("%-20s %s\n", "Root:", self$root))
      cat(sprintf("%-20s %d\n", "Data Files:", nrow(self$index)))

      subject_count <- length(unique(self$index$subject))
      subjects <- paste(
        paste(head(sort(unique(self$index$subject)), 5), collapse = ", "),
        "..."
      )

      session_values <- self$index$session
      session_values <- session_values[!is.na(session_values)]
      sessions <- if (length(session_values) > 0) {
        paste(sort(unique(session_values)), collapse = ", ")
      } else {
        "None"
      }

      task_values <- self$index$task
      task_values <- task_values[!is.na(task_values)]
      tasks <- if (length(task_values) > 0) {
        paste(sort(unique(task_values)), collapse = ", ")
      } else {
        "None"
      }

      datatype_values <- self$index$datatype
      datatype_values <- datatype_values[!is.na(datatype_values)]
      datatypes <- if (length(datatype_values) > 0) {
        paste(sort(unique(datatype_values)), collapse = ", ")
      } else {
        "None"
      }

      tracksys_values <- self$index$tracksys
      tracksys_values <- tracksys_values[!is.na(tracksys_values)]
      tracksys <- if (length(tracksys_values) > 0) {
        paste(sort(unique(tracksys_values)), collapse = ", ")
      } else {
        "None"
      }

      acq_values <- self$index$acq
      acq_values <- acq_values[!is.na(acq_values)]
      acq <- if (length(acq_values) > 0) {
        paste(sort(unique(acq_values)), collapse = ", ")
      } else {
        "None"
      }

      run_values <- self$index$run
      run_values <- run_values[!is.na(run_values)]
      run <- if (length(run_values) > 0) {
        paste(sort(unique(run_values)), collapse = ", ")
      } else {
        "None"
      }

      cat(sprintf("%-20s %d\n", "Total Subjects:", subject_count))
      cat(sprintf("%-20s %s\n", "Subjects:", subjects))
      cat(sprintf("%-20s %s\n", "Sessions:", sessions))
      cat(sprintf("%-20s %s\n", "Tasks:", tasks))
      cat(sprintf("%-20s %s\n", "Datatypes:", datatypes))
      cat(sprintf("%-20s %s\n", "Tracksys:", tracksys))
      cat(sprintf("%-20s %s\n", "Acquisition:", acq))
      cat(sprintf("%-20s %s\n\n", "Run:", run))
    }
  )
)
