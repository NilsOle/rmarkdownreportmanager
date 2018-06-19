#' @export
Datasetvalidator <- R6::R6Class(
  classname = "Datasetvalidator",
  public = list(
    initialize = function(db_connection) {
      private$db_connection <- db_connection
      return(invisible(self))
    },
    validateName = function(name, dataset_update = NULL) {
      checks <-
        list(private$is_valid_dataset_name(name = name, dataset_update = dataset_update))
      if (!all(sapply(checks, function(cur_element) {
        cur_element[["status"]]
      }))) {
        return(list("status" = FALSE,
                    "message" = paste(na.omit(
                      sapply(checks, function(cur_element) {
                        ifelse(is.null(cur_element[["message"]]), NA, cur_element[["message"]])
                      }), collapse = "; "
                    ))))
      } else {
        return(list("status" = TRUE,
                    "message" = ""))
      }
    },
    validateFile = function(filepath) {
      checks <- list(private$is_valid_dataset_file(filepath = filepath))
      if (!all(sapply(checks, function(cur_element) {
        cur_element[["status"]]
      }))) {
        return(list("status" = FALSE,
                    "message" = paste(na.omit(
                      sapply(checks, function(cur_element) {
                        ifelse(is.null(cur_element[["message"]]), NA, cur_element[["message"]])
                      }), collapse = "; "
                    ))))
      } else {
        return(list("status" = TRUE,
                    "message" = ""))
      }
    },
    getFileChecksumOccurence = function(dataset) {
      query <- "SELECT COUNT(*) FROM datasets where filename = ?"
      RODBCext::sqlPrepare(channel = private$db_connection, query = query)
      result <-
        RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          data.frame("filename" =
                       dataset$filename),
          fetch = T,
          rows_at_time = 1,
          believeNRows = FALSE
        )
      if (!is.data.frame(result) ||
          nrow(result) == 0) {
        stop("SQL execution error. Non-emtpy data frame expected.")
      }
      return(as.integer(result[1, 1]))
    }
  ),
  private = list(
    db_connection = NULL,
    is_valid_dataset_file = function(filepath = as.character(filepath)) {
      if (!file.exists(filepath)) {
        return(list("status" = FALSE,
                    "message" = "Could not find file."))
      }
      error_occured <- FALSE
      loaded_obj <-
        tryCatch(
          expr = {
            readRDS(filepath)
          },
          error = function(e) {
            error_occured <<- TRUE
          }
        )
      if (error_occured ||
          !tibble::is_tibble(loaded_obj) ||
          nrow(loaded_obj) == 0 ||
          ncol(loaded_obj) == 0) {
        return(
          list("status" = FALSE,
               "message" = "Invalid dataset file format. Non-empty RDS tibble object expected.")
        )
      }
      return(list("status" = TRUE,
                  "message" = NULL))
    },
    is_valid_dataset_name = function(name = as.character(name),
                                     dataset_update =
                                       NULL) {
      if (private$dataset_exists(name = name,
                                 except = dataset_update$id)) {
        return(list("status" = FALSE,
                    "message" = "A dataset with that name already exists."))
      }
      if (regexpr("^[a-zA-Z0-9_\\-]+$", name) == -1) {
        return(
          list("status" = FALSE,
               "message" = "Invalid dataset name. Name may contain small and capital letters, numbers, underscore and dash.")
        )
      }
      return(list("status" = TRUE,
                  "message" = NULL))
    },
    dataset_exists = function(id = NULL,
                              name = NULL,
                              except = NULL) {
      if (!is.null(name)) {
        values <- name
        query_addition <- ""
        if (!is.null(except)) {
          values <- c(values, except)
          query_addition <- " AND dataset_id != ?"
        }
        query <-
          paste0("SELECT dataset_id from datasets where name = ?",
                 query_addition)
      }
      if (!is.null(id)) {
        values <- as.character(id)
        query <-
          "SELECT 1 from datasets where dataset_id = ?"
      }
      value_df <-
        data.frame(matrix(ncol = length(values), nrow = 1), stringsAsFactors = F)
      value_df[1, ] <- values
      RODBCext::sqlPrepare(channel = private$db_connection, query = query)
      result <-
        RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          value_df,
          fetch = T,
          rows_at_time = 1,
          believeNRows = FALSE
        )
      if (!is.data.frame(result)) {
        stop("SQL execution error. Data frame expected.")
      }
      if (nrow(result) > 0) {
        return(TRUE)
      }
      return(FALSE)
    },
    get_dataset_filename = function(id = as.integer(id)) {
      RODBCext::sqlPrepare(private$db_connection,
                           "SELECT filename FROM datasets WHERE dataset_id = ?")
      result <- RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          id,
          fetch = T,
          rows_at_time = 1,
          believeNRows = FALSE
        )
      if (!is.data.frame(result)) {
        stop("SQL execution error. Data frame expected.")
      }
      if (nrow(result) == 0) {
        return (FALSE)
      }
      return(result[, "filename"])
    }
  )
)
