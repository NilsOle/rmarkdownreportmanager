#' @export
Datasetadapter <- R6::R6Class(
  classname = "Datasetadapter",
  public = list(
    initialize = function(db_connection, dataset_validator) {
      private$db_connection <- db_connection
      private$dataset_validator <- dataset_validator
      return(invisible(self))
    },
    getDatasets = function(md5sum = NULL) {
      md5sum_query <- ""
      data <- NULL
      if (!is.null(md5sum)) {
        md5sum_query <- " WHERE file_md5sum = ?"
        data <- md5sum
      }
      query <-
        paste0(
          "SELECT dataset_id, name, filename, rows, cols, created, last_updated, file_md5sum FROM datasets",
          md5sum_query
        )
      RODBCext::sqlPrepare(channel = private$db_connection, query = query)
      result <-
        RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          data,
          fetch = T,
          rows_at_time = 1,
          believeNRows = FALSE
        )
      if (!is.data.frame(result)) {
        stop("SQL execution error. Data frame expected.")
      }
      return_list <- list()
      if (nrow(result) > 0) {
        return_list <- apply(
          result,
          1,
          FUN = function(cur_row) {
            return(
              Dataset$new(
                id = as.integer(cur_row["dataset_id"]),
                name = as.character(cur_row["name"]),
                filename = as.character(cur_row["filename"]),
                file_md5sum = as.character(cur_row["file_md5sum"]),
                rows = as.integer(cur_row["rows"]),
                cols = as.integer(cur_row["cols"]),
                created = as.integer(cur_row["created"]),
                last_updated = as.integer(cur_row["last_updated"]),
                db_connection = private$db_connection,
                validator = private$dataset_validator
              )
            )
          }
        )
      }
      return(return_list)
    },
    createDataset = function(dataset) {
      query <-
        "INSERT into datasets (name, filename, rows, cols, created, last_updated, file_md5sum) VALUES (?, ?, ?, ?, ?, ?, ?)"
      RODBCext::sqlPrepare(private$db_connection, query)
      result <-
        RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          data.frame(
            "name" = dataset$name,
            "filename" = paste0(dataset$file_md5sum, ".rds"),
            "rows" = dataset$rows,
            "cols" = dataset$cols,
            "created" = dataset$created,
            "last_updated" = dataset$last_updated,
            "file_md5sum" = dataset$file_md5sum,
            stringsAsFactors = F
          ),
          fetch = T,
          rows_at_time = 1,
          believeNRows = FALSE
        )
      return(list("status" = TRUE,
                  "message" = NULL))
    },
    updateDataset = function(dataset) {
      query <-
        "UPDATE datasets set name = ?, filename = ?, rows = ?, cols = ?, last_updated = ?, file_md5sum = ? WHERE dataset_id = ?"
      RODBCext::sqlPrepare(private$db_connection, query)
      result <-
        RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          data.frame(
            "name" = dataset$name,
            "filename" =
              paste0(dataset$file_md5sum, ".rds"),
            "rows" = dataset$rows,
            "cols" = dataset$cols,
            "last_updated" = dataset$last_updated,
            "file_md5sum" = dataset$file_md5sum,
            "dataset_id" = dataset$id,
            stringsAsFactors = F
          ),
          fetch = T,
          rows_at_time = 1,
          believeNRows = FALSE
        )
      return(list("status" = TRUE,
                  "message" = NULL))
    },
    deleteDataset = function(dataset) {
      query <- "DELETE FROM datasets WHERE dataset_id = ?"
      RODBCext::sqlPrepare(private$db_connection, query)
      result <-
        RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          data.frame("dataset_id" = dataset$id,
                     stringsAsFactors = F),
          fetch = T,
          rows_at_time = 1,
          believeNRows = FALSE
        )
      return(list("status" = TRUE,
                  "message" = NULL))
    },
    getDatasetByMd5sum = function(md5sum) {
      fetch <- self$getDatasets(md5sum = md5sum)
      if (length(fetch) != 1) {
        return(FALSE)
      }
      return(fetch[[1]])
    },
    print = function(...) {
      cat(str(self))
      invisible(self)
    }
  ),
  private = list(db_connection = NULL,
                 dataset_validator = NULL)
)
