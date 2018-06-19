#' @export
Reportpackageadapter <-
  R6::R6Class(
    classname = "Reportpackageadapter",
    public = list(
      initialize = function(db_connection, reportpackage_validator) {
        private$db_connection <- db_connection
        private$reportpackage_validator <-
          reportpackage_validator
        return(invisible(self))
      },
      getReportpackages = function(md5sum = NULL) {
        md5sum_query <- ""
        data <- NULL
        if (!is.null(md5sum)) {
          md5sum_query <- " WHERE file_md5sum = ?"
          data <- md5sum
        }
        query <-
          paste0(
            "SELECT reportpackage_id, name, version, filename, created, last_updated, file_md5sum FROM reportpackages",
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
                Reportpackage$new(
                  id = as.integer(cur_row["reportpackage_id"]),
                  name = as.character(cur_row["name"]),
                  version = as.character(cur_row["version"]),
                  filename = as.character(cur_row["filename"]),
                  file_md5sum = as.character(cur_row["file_md5sum"]),
                  created = as.integer(cur_row["created"]),
                  last_updated = as.integer(cur_row["last_updated"]),
                  db_connection = private$db_connection,
                  validator = private$reportpackage_validator
                )
              )
            }
          )
        }
        return(return_list)
      },
      createReportpackage = function(reportpackage) {
        query <-
          "INSERT into reportpackages (name, version, filename, created, last_updated, file_md5sum) VALUES (?, ?, ?, ?, ?, ?)"
        RODBCext::sqlPrepare(private$db_connection, query)
        result <-
          RODBCext::sqlExecute(
            private$db_connection,
            NULL,
            data.frame(
              "name" = reportpackage$name,
              "version" =
                reportpackage$version,
              "filename" =
                paste0(reportpackage$file_md5sum, ".tar.gz"),
              "created" =
                reportpackage$created,
              "last_updated" =
                reportpackage$last_updated,
              "file_md5sum" =
                reportpackage$file_md5sum,
              stringsAsFactors = F
            ),
            fetch = T,
            rows_at_time = 1,
            believeNRows = FALSE
          )
        return(list("status" = TRUE,
                    "message" = NULL))
      },
      updateReportpackage = function(reportpackage) {
        query <-
          "UPDATE reportpackages set name = ?, version = ?, filename = ?, last_updated = ?, file_md5sum = ? WHERE reportpackage_id = ?"
        RODBCext::sqlPrepare(private$db_connection, query)
        result <-
          RODBCext::sqlExecute(
            private$db_connection,
            NULL,
            data.frame(
              "name" = reportpackage$name,
              "version" = reportpackage$version,
              "filename" = paste0(reportpackage$file_md5sum, ".tar.gz"),
              "last_updated" = reportpackage$last_updated,
              "file_md5sum" = reportpackage$file_md5sum,
              "reportpackage_id" = reportpackage$id,
              stringsAsFactors = F
            ),
            fetch = T,
            rows_at_time = 1,
            believeNRows = FALSE
          )
        return(list("status" = TRUE,
                    "message" = NULL))
      },
      deleteReportpackage = function(reportpackage) {
        query <- "DELETE FROM reportpackages WHERE reportpackage_id = ?"
        RODBCext::sqlPrepare(private$db_connection, query)
        result <-
          RODBCext::sqlExecute(
            private$db_connection,
            NULL,
            data.frame(
              "reportpackage_id" = reportpackage$id,
              stringsAsFactors = F
            ),
            fetch = T,
            rows_at_time = 1,
            believeNRows = FALSE
          )
        return(list("status" = TRUE,
                    "message" = NULL))
      },
      getReportpackageByMd5sum = function(md5sum) {
        fetch <- self$getReportpackages(md5sum = md5sum)
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
    private = list(
      db_connection = NULL,
      reportpackage_validator = NULL
    )
  )
