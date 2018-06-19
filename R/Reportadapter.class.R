#' @export
Reportadapter <- R6::R6Class(
  classname = "Reportadapter",
  public = list(
    initialize = function(db_connection,
                          report_validator,
                          fileadapter) {
      private$db_connection <- db_connection
      private$report_validator <- report_validator
      private$fileadapter <- fileadapter
      return(invisible(self))
    },
    getReports = function() {
      query <-
        "SELECT report_id, created, lastrun_started, lastrun_finished, lastrun_status, lastrun_messagelog, lastrun_filesgenerated, config, dataset_md5sum, reportpackage_md5sum, config_md5sum FROM reports"
      RODBCext::sqlPrepare(channel = private$db_connection, query = query)
      result <-
        RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          NULL,
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
              Report$new(
                id = as.integer(cur_row["report_id"]),
                created = as.integer(cur_row["created"]),
                lastrun_started = private$NAtoNull(as.integer(cur_row["lastrun_started"])),
                lastrun_finished = private$NAtoNull(as.integer(cur_row["lastrun_finished"])),
                lastrun_status = private$NAtoNull(as.integer(cur_row["lastrun_status"])),
                lastrun_messagelog = rjson::fromJSON(as.character(cur_row["lastrun_messagelog"])),
                lastrun_filesgenerated = private$NAtoNull(as.integer(cur_row["lastrun_filesgenerated"])),
                config = rjson::fromJSON(as.character(cur_row["config"])),
                dataset_md5sum = as.character(cur_row["dataset_md5sum"]),
                reportpackage_md5sum = as.character(cur_row["reportpackage_md5sum"]),
                config_md5sum = as.character(cur_row["config_md5sum"]),
                db_connection = private$db_connection,
                validator = private$report_validator,
                fileadapter = private$fileadapter,
                adapter = self
              )
            )
          }
        )
      }
      return(return_list)
    },
    createReport = function(report) {
      query <-
        "INSERT into reports (created, lastrun_started, lastrun_finished, lastrun_status, lastrun_messagelog, lastrun_filesgenerated, config, dataset_md5sum, reportpackage_md5sum, config_md5sum) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
      RODBCext::sqlPrepare(private$db_connection, query)
      result <-
        RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          data.frame(
            "created" = report$created,
            "lastrun_started" =
              private$nullToNA(report$lastrun_started),
            "lastrun_finished" =
              private$nullToNA(report$lastrun_finished),
            "lastrun_status" =
              private$nullToNA(report$lastrun_status),
            "lastrun_messagelog" =
              rjson::toJSON(report$lastrun_messagelog),
            "lastrun_filesgenerated" =
              private$nullToNA(report$lastrun_filesgenerated),
            "config" =
              rjson::toJSON(report$config),
            "dataset_md5sum" =
              report$dataset_md5sum,
            "reportpackage_md5sum" =
              report$reportpackage_md5sum,
            "config_md5sum" =
              report$config_md5sum,
            stringsAsFactors = F
          ),
          fetch = T,
          rows_at_time = 1,
          believeNRows = FALSE
        )
      return(list("status" = TRUE,
                  "message" = NULL))
    },
    updateReport = function(report) {
      query <-
        "UPDATE reports set lastrun_started = ?, lastrun_finished = ?, lastrun_status = ?, lastrun_messagelog = ?, lastrun_filesgenerated = ? WHERE report_id = ?"
      RODBCext::sqlPrepare(private$db_connection, query)
      result <-
        RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          data.frame(
            "lastrun_started" = private$nullToNA(report$lastrun_started),
            "lastrun_finished" =
              private$nullToNA(report$lastrun_finished),
            "lastrun_status" =
              private$nullToNA(report$lastrun_status),
            "lastrun_messagelog" =
              rjson::toJSON(report$lastrun_messagelog),
            "lastrun_filesgenerated" =
              private$nullToNA(report$lastrun_filesgenerated),
            "report_id" =
              report$id,
            stringsAsFactors = F
          ),
          fetch = T,
          rows_at_time = 1,
          believeNRows = FALSE
        )
      return(list("status" = TRUE,
                  "message" = NULL))
    },
    deleteReport = function(report) {
      query <- "DELETE FROM reports WHERE report_id = ?"
      RODBCext::sqlPrepare(private$db_connection, query)
      result <- RODBCext::sqlExecute(
        private$db_connection,
        NULL,
        data.frame("report_id" = report$id,
                   stringsAsFactors = F),
        fetch = T,
        rows_at_time = 1,
        believeNRows = FALSE
      )
      return(list("status" = TRUE,
                  "message" = NULL))
    },
    activeReport = function() {
      now <- floor(as.numeric(Sys.time()))
      query <-
        "SELECT report_id FROM reports WHERE lastrun_started IS NOT NULL AND lastrun_finished IS NULL AND lastrun_started > ?"
      RODBCext::sqlPrepare(private$db_connection, query)
      result <-
        RODBCext::sqlExecute(
          private$db_connection,
          NULL,
          data.frame("timestamp" = now - 60 * 30, stringsAsFactors = F),
          fetch = T,
          rows_at_time = 1,
          believeNRows = FALSE
        )
      if (nrow(result) == 0) {
        return(F)
      }
      return(as.integer(result[1, 1]))
    },
    print = function(...) {
      cat(str(self))
      invisible(self)
    }
  ),
  private = list(
    db_connection = NULL,
    report_validator = NULL,
    fileadapter = NULL,
    nullToNA = function(x) {
      ifelse(is.null(x), NA, x)
    },
    NAtoNull = function(x) {
      if (is.na(x)) {
        NULL
      } else {
        x
      }
    }
  )
)
