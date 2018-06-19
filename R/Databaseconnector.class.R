#' @export
Databaseconnector <- R6::R6Class(
  classname = "Databaseconnector",
  public = list(
    initialize = function(connection_string) {
      private$db_connection <- RODBC::odbcDriverConnect(connection_string)
      return(self)
    },
    getConnection = function() {
      return(private$db_connection)
    },
    print = function(...) {
      cat(str(self))
      invisible(self)
    }
  ),
  private = list(db_connection = NULL)
)
