#' @export
Dataset <- R6::R6Class(
  classname = "Dataset",
  public = list(
    id = NULL,
    name = NULL,
    file_md5sum = NULL,
    filename = NULL,
    rows = NULL,
    cols = NULL,
    created = NULL,
    last_updated = NULL,
    initialize = function(id,
                          name,
                          filename,
                          file_md5sum,
                          rows,
                          cols,
                          created,
                          last_updated,
                          db_connection,
                          validator) {
      self$id <- id
      self$name <- name
      self$filename <- filename
      self$file_md5sum <- file_md5sum
      self$rows <- rows
      self$cols <- cols
      self$created <- created
      self$last_updated <- last_updated
      private$db_connection <- db_connection
      private$validator <- validator
      return(invisible(self))
    },
    print = function(...) {
      cat(str(self))
      invisible(self)
    },
    setName = function(name) {
      self$name <- name
      return(invisible(self))
    },
    setFile = function(filepath) {
      self$file_md5sum <- private$get_file_checksum(filepath = filepath)
      self$filename <-
        paste0(self$file_md5sum, ".rds")
      return(invisible(self))
    },
    setRows = function(rows) {
      self$rows <- rows
      return(invisible(self))
    },
    setCols = function(cols) {
      self$cols <- cols
      return(invisible(self))
    }
  ),
  private = list(
    db_connection = NULL,
    validator = NULL,
    get_file_checksum = function(filepath) {
      return(tools::md5sum(files = filepath))
    }
  )
)
