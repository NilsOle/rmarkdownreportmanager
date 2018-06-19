#' @export
Reportpackage <- R6::R6Class(
  classname = "Reportpackage",
  public = list(
    id = NULL,
    name = NULL,
    version = NULL,
    file_md5sum = NULL,
    filename = NULL,
    created = NULL,
    last_updated = NULL,
    initialize = function(id,
                          name,
                          version,
                          filename,
                          file_md5sum,
                          created,
                          last_updated,
                          db_connection,
                          validator) {
      self$id <- id
      self$version <- version
      self$name <- name
      self$filename <- filename
      self$file_md5sum <- file_md5sum
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
    setVersion = function(version) {
      self$version <- version
      return(invisible(self))
    },
    setFile = function(filepath) {
      self$file_md5sum <- private$get_file_checksum(filepath = filepath)
      self$filename <-
        paste0(self$file_md5sum, ".tar.gz")
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
