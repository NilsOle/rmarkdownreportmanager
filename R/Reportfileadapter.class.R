#' @export
Reportfileadapter <- R6::R6Class(
  classname = "Reportfileadapter",
  public = list(
    initialize = function(directory) {
      private$directory <- directory
      return(invisible(self))
    },
    getFilepath = function(report) {
      return(file.path(
        private$directory,
        paste(
          report$dataset_md5sum,
          report$reportpackage_md5sum,
          report$config_md5sum,
          sep = "_"
        )
      ))
    },
    cleanup = function(report) {
      unlink(file.path(self$getFilepath(report = report),
                       "*"), recursive = T)
      return(T)
    },
    delete = function(report) {
      unlink(self$getFilepath(report = report),
             recursive = T)
      return(T)
    },
    create = function(report) {
      dir.create(self$getFilepath(report = report))
      return(T)
    },
    countFiles = function(report) {
      return(length(list.files(self$getFilepath(report = report))))
    },
    getFiles = function(report) {
      tf <- paste0(tempfile(), ".zip")
      fp <-  self$getFilepath(report = report)
      if (length(list.files(fp)) == 0) {
        return(F)
      }
      zip(tf, fp, flags = "-r9X -j")
      return(tf)
    },
    print = function(...) {
      cat(str(self))
      invisible(self)
    }
  ),
  private = list(directory = NULL)
)
