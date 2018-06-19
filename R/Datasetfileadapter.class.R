#' @export
Datasetfileadapter <- R6::R6Class(
  classname = "Datasetfileadapter",
  public = list(
    initialize = function(directory) {
      private$directory <- directory
      return(invisible(self))
    },
    getFile = function(dataset) {
      return(readRDS(self$getFilepath(dataset$filename)))
    },
    createFile = function(dataset, file) {
      filepath_dataset <- self$getFilepath(dataset$filename)
      if (!file.exists(filepath_dataset)) {
        saveRDS(object = file,
                file = filepath_dataset)
      }
      return(invisible(self))
    },
    updateFile = function(dataset, file, prior_filename =
                            NULL) {
      self$createFile(dataset = dataset, file = file)
      if (!is.null(prior_filename) &&
          prior_filename != dataset$filename) {
        self$deleteFile(filename = prior_filename)
      }
      return(invisible(self))
    },
    deleteFile = function(filename) {
      unlink(self$getFilepath(filename))
      return(invisible(self))
    },
    deleteFileByDataset = function(dataset) {
      unlink(self$getFilepath(dataset$filename))
      return(invisible(self))
    },
    getFilepath = function(filename) {
      return(paste0(private$directory, "/", filename))
    },
    print = function(...) {
      cat(str(self))
      invisible(self)
    }
  ),
  private = list(directory = NULL)
)
