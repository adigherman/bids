#' @title Use BIDS structure
#' @description Creates necessary sub-folders of a path for a
#' BIDS structure
#'
#' @param path Directory to put the BIDS data structure folders
#' @param recursive Should the \code{path} be created recursively if not
#' available
#'
#' @return NULL
#' @export
use_bids = function(
  path,
  recursive = TRUE) {
  if (!dir.exists(path)) {
    dir.create(path, recursive = recursive)
  }
  f = function(x){
    dir.create(file.path(path, x), showWarnings = FALSE)
  }
  f("code")
  f("derivatives")
  f("stimuli")
  f("sourcedata")
  invisible(NULL)
}
