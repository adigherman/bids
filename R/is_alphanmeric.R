#' @title Checks alphanumeric format of a string
#' @description These functions wrap a simple regular expression for
#' checking if a vector is alpha-numeric characters only.
#'
#' @param x Character vector
#'
#' @return Logical vector (same length as \code{x})
#' @export
#' @note The functions \code{all_alphanumeric} simply wraps
#' \code{is_alphanumeric} within \code{all}, and \code{check_alphanumeric}
#' simply wraps \code{all_alphanumeric} within \code{stop}.
#'
#' @examples
#' x = c("hey", "1hey", "hey1", "$hey", "-o", "/", " hey")
#' is_alphanumeric(x)
is_alphanumeric = function(x) {
  grepl("^[[:digit:][:alpha:]]+$", x)
}

#' @rdname is_alphanumeric
#' @export
all_alphanumeric = function(x) {
  all(is_alphanumeric(x))
}

#' @rdname is_alphanumeric
#' @export
check_alphanumeric = function(x) {
  if (!is.null(x)) {
    if (!all_alphanumeric(x)) {
      x = x[!is_alphanumeric(x)]
      x = paste(x, collapse = ", ")
      stop(paste0("The values of ", x, " were not alphanumeric"))
    }
  }
}