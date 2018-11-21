#' @title Use BIDS structure
#' @description Creates necessary sub-folders of a path for a
#' BIDS structure
#'
#' @param path Directory to put the BIDS data structure folders
#' @param participant_label label/identifiers for participants.  The
#' folders will be prefixed with \code{sub-}.
#' @param session_label label/identifiers for sessions.  The
#' folders will be prefixed with \code{ses-}.
#' @param data_type Type of data that will be added.
#' @param recursive Should the \code{path} be created recursively if not
#' available
#'
#' @return A \code{data.frame} of paths and an indicator
#' if they were created
#' @export
add_bids_subject = function(
  path,
  participant_label,
  session_label = "01",
  data_type = c("anat",
                "func",
                "dwi",
                "fmap"),
  recursive = TRUE) {

  #########################################
  # check the formats
  #########################################
  check_alphanumeric(participant_label)
  check_alphanumeric(session_label)


  if (!dir.exists(path)) {
    dir.create(path, recursive = recursive)
  }

  # if (missing(data_type)) {
  #   data_type = NULL
  # }

  eg = expand.grid(
    id = participant_label,
    session = session_label,
    data_type = data_type,
    stringsAsFactors = FALSE)

  eg$participant_label = paste0("sub-", eg$id)
  eg$session_label = paste0("ses-", eg$session)

  eg$path = file.path(path, eg$participant_label,
                      eg$session_label, eg$data_type)
  res = sapply(eg$path, dir.create, recursive = recursive,
               showWarnings = FALSE)
  eg$created = res
  eg$exists = file.exists(eg$path)

  return(eg)
}
