#' @title Use BIDS structure for a data file
#' @description Creates necessary sub-folders of a path for a
#' BIDS structure
#'
#' @param path Directory to put the BIDS data structure folders
#' @param participant_label label/identifiers for participants.  The
#' folders will be prefixed with \code{sub-}.
#' @param session_label label/identifiers for sessions.  The
#' folders will be prefixed with \code{ses-}.
#' @param data_type Type of data to be added, will determine the sub-folder
#' @param extension Extension of file to be added
#' @param add_session Indicator if \code{session_label} should be added
#' to the filename.
#' @param modality_label Label of the imaging modality (e.g. T1w)
#' @param acquisition_label Label of acquisition (e.g. highres)
#' @param reconstruction_label Label of reconstruction
#' @param run_index Index for which run (e.g. 01, 02)
#' @param recursive Should the \code{path} be created recursively if not
#' available
#' @note The format is based on (without line breaks):
#' sub-<participant_label>[_ses-<session_label>][_acq-<label>]
#' [_rec-<label>][_run-<index>]_<modality_label>.<extension>
#'
#' @return A \code{data.frame} of paths
#' @export
add_bids_data = function(
  path,
  participant_label,
  session_label = "01",
  data_type = NULL,
  extension = ".nii.gz",
  add_session = TRUE,
  modality_label,
  acquisition_label = NULL,
  reconstruction_label = NULL,
  run_index = NULL,
  recursive = TRUE) {

  stopifnot(!is.null(data_type))
  #########################################
  # check the formats
  #########################################
  check_alphanumeric(participant_label)
  check_alphanumeric(session_label)
  check_alphanumeric(modality_label)
  check_alphanumeric(acquisition_label)
  check_alphanumeric(reconstruction_label)
  check_alphanumeric(run_index)


  if (!dir.exists(path)) {
    dir.create(path, recursive = recursive)
  }

  #########################################
  # paste for null
  #########################################
  null_paste0 = function(pre, x) {
    if (is.null(x)) {
      return(NULL)
    }
    else {
      paste0(pre, x)
    }
  }

  participant_label = paste0("sub-", participant_label)
  session_label = paste0("ses-", session_label)
  acquisition_label = null_paste0("_acq-", acquisition_label)
  reconstruction_label = null_paste0("_rec-", reconstruction_label)
  run_index = null_paste0("_run-", run_index)


  L = list(participant_label = participant_label,
           session_label = session_label,
           data_type = data_type)
  L$acquisition_label = acquisition_label
  L$reconstruction_label = reconstruction_label
  L$run_index = run_index
  L$modality_label = modality_label

  eg = expand.grid(
    L,
    stringsAsFactors = FALSE)

  eg$path = file.path(path, eg$participant_label,
                      eg$session_label, eg$data_type)
  eg$fname = file.path(
    eg$path,
    paste0(
      eg$participant_label, "_",
      eg$session_label,
      eg$acquisition_label,
      eg$reconstruction_label,
      eg$run_index,
      "_", eg$modality_label,
      extension
    )
  )
  return(eg)
}
