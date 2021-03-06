% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/add_bids_anat.R
\name{add_bids_anat}
\alias{add_bids_anat}
\title{Use BIDS structure}
\usage{
add_bids_anat(path, participant_label, session_label = "01",
  add_session = TRUE, modality_label, acquisition_label = NULL,
  reconstruction_label = NULL, run_index = NULL, gzipped = TRUE,
  recursive = TRUE)
}
\arguments{
\item{path}{Directory to put the BIDS data structure folders}

\item{participant_label}{label/identifiers for participants.  The
folders will be prefixed with \code{sub-}.}

\item{session_label}{label/identifiers for sessions.  The
folders will be prefixed with \code{ses-}.}

\item{add_session}{Indicator if \code{session_label} should be added
to the filename.}

\item{modality_label}{Label of the imaging modality (e.g. T1w)}

\item{acquisition_label}{Label of acquisition (e.g. highres)}

\item{reconstruction_label}{Label of reconstruction}

\item{run_index}{Index for which run (e.g. 01, 02)}

\item{gzipped}{logical if the file will be a gzipped NIfTI or not.}

\item{recursive}{Should the \code{path} be created recursively if not
available}
}
\value{
A \code{data.frame} of paths and an indicator
if they were created
}
\description{
Creates necessary sub-folders of a path for a
BIDS structure
}
\note{
The format is based on (without line breaks):
sub-<participant_label>[_ses-<session_label>][_acq-<label>]
[_rec-<label>][_run-<index>]_<modality_label>.nii[.gz]
}
