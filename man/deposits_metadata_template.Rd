% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata-template.R
\name{deposits_metadata_template}
\alias{deposits_metadata_template}
\title{Write an empty metadata template to local file}
\usage{
deposits_metadata_template(filename = NULL)
}
\arguments{
\item{filename}{Name or full path to local file where template is to be
written. This file will be created. If a file of that name already exists, it
must first be deleted. The file extension '.json' will be automatically
appended.}
}
\value{
(Invisibly) \code{TRUE} if local file successfully created; otherwise
\code{FALSE}.
}
\description{
The fields are those defined by the Dublin Core Metadata Initiative (DCMI),
defined at
\url{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}. The
template produced by this function is in \code{json} format which can be manually
edited to provide metadata for a deposit.
}
\examples{
filename <- tempfile (fileext = ".json")
deposits_metadata_template (filename)
# then edit that file to complete metadata
}
\seealso{
Other meta: 
\code{\link{dcmi_terms}()},
\code{\link{figshare_categories}()}
}
\concept{meta}
