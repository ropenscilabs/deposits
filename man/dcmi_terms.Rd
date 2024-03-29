% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metadata-dcmi.R
\name{dcmi_terms}
\alias{dcmi_terms}
\title{Get names of DCMI terms}
\usage{
dcmi_terms(term = NULL)
}
\arguments{
\item{term}{If specified, match term to official DCMI nomenclature, and
return single match.}
}
\value{
A character vector of DCMI terms.
}
\description{
The Dublin Core Metadata Initiative defines a set of terms at
\url{https://www.dublincore.org/specifications/dublin-core/dcmi-terms/}.
}
\seealso{
Other meta: 
\code{\link{deposits_metadata_template}()},
\code{\link{figshare_categories}()}
}
\concept{meta}
