\name{find_global}
\alias{find_global}
\title{Search for Named Object}
\usage{
  find_global(name, env = environment())
}
\arguments{
  \item{name}{character name of object to search for}

  \item{env}{environment to search within as first
  priority}
}
\value{
  Instance of the named object (if it exists), or
  \code{NULL} (if it does not).
}
\description{
  \code{find_global} is a function that conducts a named
  search for the \code{name} object instance, within the
  \code{env} environment. If an instance doesn't exist
  within the \code{env} environment, a search is then
  conducted within the \code{ggtern} and \code{ggplot2}
  namespaces \emph{(in that order)}.
}
\details{
  This is a modified version of the original source as
  provided in \code{ggplot2}, which has the same
  functionality, however, the modification is such that the
  function now additionally searches within the
  \code{ggtern} namespace prior to the \code{ggplot2}
  namespace.
}
\examples{
find_global('scale_x_continuous')
}

