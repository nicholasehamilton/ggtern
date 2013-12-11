\name{scale_L_continuous}
\alias{scale_L_continuous}
\alias{scale_R_continuous}
\alias{scale_T_continuous}
\title{Ternary Continuous Scales}
\usage{
  scale_L_continuous(name = NULL,
    breaks = pretty(c(0, 1), n = 10),
    minor_breaks = pretty(c(0, 1), n = 20),
    labels = 100 * breaks, limits = waiver())

  scale_T_continuous(name = NULL,
    breaks = pretty(c(0, 1), n = 10),
    minor_breaks = pretty(c(0, 1), n = 20),
    labels = 100 * breaks, limits = waiver())

  scale_R_continuous(name = NULL,
    breaks = pretty(c(0, 1), n = 10),
    minor_breaks = pretty(c(0, 1), n = 20),
    labels = 100 * breaks, limits = waiver())
}
\arguments{
  \item{breaks}{the major breaks}

  \item{minor_breaks}{the minor breaks}

  \item{labels}{the major labels}

  \item{limits}{the range of the scale}

  \item{name}{the name of the scale}
}
\description{
  \code{scale_L_continuous} creates a continuous scale for
  the left species in the ternary diagram

  \code{scale_T_continuous} creates a continuous scale for
  the top species in the ternary diagram

  \code{scale_R_continuous} creates a continuous scale for
  the right species in the ternary diagram
}

