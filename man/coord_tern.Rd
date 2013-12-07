\name{coord_tern}
\alias{coord_tern}
\title{Ternary Coordinate System}
\usage{
  coord_tern(T = "x", L = "y", R = "z", xlim = c(0, 1),
    ylim = c(0, 1), Tlim = c(0, 1), Llim = c(0, 1),
    Rlim = c(0, 1))
}
\arguments{
  \item{T}{the Top Mapping (default x)}

  \item{L}{the Left Mapping (default y)}

  \item{R}{the Right Mapping (default z)}

  \item{xlim}{the range of x in the cartesian space}

  \item{ylim}{the range of y in the cartesian space}

  \item{Tlim}{the range of T in the ternary space}

  \item{Llim}{the range of L in the ternary space}

  \item{Rlim}{the range of R in the ternary space}
}
\value{
  ternary coordinate system object.
}
\description{
  \code{coord_tern} is a function which creates a
  transformation mechanism between the ternary system, and,
  the cartesian system. It inherits from the fixed
  coordinate system, employing fixed ratio between x and y
  axes once transformed.
}

