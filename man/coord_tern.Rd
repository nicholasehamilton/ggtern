\name{coord_tern}
\alias{coord_tern}
\title{Ternary Coordinate System}
\usage{
  coord_tern(T = "x", L = "y", R = "z", xlim = c(0, 1),
    ylim = c(0, 1), Tlim = NULL, Llim = NULL, Rlim = NULL,
    clockwise)
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

  \item{clockwise}{(Depreciated) logical (default
  \code{FALSE}) indicating whether the precession of axes
  is clockwise (\code{TRUE}) or counter-clockwise
  (\code{FALSE}).}
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
\details{
  It is important to note that once the \code{coord_tern()}
  coordinate system has been applied, the base plot object
  is no longer strictly a ggplot object, rather, a ggtern
  object where several patches have been applied to
  facilitate correct plotting, including, some limitations
  on the types of geometries which can be used. One such
  essential patch is, for approved geometries previously
  requiring \code{x} and \code{y} coordinates, now require
  an additional \code{z} coordinate.
  \code{\link[ggtern]{geom_segment}} goes one step further
  in that it requires both an additional \code{z} and
  \code{zend} coordinate mappings.
}

