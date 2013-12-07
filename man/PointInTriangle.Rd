\name{point_in_triangle}
\alias{point_in_triangle}
\title{Point In Triangle}
\usage{
  point_in_triangle(P, x, y, ..., strictly = FALSE,
    check = TRUE)
}
\arguments{
  \item{P}{numeric vector of length 2 [x,y], representing
  the point to test}

  \item{x}{numeric vector of length 3, representing the x
  coordinates of the triangle}

  \item{y}{numeric vector of length 3, representing the y
  coordinates of the triangle}

  \item{...}{not used}

  \item{strictly}{logical value representing whether inside
  is 'strictly' inside, meaning, that a point lying on the
  edges or vertices is considered outside the triangle when
  strictly is \code{TRUE}.}

  \item{check}{logical validate input data or not.}
}
\description{
  \code{point_in_triangle} is a function that determines if
  point is contained by a triangle via the Barycentric
  Technique
}
\examples{
P <- c(0,0); x <- c(0,2,1); y = c(0,0,1)
point_in_triangle(P,x,y)
}

