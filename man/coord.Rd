\name{is.linear.ternary}
\alias{coord_aspect.ternary}
\alias{coord_distance.ternary}
\alias{coord_expand_defaults.ternary}
\alias{coord_render_axis_h.ternary}
\alias{coord_render_axis_v.ternary}
\alias{coord_render_bg.ternary}
\alias{coord_render_fg.ternary}
\alias{coord_train.ternary}
\alias{coord_transform.ternary}
\alias{is.linear.ternary}
\title{S3 Method Is Linear}
\usage{
  \method{is.linear}{ternary} (coord)

  \method{coord_transform}{ternary} (coord, data, details,
    verbose = FALSE, revertToCart = TRUE,
    adjustCart = TRUE,
    discard = getOption("tern.discard.external"))

  \method{coord_expand_defaults}{ternary} (coord, scale,
    aesthetic)

  \method{coord_train}{ternary} (coord, scales)

  \method{coord_aspect}{ternary} (coord, details)

  \method{coord_distance}{ternary} (coord, x, y, details)

  \method{coord_render_axis_v}{ternary} (coord, details,
    theme)

  \method{coord_render_axis_h}{ternary} (coord, details,
    theme)

  \method{coord_render_fg}{ternary} (coord, details, theme)

  \method{coord_render_bg}{ternary} (coord, details, theme)
}
\arguments{
  \item{coord}{coordinate system}

  \item{data}{input data}

  \item{details}{scales details}

  \item{verbose}{verbose reporting}

  \item{revertToCart}{fall back to cartesian data if error}

  \item{adjustCart}{adjust for the cartesian scale or not}

  \item{discard}{throw away data outside the plotting
  perimeter}

  \item{scales}{plot scales}

  \item{scale}{plot scale}

  \item{aesthetic}{mappings}

  \item{x}{data}

  \item{y}{data}

  \item{theme}{net theme}
}
\description{
  S3 Method Is Linear

  S3 Method Coordinate Transform

  S3 Method Expand Deraults

  S3 Method Coordinate Train

  S3 Method Coordinate Aspect

  S3 Method Coordinate Distance

  S3 Method Render Vertical Axis

  S3 Method Render Horizontal Axis

  S3 Method Render Foreground

  S3 Method Render Background
}
\keyword{internal}

