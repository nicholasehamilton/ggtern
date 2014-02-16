\name{themearrowlength}
\alias{theme_arrowcustomlength}
\alias{theme_arrowdefault}
\alias{theme_arrowlarge}
\alias{theme_arrowlength}
\alias{theme_arrowlong}
\alias{theme_arrownormal}
\alias{theme_arrowshort}
\alias{theme_arrowsmall}
\alias{themearrowlength}
\title{Change the length of the ternary arrows}
\usage{
  theme_arrowcustomlength(start = getOption("tern.arrowstart"),
    finish = getOption("tern.arrowfinish"))

  theme_arrowlength(start = getOption("tern.arrowstart"),
    finish = getOption("tern.arrowfinish"))

  theme_arrowsmall()

  theme_arrowshort()

  theme_arrownormal()

  theme_arrowdefault()

  theme_arrowlarge()

  theme_arrowlong()
}
\description{
  A set of convenience functions to rapidly change the
  length of the ternary arrows, the convenience functions
  include presents (short, normal, long), or makes
  provision for the user to specify custom fractional
  starting and ending values relative to the size of the
  ternary axis.

  \code{theme_arrowcustomlength} or
  \code{theme_arrowlength} (alias) sets the ternary arrow
  lengths to values as specified by the user, occupying a
  length between start and finish (fractions) of the length
  of the ternary axis. If for some reason, start and finish
  are identical, then the ternary arrows will be switched
  OFF.

  \code{theme_arrowsmall} or \code{theme_arrowshort}(alias)
  reduces the ternary arrows to short arrows, occupying a
  length between \strong{0.4} and \strong{0.6} of the
  length of the ternary axis

  \code{theme_arrownormal} or
  \code{theme_arrowdefault}(alias) reduces the ternary
  arrows to normally sized arrows, occupying a length
  between \code{getOption("tern.arrowstart")} and
  \code{getOption("tern.arrowfinish")} global option
  values, whatever they may be.

  \code{theme_arrowlarge} or \code{theme_arrowlong}(alias)
  increases the ternary arrows to long arrows occupying a
  length between \strong{0.2} and \strong{0.8} of the
  length of the ternary axis
}
\details{
  If the ternary arrows are switched OFF (via the
  \code{\link{theme_hidearrows}} command, or the
  \code{theme(axis.tern.showarrows=FALSE)} theme element),
  then under such circumstance, these convenience functions
  will turn ON the ternary arrows, essentially running
  \code{\link{theme_showarrows}} or
  \code{theme(axis.tern.showarrows=TRUE)}
}
\examples{
\donttest{
 #Create base plot
 plot <- ggtern(data=data.frame(x=1,y=1,z=1),aes(x,y,z)) + geom_point()

 #Pre-Specified Values
 #Short arrows
 plot + theme_arrowsmall()
 plot + theme_arrowshort() #Alias to previous line

 #Normally sized arrows
 plot + theme_arrownormal()
 plot + theme_arrowdefault() #Alias to previous line

 #Long arrows
 plot + theme_arrowlarge()
 plot + theme_arrowlong() #Alias to previous line

 #User-Specified Values
 plot + theme_arrowcustomlength(.1,.8)
 plot + theme_arrowlength(.15,.75) #Alias to previous line
}
}

