#' @importFrom tidyr gather
#' @importFrom ggplot2 ggplot
#'
#'
#'
#' @title Quick visual explorations of sociometric datasets
#'
#' @description A set of standard visualizations of interaction, audio, and body movement data.
#'
#' @param x Data frame with sociometric data.
#' @param measure String. For dataframes of type \code{"ego"}, indicates the column name to be mapped
#'  to y-axis.
#' @param title String. Title for the graphic
#' @param colour String. Name of the column to map on aesthetic colour.
#' @param alpha Numeric 0..1. 0 is completely transparent, 1 is completely obscure.
#' @param leg.pos String. In case colour mapping is used, indicates position of the legend, either
#'  "bottom", "top", "left", "right"
#' @param size Numeric. Width of geom_line or size of geom_point.
#'
#' @return graphic object
#'
#' @examples
#'
#' load("data/smtrx.RDATA")
#'
#' #print interaction data
#' qexpl(df.interact)
#'
#' #add unidirectional edge column, named "Pairs"
#' df.interact <- mreverse(df.interact, into="Pairs")
#'
#' qexpl(df.interact, colour="Pairs", title="Interaction pairs")
#'
#' #print volume
#' qexpl(df.vol)
#'
#' #print pitch
#' df.pitch %>%
#'  filter(ids=c("3119", "3184")) %>%
#'  qexpl(size=.2, title="Pitch", leg.pos="right")
#'
#'
#'
#' @export
#'
qexpl <- function(x, measure=NULL, title="", size=1, colour=NULL, alpha=1, leg.pos="none"){
  UseMethod("qexpl")
}


#' @export
qexpl.default <- function(x, measure=NULL, title="", size=1, colour=NULL, alpha=1, leg.pos="none"){
  return(x)
}


#' @export
qexpl.interact <- function(x, title="", size=1, colour=NULL, alpha=1, leg.pos="none"){

  #x$alpha <- ifelse(is.na(x$RSSI), alpha,  alpha-abs(x$RSSI)/2)

  g <- x %>%
    tidyr::gather("BadgeType", "BadgeID", Badge.ID, Other.ID) %>%
    ggplot2::ggplot(aes_string(x="Timestamp", y="BadgeID", colour = colour)) +
    labs(y="Badges", x="", title=title) +
    geom_point(position="jitter", size=size, alpha=alpha)  +
    theme(legend.position=leg.pos)

  g

}


#' @export
qexpl.ego <- function(x, measure, title="", size=1, colour=NULL, alpha=1, leg.pos="none"){

  colour <- ifelse(is.null(colour), "Badge.ID", colour)

  g <- x %>%
    ggplot2::ggplot(aes_string(x="Timestamp", y=measure, colour=colour)) +
    labs(y=measure, x="", title=title) +
    geom_line(size=size, alpha=alpha)  +
    theme(legend.position=leg.pos)

  g
}


#' @export
qexpl.pitch <- function(x, title="", size=1, colour=NULL, alpha=1, leg.pos="none"){

  #class "pitch" is not "ego"; thus explicit call instaed of NextMethod
  #g <- qexpl.ego(x, measure="Pitch", title=title, size=size, colour=colour, alpha=alpha, leg.pos=leg.pos)

  g <- NextMethod("qexpl", x, measure="Pitch", title=title, size=size, colour=colour, alpha=alpha, leg.pos=leg.pos)

  g
}


#' @export
qexpl.vol <- function(x, title="", size=1, colour=NULL, alpha=1, leg.pos="none"){

  g <- NextMethod("qexpl", x, measure="Volume", title=title, size=size, colour=colour, alpha=alpha, leg.pos=leg.pos)

  g
}


#' @export
qexpl.act <- function(x, title="", size=1, colour=NULL, alpha=1, leg.pos="none"){

  g <- NextMethod("qexpl", x, measure="Activity", title=title, size=size, colour=colour, alpha=alpha, leg.pos=leg.pos)

  g
}


