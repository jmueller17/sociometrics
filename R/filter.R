#' @importFrom dplyr filter
#' @importFrom rlang eval_tidy
#' @importFrom rlang quos
#'
#'
#' @title Filter days or list of badges
#'
#' @description Utility wrapper around \code{dplyr::filter} to easily extract rows per "days" and/or
#'  a list of badge ids.
#'
#'  Filtering rows per single days requires conversion of timestamp data to single
#'  day datum; similar, for interaction data, filtering for a list of badges requires scanning both
#'  Badge.ID and Other.ID columns. Using the ellipsis operator \code{...} other filter conditions
#'  will be passed on to \code{dplyr::filter}.
#'
#'  NOTE: the type of arguments accepted by this function combines normal parameter assignments such
#'  as for example \code{filter( ids=c("34") )} with dplyr conditionals that evaluate to logical
#'  TRUE or FALSE \code{filter(Timestamp > "2018-12-12 23:23:23")}. Overall, \code{sociometrics::filter()}
#'  accepts three additional parameters: \code{days}, \code{ids}, and \code{ids.strict} (see parameters).
#'
#' @param x A data frame with sociometric data, either interaction, audio, or accelerometer data
#' @param days Vector of date strings specifying one or several days to filter for.
#' @param ids Vector of badge IDs to retain in data frame.
#' @param ids.strict Logical. If \code{TRUE} only dyads where both nodes are simultaneously listed in
#'  \code{ids} will be retained. If \code{ids.strict=FALSE} (default) either nodes is sufficient.
#' @param ... Additional filtering conditions passed on to \code{dplyr::filter}. These conditions evalute
#'  to logical \code{TRUE|FALSE},
#'
#' @return Original object of class sociometrics.
#'
#' @examples
#' data("smtrx")
#'
#' #get all detects involving badge 3119 or 3180 with RSSI stronger than -75
#' filter(df.interact, ids=c("3119", "3180"), RSSI > -75)
#'
#' #get all detects involving 3119 and 3180 only
#' filter(df.interact, ids=c("3119", "3180"), ids.strict=T)
#'
#' #get badges 3186 or 3119 between specified moments in time
#' filter(df.interact, ids=c("3186", "3119"), Timestamp > "2016-01-18 10:44:45" & Timestamp <  "2016-01-18 10:45:00")
#'
#' @export filter
#'
filter <- function(x, ...){
  UseMethod("filter")
}


#' @export
filter.default <- function(x, ...){

  #quote all input arguments
  params <- rlang::quos(...)

  #preserve class names
  cls <- class(x)

  class(x) <- c("tbl_df", "tbl" , "data.frame")

  #remove custom arguments
  params$ids <- NULL
  params$days <- NULL
  params$ids.strict <- NULL
  params$comp <- NULL

  #and pass rest forward to dplyr
  x <- dplyr::filter(x, !!!params)

  class(x) <- cls

  x
}



#' @describeIn filter Filter dataframe of class "smtrx" with by days.
#'
#' @export
filter.smtrx <- function(x, ...){

  params <- rlang::quos(...)

  cls <- class(x)

  x <- NextMethod("filter")

  days <- rlang::eval_tidy(params$days)

  #subset single day
  if (!is.null(days)){
    formatd <- "%Y-%m-%d"
    x <- subset(x, format(Timestamp, formatd) %in% days)
  }

  class(x) <- cls

  x
}




#' @describeIn filter interaction data frame containing Badge.ID and Other.ID columns.
#'
#' @export
filter.interact <- function(x, ...){

  params <- rlang::quos(...)

  cls <- class(x)

  x <- NextMethod("filter")

  ids <-  rlang::eval_tidy(params$ids)
  ids.strict <- ifelse (is.null(rlang::eval_tidy(params$ids.strict)), FALSE, TRUE)


  #subset badge ids
  if (!is.null(ids) & !ids.strict) {
    x <- subset(x, x$Badge.ID %in% ids | x$Other.ID %in% ids)
  } else if (!is.null(ids) & ids.strict){
    x <- subset(x, x$Badge.ID %in% ids & x$Other.ID %in% ids)
  }

  class(x) <- cls

  x
}


#' @describeIn filter Filter data frames of class "ego" with column Badge.ID
#'
#' @export
filter.ego <- function(x, ...){

  params <- rlang::quos(...)

  cls <- class(x)

  x <- NextMethod("filter")

  ids <-  rlang::eval_tidy(params$ids)

  #subset badge ids. Equivalent to dplyr::filter(ids %in% c(id1, id2, id3, ...))
  if (!is.null(ids) & "Badge.ID" %in% names(x)) {
    x <- subset(x, x$Badge.ID %in% ids )
  }

  class(x) <- cls

  x
}




filter.vol <- function(x, ...){


  if (is.null(compare_tbls2())){
    return(x)
  }

  x <- NextMethod("filter")

  #relative maxium volume of all badge readings for the same timestamp
  if (comp == "rmax"){

    x <- x %>%
      group_by(Timestamp) %>%
      mutate(max = max(Volume)) %>%
      filter(Volume == max) %>%
      arrange(Timestamp) %>%
      select(-max) #remove max columne when finished

  #relative minimum volume of all badge readings for the same timestamp
  } else if (comp == "rmin") {

    x <- x %>%
      group_by(Timestamp) %>%
      mutate(min = min(Volume)) %>%
      filter(Volume == min) %>%
      arrange(Timestamp) %>%
      select(-min)
  }

  x

}



