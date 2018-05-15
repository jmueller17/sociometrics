#' @title Retrieve unique days of dataset
#'
#' @description Convenient way to retrieve all unique dates within data set. The default formating
#'  of the date retrieves unique "days" (y-m-d), but other formats are possible (e.g. month or years).
#'
#' @param x Data frame with column "Timestamp" or vector of timestamp data.
#' @param ts_format String. Indicating the format of the specified date. Default is
#'  specifies days: "\%Y-\%m-\%d"
#'
#' @return String of unique day-dates.
#'
#' @seealso \code{\link{unique_ids}}
#'
#' @export
unique_dates <- function(x, ts_format="%Y-%m-%d"){

  if (is.data.frame(x) & "Timestamp" %in% names(x) ){
    x <- x$Timestamp
  }

  if ("POSIXct" %in% class(x) ) {
    paste(unique(format(x, ts_format)), collapse=", ")
  } else {
    warning("x is not date object")
  }


}



#' @title Retrieve unique values across several columns
#'
#' @description Convenient way to retrieve all IDs within a given dataset over several columns. In
#'  the context of sociometric data, these are usually directed edge lists; the function provides
#'  a quick access to all badges (nodes) that form part of an interaction network.
#'
#' @param x Dataframe, usually containing sociometric data
#' @param cols Names or indices of columns to gather unique values from
#' @param decreasing Logical. Order badge ids in decreasing or increasing order.
#'  \code{decreasing=NULL} leaves order unaffected.
#'
#' @return Vector of unique values over given columns
#'
#' @details Contrary to \code{dplyr::distinct} if several columns are provided, the function
#'  does not find unique combinations across columns. Unique values are found "globally" by first
#'  stacking all columns into a single vector before finding distinct values.
#'
#' @examples
#' x <- data.frame(a=c(1:10), b=(1:10))
#' unique_ids(x, cols=c("a","b"))
#'
#' #compare to
#'
#' unique(x)
#'
#' @seealso \code{\link{unique_dates}}
#'
#' @export
#'
unique_ids <- function(x, cols=c("Badge.ID", "Other.ID"), decreasing=NULL){

  #check if cols do exist in dataframe
  if (is.character(cols) & !all(cols %in% names(x))){
    stop("\nNo match in dataframe for provided cols '", cols, "'")
  }

  #TODO: check if all columns have same type. otherwise coerced to character.

  #stack all columns into single column
  tmp <- stack(x, select=cols)

  #determine unique values
  ids <- unique(tmp$values)


  if (!is.null(decreasing)){
    ids <- sort(ids, decreasing)
  }

  ids
}



#' @title Cast to sociometrics
#'
#' @description Given a data frame, performs basic checks on availability of Timestamp data and
#'  other common sociometric columns and attaches the corresponding classes to the object. The
#'  \code{read_interaction, read_body, read_audio} functions attach the correct class format to
#'  the dataframe. However, performing other functions on might erase this information which
#'  then prevents the corresponding \code{qexpl()} or \code{anonymize} methods from working.
#'
#' @param x A data frame.
#'
#' @return The original object with corresponding class information attached.
#'
#' @export
#'
as_smtrx <- function(x){

  cls <- class(x)

  cols <- names(x)

  if ("Timestamp" %in% cols){
    cls <- c("smtrx", cls)
  }

  if ("Badge.ID" %in% cols & "Other.ID" %in% cols){
    cls <- c("interact", cls)
  }


  if ("Pitch" %in% cols & "Volume" %in% cols) {
    cls <- c("pitch", "ego", cls)

  } else if ("Activity" %in% cols){
    cls <- c("act", "ego", cls)

  } else if ("Speaking" %in% cols & "Overlap" %in% cols & "Listening" %in% cols & "Silent" %in% cols){
    cls <- c("sp", "ego", cls)

  } else if ("Volume" %in% cols & !("Pitch" %in% cols)){
    cls <- c("vol", "ego", cls)

  } else if ("Participation" %in% cols){
    cls <- c("par", "ego", cls)

  } else if ("Consistency" %in% cols){
    cls <- c("con", "ego", cls)

  } else if ("Rate" %in% cols){
    cls <- c("rate", "ego", cls)

  } else if ("Similar" %in% cols & "Lag" %in% cols){
    cls <- c("mirror", cls)

  } else if ("Left_Right" %in% cols & "Front_Back" %in% cols){
    cls <- c("pos", "ego", cls)

  }

  class(x) <- cls

  x

}




