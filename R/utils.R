#' @title Retrieve unique dates of dataset
#'
#' @description Convenient way to retrieve all unique dates - usually days - within data set.
#'  Retrieves the unique days ignoring hour, minute and sec information.
#'
#' @param x Data frame with column "Timestamp" or vector of timestamp data.
#' @param ts_format String. Indicating the format of the specified date
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




