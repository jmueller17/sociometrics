#'
#' @title Match reversed values
#'
#' @description Eliminates directionality of value pairs. Largely a wrapper around
#'  \code{\link{grp_reverse_pairs}} accepting single, double or multi-column data frame
#'  inputs. \code{grp_reverse_pairs} generates a list of unique reverse-value pairs,
#'  and \code{mreverse} assigns these identifiers to all entries of the given
#'  data frame. This is useful to convert directed network edges into undirected ones.
#'
#' @param x Either single- or multi column data frame. In case of data frame containing more than
#'  two columns, \code{cols} needs to identify either a single column (containing splitable
#'  strings) or two columns that hold the value pair.
#' @param cols Vector of column names (or index) if \code{x} has more than two columns.
#' @param sep Single character for spliting single column string values
#' @param into String. Name of additional column to store shared identifier.
#'
#' @return Original dataframe with additional column (specified by \code{into}) containing
#'  shared identifier between value pairs.
#'
#' @details A given list of directed dyads (e.g. "A-B", "B-A", "A-C", "C-A") is converted into
#'  undirected dyads such that "A-B" and "B-A" are both represented as "A-B".
#'
#'  Accepts both, a single character column to be split (e.g. "Student-Teacher") or two
#'  columns such as "Head", "Tail".
#'
#'  In case a single column is provided, needs to contain a string that can be split by
#'  a given separater \code{sep}.
#'
#' @seealso \code{\link{grp_reverse_pairs}}
#'
#'
#' @examples
#' #single column
#' x <- data.frame(a=c("A-B", "B-A", "A-B", "C-D", "D-C", "Z-P", "Z-P"), stringsAsFactors = F)
#' mreverse(x)
#'
#' #two columns
#' x <- data.frame(a=c(1,2,3,4,5), b=c(2,1,4,3,5), d=c("A-B", "B-A", "C-D", "D-C", "F-F"), stringsAsFactors = F)
#' mreverse(x, cols=c(1,2))
#'
#' #or again just the single column with string value pair
#' mreverse(x, cols=c("d"))
#'
#' @exportClass mreverse
#'
#' @export mreverse
mreverse <- function(x, cols, into, sep, as_fct=F){
  UseMethod("mreverse")
}


#' @export
mreverse.default <- function(x, cols, into, sep, as_fct=F){

  cls <- class(x)

  #single column needs to be string
  if (length(cols)==1){
    pairs <- split_single_col(x[,cols])

    #if only one column is given, the string needs to be split
  } else if (ncol(x) == 1) {
    pairs <- split_single_col(x)

  } else if (ncol(x) == 2) {
    pairs <- x
    names(pairs) <- c("a", "b")

  } else if (ncol(x) > 2 & length(cols) == 2) {
    pairs <- x[,cols]
    names(pairs) <- c("a", "b")

  } else {
    stop("Requires single character column or two columns.")
  }

  grp.pairs <- grp_reverse_pairs(pairs, as_fct=as_fct)

  for (i in 1:nrow(grp.pairs)){
    p1 <- grp.pairs[i,]$a
    p2 <- grp.pairs[i,]$b
    g <- grp.pairs[i,]$g

    pairs[which(pairs$a==p1 & pairs$b==p2), into] <- g

  }

  x[, into] <- pairs[,into]

  class(x) <- cls

  x
}


#' @export
mreverse.interact <- function(x, cols=c("Badge.ID", "Other.ID"),
                                   into="Dyad", sep="-", as_fct=F){

  x <- NextMethod("mreverse", x, cols=cols, into=into, sep=sep, as_fct=as_fct)

  x
}



#' @title Assigns shared identifier to reversed value pairs
#'
#' @description Given a list of unique value pairs such as "A-B" and "B-A",
#'  both are assigned the same id.
#'
#' @param x Data frame with two columnes to be matched
#' @param as_fct Logical. If resulting shared identifier should be a factor.
#'
#' @return Original data frame with additional column identifying matching pairs.
#'  Column names are "a", "b" and shared value "g". Duplicated entries are removed
#'  from original list; each value - reverse value pair occurs exactly once.
#'
#' @details A unique list of value pairs is created first. Value- and reverse-value pairs
#'  are then matched. The first occurence of a value pair will be used as shared name. Thus,
#'  if "A-B" is first, both "A-B" and "B-A" will be named "A-B".
#'
#' @seealso \code{\link{mreverse}}
#'
#' @examples
#' x <- data.frame(a=c(1,2,3,4,5,1), b=c(2,1,4,3,5,2))
#' grp_reverse_pairs(x)
#'
#' @export
grp_reverse_pairs <- function(x, as_fct=F){

  if (ncol(x) != 2){
    stop("Matching requires two columns exactly.")
  }

  #make sure values are unique
  x <- unique(x)

  grp.pairs <- NULL

  for (i in 1:nrow(x)){
    p1 <- as.character(x[i,1])
    p2 <- as.character(x[i,2])

    #start off the df
    if (is.null(grp.pairs)){
      grp.pairs <- grp_entry(p1, p2, as_fct = as_fct)

    } else if ( !any((grp.pairs[,1]==p1) & (grp.pairs[,2]==p2))){
      #always add A-B and B-A to result set, irrespective if it occurs in actual
      #unique pair listing. saves some work.
      grp <- grp_entry(p1, p2, as_fct = as_fct)
      grp.pairs <- rbind(grp, grp.pairs)
    }

  }

  if (as_fct) {
    grp.pairs$g <- factor(grp.pairs$g)
  }

  grp.pairs

}




#creates unidirectional (value and reverse value) group entry for data frame.
#If p1 == p2, pair does not need to be
#reversed.
grp_entry <- function(p1,p2, as_fct=F){

  #use merged string as undirected, i.e shared name.
  pair <- paste0(p1, "-", p2)

  if (p1 == p2){
    df <- data.frame(a=c(p1), b=c(p2), g=c(pair), stringsAsFactors = as_fct)
  } else {
    df <-  data.frame(a=c(p1, p2), b=c(p2, p1), g=c(pair, pair), stringsAsFactors = as_fct)
  }

  df
}


#mreverse utility function. In case single character column is provided,
#needs checking if type is character and can be split.
split_single_col <- function(x, sep="-"){

  x <- as_tibble(x)

  if (is.character(x[[1]])){
    pairs <-  separate(x, col=1, into=c("a", "b"), sep=sep, remove=T)
  } else {
    stop("Single column needs to be string")
  }

  pairs
}
