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


#' @title Multiple file reads
#'
#' @description Utility function for reading multiple files. Allows to extract part of a file name through the
#' provided regular expression. This allows to add additional information when reading several sociometric
#' export files and combining them into a single data frame. An additional column will be added
#' containing information about each original source file.
#'
#' @param readfn Read function for sociometric files. @seealso \code{read_interaction},
#'  \code{read_audio}, or \code{read_body}.
#' @param files List of absolute file paths to be read. Usually produced by \code{list.files}
#' @param pattern. Regular expression pattern for extracting part of the file name.
#'  Extracted fragment will be stored in extra data frame column "ses_info".
#'
#' @return Tibble
#'
#' @export
#'
mread <- function(readfn, files, pattern, ...){

  df <- NULL

  for (file in files){
    tmp <- readfn(file, ...)

    ses_info <- regmatches(file, regexpr(pattern, file))
    ses_info <- substr(ses_info, 2, nchar(ses_info))

    tmp$ses_info <- ses_info
    tmp$SourceFile <- basename(file)

    if (is.null(df)){
      df <- tmp
    } else {
      df <- rbind(df, tmp)
    }

  }

  df

}

#' @importFrom dplyr transmute
#' @importFrom dplyr if_else
#' @importFrom dplyr distinct
#' @import tibble
#' @importFrom magrittr %<>%
#' @import network

#' @title Extract round-robin rating
#'
#' @description The \code{data(gedii_rr)} of package \code{gediismtrx} provides a matrix of
#'  round robin scores for advice seeking, social affinity and psychological safety. This
#'  method provides utility functionalities to convert the round robin matrix into either an
#'  edge list, a network object or a matrix. It performs several transformations if required,
#'  i.e. collapses directed ties to undirected ties or imputes NA values. If directed ties are
#'  converted to undirected ties, several methods can be specified for conversion such as using
#'  only the max or minmum value, the sum, product or mean scores. Also can aggregate network 
#'  ties and convert value ties into binary ones, dichotomizing the network at a certain
#'  threshold. 
#'  
#'  The order in which these operations are carried out is: first, imputation, second, to-undirected ties, 
#'  and last dichotomization. 
#'
#' @param x round-robin matrix
#' @param directed logical. If set to \code{TRUE} will retain a directed edge list. If set to
#'  \code{directed=FALSE} the \code{weight} of each edge will be calculated according to \code{to.undir}.
#' @param dich.at numeric. Dichotomizes the matrix at the specified value (included).
#' @param to.undir string or number. Specifies the method for collapsing directed to undirected
#' edge list. Possible values are "min", "max", "mean", "recip", "weight".
#' See details.
#' @param impute.na string. Indicates how NAs should be imputed
#'  \code{impute.na=["mean"|"recip"|"recip_mean","native", "without"]}. See details.
#' @param as.type string. The round-robin rating can be retrieved in different formats: as
#'  \code{"edgelist"}, \code{"network"} object, or \code{"matrix"}.
#' @param shuffle logical. Default is \code{FALSE}. Reshuffels the column and row names of matrix at random. 
#'
#'
#' @details Round robin ratings are directional where person A rates person B while person B can
#'  of course rate person A differently. On some occasions, there directional weights (or scores)
#'  need to be collapsed, i.e. the directional ties are converted into undirectional ones based upon
#'  certain rules to be controlled by the two parameters \code{directed} and \code{to.undir}.
#'
#'  The \code{to.undir} controls how directed ties are converted to unidirectional ties. The parameter 
#'  takes the following values:
#'
#'  \describe{
#'    \item{"min"}{Will retain the minimum weight of two given ties between node pairs. If the weight
#'     of A->B = 2 and the weight of B->A = 4, then 2 will be retained for both ties.}
#'    \item{"max"}{Will retain the maximum weight of two given ties between node pairs. If the weight
#'     of A->B = 2 and the weight of B->A = 4, then 4 will be retained for both ties.}
#'    \item{"mean"}{Will retain the mean weight of two ties between node pairs. Note that NAs (i.e.
#'     missing ratings in the matrix) will first be replaced by the overall mean value of all ratings
#'     of the matrix. If the weight of A->B = 2 and the weight of B->A = 3, the mean value for
#'     the node pair A-B is (2+3)/2  2.5.}
#'    \item{"recip"}{Retains only ties which have a reciprocal weight, i.e. both involved parties
#'     gave the same rating to each other. If weight of A->B = 2 and B->A is 2, then 2 will be
#'     retained; otherwise the weight of the A-B pair will be replaced with 0}
#'    \item{"weight"}{Will retain the original scores. No transformation is applied.}
#'    \item{"prod"}{Multiplies scores of dyad. A->B=2 and B->A=5 produces 2x5=10}
#'    \item{"sum"}{Sums scores of dyad. A->B=2 and B->A=5 produces 2+5=7}
#'    \item{"diff"}{Absolute difference between two dyad scores. A->B=2 and B->A=5 produces
#'     abs(2-5)=3}
#'  }
#'  
#' The \code{dich.at} dichotomizes the matrix and sets all ties with \code{dich.at>=number} to 1 and 0 
#' otherwise. This maintains directionality of ties. 
#'
#' At the same time, \code{directed} controls if the resulting edge list maintains directed edges
#' or undirected. In the latter case, \code{\link{mreverse}} is applied to obtain an undirected
#' list of edges with their corresponding scores. A warning is issued if a undirected edge list is
#' requested but no method for \code{to.undir} specified. Similar, if a method other than "weight"
#' is specified for \code{to.undir}, which implies to collapse the directed to an undirected edge list,
#' a warning will be issued that the directionality of the weights will not be maintained.
#'
#' The \code{impute.na} parameter controls how NAs in round-robin matrix are imputed. It takes the following 
#' values: 
#' 
#' \describe{
#'   \item{"recip"}{In case a ego did not respond and there is no rating of alters, the NAs are 
#'    replaced with ratings of alter. This means we replaced all NAs in row i with the transpose
#'    of ratings given in col(i). In case two ratings are absent, NA remains.}
#'   \item{"recip_mean"}{Same as 'recip' with the difference that remaining NAs will be replaced with mean value 
#'    of all entries}
#'   \item{"mean"}{Replaces all NA entries with mean value of matrix}
#'   \item{"native"}{Depends on to.undir options set. For 'sum' and 'diff' will use 0 for NA replacement to have 
#'   no effect. For 'prod' will use 1 for NA replacement in order to have no effect.}
#'   \item{"without"}{Default value. Will not impute any NAs and thus does not modify matrix}
#' }
#' 
#'
#' The \code{as.type} allows to select between different return types, either an edgelist, a statnet
#' network object or a sociomatrix.
#' 
#' If \code{shuffle=T} changes the order of column and rownames at random. Can be used for 
#' manually constructing a QAP permutation test. In detail: retrieves existing colnames (rownames) from
#' matrix and draws equal size sample. The random ordered column (row) names are then re-assigned before 
#' ordering the matrix rownames and colnames, i.e. changing effectively the order of the column (row)
#' values.   
#'
#' @return tibble. In case an \code{directed=T}, a tibble with three columns: head, tail, edge, weight.
#'  In case \code{directed=F} a tibble with two columns only: edge, weight.
#'
#' @examples
#' #construct matrix
#' rrmat <- matrix(c(1,0,1,2,0,0,1,1,1,2,3,4,5,2,3,4), ncol=4)
#'
#' #retain only reciprocal ties of same weight
#' rr_rating(rrmat, directed=F, to.undir="recip")
#'
#' #retain min value of ties
#' rr_rating(rrmat, directed=F, to.undir="min")
#'
#' #retain "max" values of ties
#' rr_rating(rrmat, directed=F, to.undir="max")
#'
#' #dichotomize matrix at weight 3 or above
#' rr_rating(rrmat, directed=T, dich.at=3)
#'
#' #retain "mean" values
#' rr_rating(rrmat, directed=F, to.undir="mean")
#'
#' #impute NAs. replace row=2 of NAs with col=2 values
#' rrmat <- matrix(c(1,NA,1,2,0,NA,1,1,1,NA,0,4,5,NA,3,4), ncol=4)
#' rr_rating(rrmat, impute.na="recip", as.type="matrix")
#'
#'
#' @export
#'
rr_rating <- function(x, directed=T, dich.at=NULL, to.undir="weight", as.type="edgelist", impute.na="without", shuffle=F){

  if (!is.matrix(x)){
    stop("x requires to be of type matrix.")
  }


  # if a directed edge list is required, it makes no sense to merge values!
  if (directed == T & (to.undir %in% c("min", "max", "mean", "recip", "prod", "sum"))){
    warning("Using to.undir %in% c('min','max','mean','recip', 'prod', 'sum') produces identical weights for directed edges!")

    # if matrix is converted to undirected network (edge list), then the scores need to
    # treated/merged.
  } else if (directed == F & to.undir == "weight") {
    stop("Converting to undirected matrix requires to specify to.undir conversion method!")
  }
  
  

  # impute NAs
  if (impute.na == "mean") {

    mean_weight <- mean(x, na.rm=T)
    x[is.na(x)] <- mean_weight

  # absent ego, missing ratings of ego, but rated by all alters
  } else if (impute.na == "recip_mean"){

    # which rows are all NAs
    #ri <- which(rowSums(is.na(x)) == ncol(x), arr.ind=T)

    # use corresponding column of values to replace row
    #x[ri,] <- t(x[,ri])

    #manual replacement. 
    for (r in 1:nrow(x)){
      for (c in 1:ncol(x)) {
        val  <- x[r,c]
        tval <- x[c,r]
        
        if (is.na(val) & !is.na(tval)){
          x[r,c] <- tval
          
        } else if (!is.na(val) & is.na(tval) ){
          x[c,r] <- val  
          
        } 
      }
    }
    
    
    # in case of two or more people missing, replace by mean.
    mean_weight <- mean(x, na.rm=T)
    x[is.na(x)] <- mean_weight

    diag(x) <- 0

    # check if any column is NA values only, i.e. member who has not received any rating
    if (any(colSums(is.na(x)) == nrow(x))){
      warning("Round-robin rating with entire column of NA values!")
    }

  # use single existing entry for dyad globally. If both entries are missing, leave NA
  } else if (impute.na == "recip"){

    # which rows are all NAs
    #ri <- which(rowSums(is.na(x)) == ncol(x), arr.ind=T)

    # use corresponding column of values to replace row
    #x[ri,] <- t(x[,ri])
    
    #manual replacement. 
    for (r in 1:nrow(x)){
      for (c in 1:ncol(x)) {
        val  <- x[r,c]
        tval <- x[c,r]
        
        if (is.na(val) & !is.na(tval)){
          x[r,c] <- tval
          
        } else if (!is.na(val) & is.na(tval) ){
          x[c,r] <- val  
          
        } 
      }
    }

    
    diag(x) <- 0

    # check if entire columns are NA, i.e. member who has not received any rating
    if (any(colSums(is.na(x)) == nrow(x))){
      warning("Round-robin rating with entire column of NA values!")
    }


  # if no impute method is specified, use 0 for sum and diff, i.e. does
  # not affect result
  } else if (impute.na == "native" & to.undir %in% c("sum", "diff")) {

    warning("NAs of round-robin matrix imputed with 0!")

    x[is.na(x)] <- 0

  # replace NAs with 1, i.e. no effect when multiplied
  } else if (impute.na == "native" & to.undir %in% c("prod")) {

    warning("\nNAs of round-robin matrix imputed with 1!")

    x[is.na(x)] <- 1

  } else if (impute.na == "native") {
      
      warning("\nNo NAs imputed. Note that 'native' requires to.undir = sum | diff | prod \n ")

  } else if (impute.na == "without") {
      
      message("\nNo NAs imputed. \n")
      
  } else {
      warning("\nUnrecognized parameter for impute.na. No NAs imputed! \n")
  }


  # decide how to use weights, i.e collapse 
  if (directed == F & to.undir == "min"){
    
    # returns the pairwise minimum of edges
    # NAs caused by non-response of A are replaced with value of B rating A.
    x <- pmin(x, t(x), na.rm=T)
    
    # retain max value of value pair as weight
  } else if (directed == F & to.undir == "max"){

    x <- pmax(x, t(x), na.rm=T)

    # use mean value of value pair as weight
  } else if (directed == F & to.undir == "mean"){

    # calculate mean of weights between directed edge pairs
    x <- (x + t(x)) / 2

    # only retain ties with same weight
  } else if (directed == F & to.undir == "recip") {

    # which ties are reciprocal, i.e. have the same value?
    recipm <- (x == t(x))

    # if ties have same value us it, otherwise set to 0
    xvec <- dplyr::if_else(recipm, x, 0, missing=0)

    # reconstruct matrix
    x <- matrix(xvec, nrow=dim(x)[1], ncol=dim(x)[2], dimnames = dimnames(x))

    # retain rr score as is, including if to.undir="weight"
  } else if (directed == F & to.undir == "prod") {

    #x <- x

    # # replace NAs with 1, i.e. no effect when multiplied
    # rrmat[is.na(rrmat)] <- 1

    # calculate prod of weights between directed edge pairs
    x <- (x * t(x))

  } else if (directed == F & to.undir == "sum") {

    #rrmat <- x

    # calculate sum of weights between directed edge pairs
    x <- (x + t(x))


  } else if (directed == F & to.undir == "diff") {

    #rrmat <- x

    # calculate sum of weights between directed edge pairs
    x <- abs(x - t(x))

  }

  
  
  # use numeric value to dichotomize matrix to 1/0
  if (is.numeric(dich.at)){
    
    xvec <- dplyr::if_else(x[,] >= dich.at, 1, 0)
    
    x <- matrix(xvec, nrow=dim(x)[1], ncol=dim(x)[2], dimnames = dimnames(x))
    
  } else if (is.numeric(to.undir)){
      warning("\nto.undir=number is replaced by 'dich.by'\n")
  }
  
  diag(x) <- NA

  rrmat <- x
  
  colnames(rrmat) <- colnames(x)
  rownames(rrmat) <- rownames(x)
  
  # reshuffle rows and columns of matrix. Can be used for QAP
  if (shuffle){

      cname <- sample(colnames(rrmat), ncol(rrmat))
      rname <- sample(rownames(rrmat), nrow(rrmat))

      colnames(rrmat) <- cname
      rownames(rrmat) <- rname

      rrmat <- rrmat[order(rownames(rrmat)),order(colnames(rrmat))]
  }
  

  # Convert to edge list by constructing network object
  rrnet <- network::network(rrmat, directed=directed, ignore.eval=F, names.eval="weight")

  vnames <- rrnet %v% "vertex.names"
  
  # extract directed edge list
  el <- network::as.edgelist(rrnet, attrname="weight", output="tibble")
  
  #replace internal node ids (1..) with real vertex names
  el$.tail <- vnames[el$.tail]
  el$.head <- vnames[el$.head]


  # collapse to undirected edge list. We can assume that weights have been
  # treated with either min, max, mean (see above)!
  if (directed == F){

    # build unidirectional edge names
    el <- mreverse(el, cols=c(1,2), into="edge")

    # retain distinct ones. two edge entries should have same weight!
    el %<>%
        dplyr::select(edge, weight) %>%
        dplyr::distinct()

  # retain directed edge list
  } else if (directed == T){

    el %<>%
      dplyr::transmute(
                edge = paste0(.head, "-", .tail),
                weight = weight)
  }


  # which return type was requested
  if (as.type == "network"){
    return(rrnet)

  } else if (as.type == "matrix"){
    return(as.sociomatrix(rrnet, attrname="weight"))

  } else if (as.type == "edgelist"){
    return(el)

  } else {
    stop("Unknown return type specified. Needs to be 'network', 'matrix', or 'edgelist'")

  }

}



