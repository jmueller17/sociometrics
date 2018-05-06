#' @title Convert covariate factors to logical vector
#'
#' @description Converts a factor (or character) vector into a logical vector (or matrix)
#'  where the reference category is set to \code{TRUE} and all other levels to \code{FALSE}. The
#'  resulting logical vector (or matrix) is used as covariate parameter in the \code{relevent::rem.dyad()}
#'  function.
#'
#'  There are two modes: either retrieves a logical vector or a n x n difference/similarity matix of
#'  node attributes.
#'
#' @param df_covars Dataframe or matrix which contains covariate information. This can either be
#'  a dataframe where columns specify sociodemographic variables such as age, gender, etc.
#'  or it can be a matrix which specifies co-location.
#' @param ids Filter for specific ids in df_covars. If \code{ids=NULL}, all
#'  entries of the \code{df_covars} will be retrieved. If \code{idcol=NULL}, assumes that ids match
#'  row index; otherwise extra column with ids has to be specified in param \code{idcol}.
#' @param covar_v Vector of length two. First entry specifies name of attribute to retrieve.
#'  This has to correspond to the column name in df_covars. Second entry specifies reference value set
#'  to \code{TRUE}. If only first entry is provided original attribute values are
#'  returned and not a logical vector.
#' @param covar_m Vector of length two. First entry specifies name of the attribute to
#'  retrieve. Second entry specifies difference function, usually either "==" for binary values
#'  like gender or "-" difference for numeric values. Returns n x n matrix of difference between
#'  data attributes.
#' @param covar_t String. Either of c("attr" | "co.loc" | "rr"). Indicating type of covariate data.
#'  Usually this is either a vector or a matrix of data attributes. However, there are further two
#'  special types, namely the colocation matrix (covar_t="co.loc") and the round robin formats (covar_t="rr").
#'  Default value is "attr".
#' @param intercept Logical. If TRUE, returns logical/matrix with all entries set to T.
#' @param idcol Name or index of the column which should be used to filter for \code{ids}. Default
#'  is \code{NULL} meaning that provided \code{ids} correspond to the row index.
#'
#' @return either logical vector or data matrix for given attribute containing difference between two node values.
#'
#' @details The order of the values for the covariate vector will be identical to the order in the
#'  df_covar dataframe. It is important that the IDs of the nodes has an ascending order since
#'  \code{relevent::rem.dyad()} expects its nodes to be ordered from 1..n in ascending order as well.
#'  When converting a sociometric data frame to an edge list to be used with relevent package, the
#'  default behavior takes care that original values are replaced in ascending order starting with
#'  1..n.
#'
#' @seealso \code{rem_edge_list},
#'
#'
#' @examples
#'
#' #Retrieve a numeric vector of the column "Tenure" in dataframe df.attr for badge ids 2,5,7,9
#' tenure <- remcovars(df.attr, ids=c(2,5,7,9), covar_v=c("Tenure"))
#'
#' #Retrieve a logical vector of "Role" Column, now with "Team leader" as reference category and set to TRUE
#' isLeader <-  remcovars(df.attr, ids=ub, covar_v=c("Role", "Team leader"))
#'
#' #Retrieve a n x n matrix of Gender (where n depends on the length of badge IDs in "ub") of "Gender", indicating
#' #TRUE if two badges have the same "Gender" or FALSE otherwise.
#' sameSex <- remcovars(df.attr, ids=ub, covar_m=c("Gender", "=="))
#'
#' #Retrieve the three round robin matrix for given badges, where NAs are replaced by column mean.
#' remcovars(rr_csx[[team]], ids=ub, na.replace="mean", covar_t="rr")
#'
#' @return Matrix
#'
#' @export
rem_covars <- function(df_covars, ids=NULL, covar_v=NULL,
                       covar_m=NULL, covar_t="attr", intercept=F, idcol=NULL, ...){

  #retrieve attributes only for specific badges
  if (!is.null(ids) & length(ids)>0 & is.data.frame(df_covars)){

    #use row index or provided extra id column.
    if (is.null(idcol)){

      #badge ids can be bigger than the actual rows in the covar data frame. In this case
      #an extra column with row ids needs to be provided
      if (max(ids) > nrow(df_covars)){
        stop("\nRow index out of bounds: ids max value > than number of rows in data frame.")
      }

      df_covars <- df_covars[which(rownames(df_covars) %in% ids ),]
      order_ids <- rownames(df_covars)

    #specific badge id column
    } else {
      df_covars <- df_covars[which(df_covars[[idcol]] %in% ids ), ]
      order_ids <- df_covars[[idcol]]
    }

    if (nrow(df_covars)==0){
      warning("\nProvided badge IDs have no match in attribute dataframe. Returning empty covariate vector/matrix!")
    }

    order_ids <- as.numeric(order_ids)
    inc_order <- sort(order_ids, decreasing=F)

    if (sum(abs(order_ids-inc_order))>0){
      warning("\nCovariate entries do not have increasing order!")
    }


  } #otherwise take entire df_covar dataframe.


  #retrieve logical vector from the dataframe of attributes
  if (!is.null(covar_v) & is.data.frame(df_covars) & covar_t=="attr"){

    #if just the attribute name is given, retrieve plain values
    if (length(covar_v)==1){
      v <- df_covars[,covar_v[1]]
      v <- v[[covar_v[1]]]

      #if attribute name and reference value is given, return logical vector
      #set to true if attribute matches reference value
    } else if (length(covar_v==2)) {
      v <- (df_covars[,covar_v[1]] == covar_v[2])

    } else {
      stop("\nWrong number of arguments in covar_v.")
    }

    if (sum(is.na(v))){
      warning("\nCovariate vector contains NAs!")
    }

  }

  #create a difference matrix. Does not preserve RID values but numbers 1...n
  if (!is.null(covar_m) & is.data.frame(df_covars) & covar_t=="attr"){
    #extract attribute vector from data frame
    v <- ( df_covars[,covar_m[1]] )

    #simplify it. outer() does not work with tibble
    v <- v[[covar_m[1]]]

    #generate difference matrix
    v <- outer(v,v, FUN=covar_m[2])

  }


  #get the co-location matrix in shape
  if (covar_t=="co.loc" & is.matrix(df_covars)){

    #use entire df_covars, which in this case is a matrix of the colocation data.
    if (is.null(ids)){
      v <- df_covars

    } else if (length(ids) > 0) {

      #make sure that listing of badges is ordered
      ids <- sort(ids, decreasing=F)

      #convert to character since number of matrix (dimnames) does not necessarily start at 1.
      ids <- as.character(ids)

      #only retain those cols/rows that are specified in used ids
      v <- df_covars[ids,ids]

    } else {
      warning("\nNo ids specified!")
    }

    d <- dim(v)
    #change col/row names to start from 1..n, although does not make a difference for rem()
    dimnames(v) <- list(c(1:d[1]), c(1:d[2]))

  }

  #retieve round roubin format.
  if (covar_t == "rr" & is.matrix(df_covars)){

    v <- graph_from_round_robin(df_covars, ids=ids,...)

  }


  if (intercept){
    v[] <- 1
  }

  v

}



#' @title Format timestamped edge list for REM
#'
#' @description Convert a timestamped data frame into an edge list to be used with
#' relevent rem.dyad() which requires a three column matrix: timestamp, head, tail.
#' Timestamp data have to start near "0". Badge ids will be retrieved in ascending order and reset
#' to start from 1..n. It is important that the order of entries in covariate vectors retrieved
#' with \code{rem_covars} has the same ascending order.
#'
#' @param df The sociometric dataframe, containing at least the Timestamp, Badge.ID, Other.ID columns
#' @param rm_sup Logical. Remove duplicated values from proximity/face-to-face data frame where timestamps are identical
#' @param replv Logical. Make sure that badge ids in dataframe start at 1...ns
#' @param use_seq Logical. If Other.ID is missing, simulate Other.ID with Badge.ID of next row
#'
#' @return dataframe / edgelist with three columns only. Numeric Timestamp, head and tail vertex ids of
#' corresponding edges.
#'
#' @export
rem_edge_list <- function(df, rm_sup=T, replv=T, use_seq=F){

  #renumber badges since IDs need to start with 1..n
  #this needs to be in increasing order unless the order of the covariate vector differs in its
  #ordering.
  if(replv){
    df <- anonymize(df, ids=NULL, replv=NULL, decreasing=F)
  }

  #remove duplicates or "near" duplicates
  if (rm_sup) {
    df <- df[!duplicated(df$Timestamp),]
  }


  #if data is audio speaking sequence of badges, Other.ID is missing
  if (use_seq){

    #Other.ID column is missing.
    df$Other.ID <- NA

    otherid <- df[1,"Badge.ID"] #5

    #simulate that Badge.ID is talking to Other.ID, i.e. the person who responds
    #which is given by the badge in the next cluster.
    for (i in 1:(nrow(df)-1)){
      #df[which(df$clusterIDSeq==clseq[i]), "Other.ID"] <- df[which(df$clusterIDSeq==clseq[i+1]),]$Badge.ID[1]

      df[i,"Other.ID"] <- otherid

      if (df[i+1,"Badge.ID"] != df[i, "Badge.ID"]){
        otherid <- df[i,"Badge.ID"]
      }


    }
  }

  #retain only three columns needed for relevent edgelist
  el <- select(df, Timestamp, Badge.ID, Other.ID)

  #convert to double
  el$Timestamp <- as.double(el$Timestamp)

  #first event needs to be near null-event, i.e. time 0 (see page 159 of Butss 2008)
  el$Timestamp <- el$Timestamp - (min(el$Timestamp)-1)

  el$Badge.ID <- as.numeric(el$Badge.ID)
  el$Other.ID <- as.numeric(el$Other.ID)

  el
}


# Converts a vertex attribute to a matrix. If attribute is numeric, the resulting matrix
# will contain the difference of each node with each other node. If the attribute is a
# character, the resulting matrix will contain TRUE if attributes match and FALSE if no.
#
# @param igraph. An igraph object.
# @param attr_name string. The name of the vertex attribute to convert to matrix
#
# @return matrix
#
vertex_attr_to_matrix <- function(igraph, attr_name){

  if (is.character(vertex_attr(igraph, attr_name)) & !is.null(vertex_attr(igraph, attr_name) )){
    m <-outer(vertex_attr(igraph, attr_name), vertex_attr(igraph, attr_name), FUN="==")

  } else if (is.numeric( vertex_attr(igraph, attr_name)) & !is.null(vertex_attr(igraph, attr_name))){
    m <-outer(vertex_attr(igraph, attr_name), vertex_attr(igraph, attr_name), FUN="-")
  }

  if (is.null(vertex_attr(igraph, attr_name))){
    stop("Data attribute does not exist or has length 0")
  }

  m
}
