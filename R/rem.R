#' @importFrom sjmisc dicho
#'
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
#'  returned and not a logical vector. Returns logical *vector*
#' @param covar_m Vector of length two or three. First entry specifies name of the attribute to
#'  retrieve (e.g. gender). Second entry specifies comparison to be applied such as usually comparison
#'  operators "==", "!=" or operators for numeric values such as "-|+".
#'  Returns n x n *matrix*.
#' @param covar_t String. Either of c("attr" | "co.loc" | "rr"). Indicating type of covariate data.
#'  Usually this is either a vector or a matrix of data attributes. However, there are further two
#'  special types, namely the colocation matrix (covar_t="co.loc") and the round robin formats
#'  (covar_t="rr"). Default value is "attr".
#' @param idcol Name or index of the column which should be used to filter for \code{ids}. Default
#'  is \code{NULL} meaning that provided \code{ids} correspond to the row index.
#'
#' @return either logical vector or data matrix for given attribute containing difference between two
#'  node values.
#'
#' @details The order of the values for the covariate vector will be identical to the order in the
#'  df_covar dataframe. It is important that the IDs of the nodes has an ascending order since
#'  \code{\link[relevent]{rem.dyad}} expects its nodes to be ordered from 1..n in ascending order as well.
#'  When converting a sociometric data frame to an edge list to be used with relevent package, the
#'  default behavior takes care that original values are replaced in ascending order starting with
#'  1..n.
#'
#'  \code{covar_m} retrieves a (difference) matrix from covariate attributes. \code{covar_m=c("Gender", "==", "Men")}
#'  for example retrieves a matrix where men-men dyad are set to 1 and all other combinations of
#'  dyads is set to 0. The same code without a reference category specified \code{covar_m=c("Gender", "==")}
#'  returns a matrix where homophilios ties (men-men or women-women) is set to 1 and all others 0.
#'
#' @seealso \code{rem_edge_list},
#'
#'
#' @examples
#'
#' #Retrieve a numeric vector of the column "Tenure" in dataframe df.attr for badge ids 2,5,7,9
#' tenure <- rem_covars(df.attr, ids=c(2,5,7,9), covar_v=c("Tenure"))
#'
#' #Retrieve a logical vector of "Role" Column, now with "Team leader" as reference category and set to TRUE
#' isLeader <-  rem_covars(df.attr, ids=ub, covar_v=c("Role", "Team leader"))
#'
#' #Retrieve a n x n matrix of Gender (where n depends on the length of badge IDs in "ub") of "Gender", indicating
#' #TRUE if two badges have the same "Gender" or FALSE otherwise.
#' sameSex <- rem_covars(df.attr, ids=ub, covar_m=c("Gender", "=="))
#'
#' #Retrieve a n x n matrix of Gender where men-men ties are set to 1 and all others to 0
#' rem_covars(df.attr, covar_m=c("Gender", "==", "Men"))
#'
#' #Retrieve difference matrix of factor roles; if Role is factor, uses underlying numeric
#' #representation to construct difference.
#' rem_covars(df.attr, covar_m=c("Role", "-"))
#'
#' @return Matrix
#'
#' @export
rem_covars <- function(df_covars, ids=NULL, covar_v=NULL,
                       covar_m=NULL, covar_t="attr", idcol=NULL, ...){

  #retrieve attributes only for specific badges
  if (!is.null(ids) & length(ids)>0 & is.data.frame(df_covars)){

    #use row index or provided extra id column.
    if (is.null(idcol)){

      #badge ids can be bigger than the actual rows in the covar data frame. In this case
      #an extra column with row ids needs to be provided
      if (max(ids) > nrow(df_covars)){
        stop("\nRow index out of bounds. Try specifying 'idcol' column to match provided ids!")
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

    col <- covar_m[1]
    oper <- covar_m[2]
    attr <- covar_m[3]

    #extract attribute vector from data frame
    v <- (df_covars[,col])

    #simplify it. outer() does not work with tibble
    v <- v[[col]]

    # if the second covar is a math symbol, we can feed it to the outer() funciton
    if (oper %in% c("+", "-", "*", "==", "/", "^", "!=") & is.na(attr)){

      if (is.factor(v)){
        warning("Applying difference function to factor!")
      }

      # generate difference matrix
      # in case v is factor, uses underlying numeric representation to generate difference
      v <- outer(as.numeric(v),as.numeric(v), FUN=covar_m[2])

    # use the attribute for which we want to construct the matrix
    } else if (!is.na(attr)) {

      # set the selected attribute to true
      v <- do.call(oper, args=list(v, attr))

      v[which(v == T)] <- 1

      # construct it as matrix
      mv <- matrix(rep(v, length(v)), ncol=length(v), byrow=T)

      #generate difference matrix
      mv <- mv + t(mv)

      #dichotomize matrix. If cell sum is 2, means attribute is the same
      # set to 1, otherwise 0
      mv[,] <- if_else(mv == 2, 1, 0)

      v <- mv

      d <- dim(v)
      dimnames(v) <- list(c(1:d[1]), c(1:d[2]))


    }

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

  #retieve round robin format.
  if (covar_t == "rr" & is.matrix(df_covars)){

    v <- get_rr(df_covars, ids=ids) #extract matrix from round robin gedii_rr dataset.

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
#' @param rm_sup Logical. Remove duplicated values from proximity/face-to-face data frame where timestamps
#'  are identical
#' @param replv Logical. Make sure that badge ids in dataframe start at 1...ns
#' @param use_seq Logical. If Other.ID is missing, simulate Other.ID with Badge.ID of next row
#'
#' @return dataframe / edgelist with three columns only. Numeric Timestamp, head and tail vertex ids of
#' corresponding edges.
#'
#' @export
rem_edge_list <- function(df, rm_sup=T, replv=T, use_seq=F, ...){

  #renumber badges since IDs need to start with 1..n
  #this needs to be in increasing order unless the order of the covariate vector differs in its
  #ordering.
  if(replv){
    df <- anonymize(df, ids=NULL, replv=NULL, decreasing=F, ...)
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




#' @title Format edge list for DyNAM
#'
#' @description Convert a dyadic event list into an edge list to be used with the goldfish package
#'  (DyNAM) model. The input data contains sender and receiver and is timestamped. The DyNAM model
#'  requires certain naming conventions and data type formats, to be delivered by this function.
#'
#' @param x Dyadic data. Accepted formas are "matrix" (adjacency), "network" or "edgelist" objects
#' (sna package), "interact" data frame.
#' @param twl Numeric. Indicates the time window length to cluster (merge) interactions together.
#'  This is passed to \code{twl} of \code{cluster_ts}.
#' @param vprefix String prefix for labeling vertices. This is useful in relation to matrix where col
#'  or row names might be just numbers while vertices are names.
#' @param vpostfix String postfix for labeling vertices.
#' @param use.lables Logical. In case a network/edgelist object is used, indicates ifthe associated
#' vertice names should be copied or not.
#' @param time POSIXct time object for event list.
#'
#' @return Object of type data.frame to be used with the DyNAM model.
#'
#' @examples
#' #Construct DyNAM edge list from "advice" network matrix
#' adv <- matrix(c(0,0,1, 1,0,0, 1,1,1), nrow=3, ncol=3)
#'
#' #without names
#' dynam_event_dyad(adv)
#'
#' #with node prefix
#' dynam_event_dyad(adv, vprefix="Node_")
#'
#' @export
dynam_event_dyad <- function(x, twl=0, vprefix=NULL, vpostfix=NULL, use.labels=F, time=1, ...){

  el <- NULL
  gfish_el <- NULL


  #convert from adjacency
  if ("matrix" %in% class(x)){

    if (sum(x, na.rm=T) == 0){
      warning("\nNo edges in adjacency matrix!")

      gfish_el <- data.frame(time=as.POSIXct(character()),
                             sender=character(),
                             receiver=character(),
                             stringsAsFactors = F)

    } else {

      net <- network::network(x, matrix.type="adjacency", ...)
      el <- network::as.edgelist(net)

    }

  } else if ("network" %in% class(x)){
    el <- network::as.edgelist(x)

  } else if ("edgelist" %in% class(x)){
    el <- x

  } else if ("interact" %in% class(x)){

    #should interactions be clustered together?
    if (twl > 0){
      x %<>%
        mreverse() %>%
        cluster_ts(twl=twl)


      gfish_el <- x %>%
        group_by(Dyad, clusterTS) %>%
        summarize(start = min(Timestamp),
                  end = max(Timestamp),
                  detects = n(),
                  team = Team[1])%>%
        separate(Dyad, into=c("sender", "receiver"), sep="-") %>%
        arrange(clusterTS)%>%
        select(time=start, sender, receiver, end, detects, clusterTS, team) %>%
        mutate(time = as.POSIXct(time),
               sender = paste0(vprefix,sender, vpostfix),
               receiver = paste0(vprefix,receiver, vpostfix))


    } else {


      names(x) <- c("time", "sender", "receiver", "rssi", "source", "team")

      gfish_el <- x %>%
        select(time, sender, receiver) %>%
        mutate(time = as.POSIXct(time),
               sender = paste0(vprefix,sender, vpostfix),
               receiver = paste0(vprefix,receiver, vpostfix))
    }


    gfish_el <- as.data.frame(gfish_el)
    gfish_el <- gfish_el[order(gfish_el$time),]

  } else {
    stop("Unrecognized input. should be adjacency matrix, network object or edgelist, or smtrx interact.")
  }



  # use edge labels or indices
  if (!is.null(el) & use.labels){
    vnames <- attr(el, "vnames")

    sname <- paste0(vprefix,vnames[el[,1]], vpostfix)
    rname <- paste0(vprefix,vnames[el[,2]], vpostfix)

  } else {
    sname <- paste0(vprefix,el[,1], vpostfix)
    rname <- paste0(vprefix,el[,2], vpostfix)
  }


  # if we are coming from other than "smtrx" "interact" object
  if (is.null(gfish_el)){
    gfish_el <- data.frame(time=time,
                           sender=sname,
                           receiver=rname,
                           stringsAsFactors = F)
  }


  gfish_el

}




#' @title Control presence of nodes for DyNAM
#'
#' @description Event sequences can be associated with ego-centered attributes in DyNAM. This is a
#'  utility function to retrieve the start and end timestamps of actors to construct their presence/absence
#'  event list by combining the body activity and interaction data frames.
#'
#' @param bm Data frame with sociometric body activity
#' @param interact Data frame with sociometric interaction data
#' @param vprefix String prefix for labeling vertices. This is useful in relation to matrix where col
#'  or row names might be just numbers while vertices used in DyNAM are full names
#' @param vpostfix String postfix for labeling vertices.
#' @param fixtime Logical. Creates for each \code{ids} an artificial time slot entry at the minimum time
#'  value minus 5 minutes and sets it to \code{present=T} and \code{present=F} 1 minute after. This new time slot can be used for attaching
#'  covariate matrix whose actors need to be present when linking the Dynamic Network Actors with
#'  change events.
#' @param ids Vector of ids. Used together with \code{fixtime}. Indicates the ids for which a artificial time slot
#'  entry will be created. This should include all badge ids for given group.
#' @param window Numeric. The time that gets added or removed from the events.
#'
#' @details This is a utility function for preparing Sociometric (Badges) data for use with DyNAM models. In
#'  particular it helps controlling the presence and absence of badges. By indicating when a given badge
#'  was present (switched on) but not involved in interactions versus simply absent (badge turned off) a
#'  more precise estimate of DyNAM rates/choice should be possible. This is also interesting for data
#'  that spans several days. By indicating when people come to work in the morning and leave work at night
#'  the time people are not at work and therefore can't interact is taken into account.
#'
#'  We combine the information of body activity readings with interaction detects. Body activity readings
#'  are a good indicator of when badges are switched on; however, in some cases badges body activity
#'  readings malfunction, i.e. there is no body activity reading although we have interaction detects.
#'  Therefore, the overall
#'  attendance of people is derived from the combination of the body activity dataset and interaction dataset
#'  for a given day. We construct a dataframe indicating the timestamp when people clock in (switch on) the
#'  badge in the morning and leave (switch off) later on (in the evening). In case the body activity data
#'  is missing, we take the first and last occurance of interaction for the given day as starting and
#'  ending timestamp.
#'
#'  The parameters `ids` and `fixtime` should be used together. In order to be able to add events and
#'  nodes in the DyNAM model together, we need to make sure that all nodes exist at the start of the
#'  timespan used in the model. Therefore we take the earlierst timestamp of the dataset minus 5 minutes
#'  and artificially indicate that all badges specified with `ids` are present. After one minute, we set
#'  `presence=F` in order to start with the real presence/absence of badges. Otherwise, DyNAM linkEvents
#'  throws an error for events whose corresponding nodes are not present.
#'
#'  The `window` parameter specifies the time added (or removed) to the events. Be default this is 1
#'  since we take the time that the last interaction occurs at time X and set the presence of the badge
#'  for that time to FALSE. This will produce an error since DyNAM expects this badge to be present
#'  when an interaction occurs. This s resolved by simply adding the default window effect of 1 to the last
#'  interaction (or removeing 1 for the first interaction). When window effects are used in the DyNAM effects
#'  the window should be set to the same size.
#'
#'
#' @return A data frame containing time, node, replace columns.
#'
#' @export
dynam_event_presence <- function(bm, interact, vprefix=NULL, vpostfix=NULL, fixtime=F, ids=NULL, window=1){

  df = NULL

  if ( !("ego" %in% class(bm)) ){
    stop("Need ego centered sociometric format for 'bm' ")
  }

  if (!("interact" %in% class(interact))){
    stop("Need interaction centered sociometric format for 'act' ")
  }


  # extracts the timestamp when each badge has been switched on and off. This gives the interval
  # when interaction detects are theoretically possible. If there is no bm activity for the given day
  # it means the badge was not switched on and hence the person probably absent this day (or
  # specific hours)

  df.bm <- bm %>%
    dplyr::mutate(day = lubridate::day(Timestamp)) %>%
    dplyr::group_by(day, Badge.ID) %>%
    dplyr::summarize(start=min(Timestamp), end=max(Timestamp)) %>%
    tidyr::gather(key="what", value="time", start, end) %>%
    dplyr::mutate(node=paste0(vprefix,Badge.ID,vpostfix), replace=if_else(what=="start", TRUE, FALSE))%>%
    dplyr::select(time.bm=time, node, replace, day) %>%
    dplyr::arrange(time.bm)


  df.in <- interact %>%
    dplyr::mutate(day = lubridate::day(Timestamp)) %>%
    tidyr::gather(key="SrcBadge", value="BadgeID", Badge.ID, Other.ID) %>%
    dplyr::group_by(day, BadgeID) %>%
    dplyr::summarize(start=min(Timestamp), end=max(Timestamp)) %>%
    tidyr::gather(key="what", value="time", start, end) %>%
    dplyr::mutate(node=paste0(vprefix,BadgeID,vpostfix), replace=if_else(what=="start", TRUE, FALSE))%>%
    dplyr::select(time.in=time, node, replace, day) %>%
    dplyr::arrange(time.in)


  # join both data frames
  # replace NA with existing time value
  # use min time for replace=T (starting time) and max time for replace=F (leaving time)
  df.all <- df.bm %>%
    full_join(df.in, by=c("node", "replace", "day")) %>%
    mutate(time.in = if_else(is.na(time.in), time.bm, time.in),
           time.bm = if_else(is.na(time.bm), time.in, time.bm),
           time = if_else(replace, if_else(time.bm < time.in, time.bm, time.in),       #start time
                                   if_else(time.bm > time.in, time.bm, time.in))) %>%  #end time
    select(time, node, replace, day)


  # there shouldn't be any NAs in the table
  if (anyNA(df.all)){
    warning("NAs detected in event list?! Every badge should have start and end timestamps!")
  }


  # insert window effect
  # in case the last timestamp of a badge (when presence=F) is an interaction, we need to set the
  # presence=F after the interaction has occured, by adding 1 to it.
  df.all <- df.all %>%
    mutate(time = if_else(replace, time-window, time+window))


  # convert to plain data frame for DyNAM
  df <- data.frame(time=as.POSIXct(df.all$time),
                   node=as.character(df.all$node),
                   replace=df.all$replace,
                   stringsAsFactors = F)

  rm(df.all, df.bm, df.in)

  if (fixtime & is.null(ids)){
    stop("'fixtime=T' but ids missing. Specify all ids for which initial presence needs to be defined.")
  }

  # create an artificial starting time for all badges in order to avoid error messages when
  # linking events in DyNAM. All badges are present at an artificial timepoint in order to add
  # events and then immediately set to "absent".
  if (fixtime){

    #get min time
    init_moment <- min(df$time)

    #choose arbitrary start date to set all ids to "present=T"
    #for easily adding covariate networks
    init_moment <- init_moment - window - 60

    #and reset presence to "absent" a little later.
    init_end <- init_moment + window + 59

    nodes <- paste0(vprefix, ids, vpostfix)

    #create a fixed time slot where each id is "present"
    start_df <- data.frame(time=init_moment,
                           node=nodes,
                           replace=rep(TRUE, length(nodes)),
                           stringsAsFactors = F)

    #immediate reset if afterwards to present=false.
    end_df <- data.frame(time=init_end,
                         node=nodes,
                         replace=rep(FALSE, length(nodes)),
                         stringsAsFactors = F)

    df <- rbind(df, start_df)
    df <- rbind(df, end_df)

  }

  df <- df[order(df$time),]

  df
}




#' @title Format nodes for DyNAM
#'
#' @description Utility function to format data frame of nodes for use with DyNAM.
#'
#' @param x Data frame of actors or nodes with arbitrary covariates
#' @param vprefix Node prefix to be pasted into label column
#' @param vpostfix Node postfix to be pasted into label column
#' @param present Logical. Indicates if nodes are present/absent at start of observation
#'
#' @return data frame ready to be used with DyNAM models.
#'
#' @export
dynam_nodes <- function(x, vprefix=NULL, vpostfix=NULL, present=FALSE){

  names(x)[1] <- "label"
  x$label <- paste0(vprefix, x$label, vpostfix)
  x$present <- present

  #convert factors to logical
  x <- dplyr::mutate_if(x, is.factor, as.character)

  x <- as.data.frame(x)

  x
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



# Extracts round-robin data from raw result matrix as stored in gedii_rr dataset
# This function is not exported and for internal use only. Usually called from rem_covars()
# dichotomization and imputation of values is now done by rr_ratings().
#
get_rr <- function(rr_x, ids=NULL){


  #extract social, advice, and ease matrix. rr_csx[[teamID]] contains all three matrix in one
  #where bound by columns, i.e. horizontally one after the other.
  df <- as.data.frame(rr_x)

  snet <- select(df, contains("social"))
  anet <- select(df, contains("advice"))
  enet <- select(df, contains("ease"))

  #make row and column names equal. Column names have format "advice_RID"; Since we have the
  #three matrix separated, each column name only contains the RID now.

  # check if matrix is emtpy. ease ratings are missing from public gediismtrx data
  if (length(snet)>0){
    row.names(snet) <- df$RID
    names(snet) <- df$RID
    snet <- subset_rr(snet, ids)

    #convert to matrix
    snet <- as.matrix(snet)

  } else {
    snet <- NULL
  }


  if (length(anet) > 0){
    row.names(anet) <- df$RID
    names(anet) <- df$RID
    anet <- subset_rr(anet, ids)
    anet <- as.matrix(anet)

  } else {
    anet <- NULL
  }


  if (length(enet) > 0){
    row.names(enet) <- df$RID
    names(enet) <- df$RID
    enet <- subset_rr(enet, ids)
    enet <- as.matrix(enet)

  } else {
    enet <- NULL
  }


  list(m_soc=snet,m_adv=anet,m_eas=enet)
}


# custom function to subset round robing ratings.
subset_rr <- function(x, ids){

  #if a vector of RID/ids have been provided, we subset the matrix.
  #This is useful for constructing REM covariate matrix
  if (!is.null(ids) & length(ids) > 0) {

    #make sure that listing of badges is ordered
    ids <- sort(ids, decreasing=F)

    #convert to character since number of matrix (dimnames) does not necessarily start at 1.
    ids <- as.character(ids)

    #only retain those cols/rows that are specified in used ids
    x <- x[ids,ids]

  }

  x
}



