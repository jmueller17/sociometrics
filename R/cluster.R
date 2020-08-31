# @title Cluster transitivity
#
# @description The algorithm helps to detect a "meeting", i.e. co-presence of badges even though not all badges
# detect all other badges.
#
# Badges are grouped together based upon transitivness, i.e. a_b, b_c, c_d all belong to the same cluster,
# since a and d are connected through b_c link. Usually \code{cluster.interaction.ts} has already been
# called on the dataset since original interaction data do not overlap. Once several badges have been
# grouped within a session, this method can look for badges that are shared between sessions
# and hence allow to merge sessions to a larger one.
#
# @param x A data.frame of proximity data
# @param clusterOffset Integer. Starting count for dyad clusters.
# @param by.col String. The name of the column indicating existing clusters (or sessions) which
# the algorithm will try to merge. If  \code{cluster.interaction.ts} has been run, this should be
# "clusterTS"
# @param max.r Integer Maxium recursion depth for clustering dyads.
#
# @return A data.frame with an additional column "clusterTrans"  running from clusterOffset + nr of clusters found.
#
# @seealso \code{\link{cluster_ids}}, \code{\link{cluster_ts}}
#
# @export
#cluster_transit <- function(x, clusterOffset=0, by.col="clusterTS", max.r = 10){
#  UseMethod("cluster_transit", x)
#}

# cluster_transit.default <- function(x){
#   return(x)
# }


# @title Cluster transitivity
#
# @description The algorithm helps to detect a "meeting", i.e. co-presence of badges even though not all badges
# detect all other badges. Only works for data of type "interaction".
#
# Badges are grouped together based upon transitivness, i.e. a_b, b_c, c_d all belong to the same cluster,
# since a and d are connected through b_c link. Usually \code{cluster.interaction.ts} has already been
# called on the dataset since original interaction data do not overlap. Once several badges have been
# grouped within a session, this method can look for badges that are shared between sessions
# and hence allow to merge sessions to a larger one.
#
# @inheritParams cluster_transit
#
# @return A data.frame with an additional column "clusterTrans"  running from clusterOffset + nr of clusters found.
#
# @seealso \code{\link{cluster_ids}}, \code{\link{cluster_ts}}
#
# @export
# cluster_transit.interact <- function(x, clusterOffset=0, by.col="clusterTS", max.r = 10){
#   if (nrow(x) == 0){
#     warning("Timestamp dataframe is empty!")
#     return(x)
#   }
#
#   if ( !(by.col %in% names(x)) ){
#     warning(paste("data frame has no '",by.col,"' column. Clustering over all badge dyads (!) or run cluster.interaction.ts() first."))
#   }
#
#   #create / reset clusterTrans
#   x$clusterTrans <- NA
#
#   clusterID <- clusterOffset
#
#   x$Rowid <- c(1:nrow(x))
#
#   #how many subgroups are there
#   subgroups <- unique(x[[by.col]])
#
#   for (i in 1:length(subgroups)) {
#     xs <- subset(x, clusterTS == subgroups[i])
#
#     clids <- list(c())
#
#     depth <- 1
#
#     j<-0
#     #for each row
#     while (j < nrow(xs)){
#       j <- j + 1
#
#       if (depth > max.r){
#         warning("max cluster depth reached")
#         break
#       }
#
#       curBID <- xs[j,"Badge.ID"]
#       curOID <- xs[j,"Other.ID"]
#
#       exists <- c()
#
#       #check if badge ids exist in any existing cluster
#       for (b in 1:length(clids)){
#         if (curBID %in% clids[[b]] | curOID %in% clids[[b]]){
#           exists <- append(exists, b)
#         }
#       }
#
#       #if it does not exist in any cluster create a new one
#       if (length(exists)==0){
#         clusterID <- clusterID + 1
#
#         clids[[clusterID]] <- c(curBID, curOID)
#         xs[j,"clusterTrans"] <- clusterID
#
#         #if it exists in a cluster, add it
#       } else if (length(exists)==1) {
#
#         clusterID <- exists[1] #in which cluster does it exist
#
#         #update the ids in the corresponding cluster
#         clids[[clusterID]] <- unique(c(clids[[clusterID]], curBID, curOID))
#         xs[j,"clusterTrans"] <- clusterID
#
#       } else if (length(exists)==2){
#
#         clusterID1 <-exists[1]
#         clusterID2 <-exists[2]
#
#         #make sure numbers are in the first
#         clids[[clusterID1]] <- unique(c(clids[[clusterID1]], curBID, curOID))
#
#         #make sure these numbers are not in the second
#         ids <- clids[[clusterID2]]
#         clids[[clusterID2]] <-  ids[! ids %in% c(curBID, curOID)]
#
#         depth <- depth + 1
#         j <- 0 #reset the loop
#       }
#
#     }
#
#     #copy clusters of this subgroup to x
#     for (i in 1:nrow(xs)){
#       x[x$Rowid == xs[i, "Rowid"],"clusterTrans"] <- xs[i,"clusterTrans"]
#     }
#   } #end for subgroups
#
#
#
#   message(paste((clusterID-clusterOffset+1), " dyad clusters over ",nrow(x)," rows created."))
#
#   x
# }




#' @title Cluster timestamps
#'
#' @description Cluster interactions data together as long as they fall into a starting ts + time window length.
#' Independent of badges. Assumes that x is ordered by timestamp (date) field in increasing order. This
#' is useful to group consecutive detects into one continuous session. The "twl" defines the separation
#' in terms of time that needs to pass between sequential detects in order to close one session and
#' start the next.
#'
#' The literature indicates a timewindow of 2min approx to count an ongoing interaction as one or two
#' separate interactions.
#'
#' @param x Dataframe of interaction data.
#' @param twl Integer indicating the length of the time window in seconds within which detects fall into the same
#' cluster. If two detects are further apart in time than twl, a new cluster is created.
#' @param force.cut String, one of c("none", "twl", "nrows", "ncluster").
#' Default is "none", meaning that cluster reflects timestamp data overlaps, i.e. the session end time
#' expands with each new detect that falls within the time window.
#' If set to "twl"  the clusters will be enforced to be of equal duration of twl seconds, independently
#' if subsequent ts detects fall into the same time window.
#' If set to "nrows", twl will be interpreted  as number of rows to be included in each cluster.
#' If set to "ncluster", twl will be read as the total number of clusters to be created.
#' @param clusterOffset Integer. Starting number of the cluster count
#'
#' @return A data.frame. The original dataframe plus a clusterTS column indicating the group into which the current ts falls
#'
#' @seealso \code{\link{cluster_ids}}
#'
#' @export
cluster_ts <- function(x, twl=120, force.cut="none",  clusterOffset=1){
  UseMethod("cluster_ts", x)
}

cluster_ts.default <- function(x, twl=120, force.cut="none",  clusterOffset=1){
  return(x)
}


#' @describeIn cluster_ts Cluster timestamps of smtrx data
#'
#' @export
cluster_ts.smtrx <- function(x, twl=120, force.cut="none",  clusterOffset=1){

  if (nrow(x) == 0){
    warning("Timestamp dataframe is empty!")
    return(x)
  }

  #create/reset cluster ids column in x
  x$clusterTS <- as.double(NA)

  #counter of current cluster
  clusterID <- clusterOffset


  startS <- x[1,"Timestamp"]
  endS <- startS + twl
  x[1,]$clusterTS <- clusterID

  #x has just one entry
  if (nrow(x) == 1){
    return(x)
  }

  if (force.cut=="ncluster"){
    twl <- floor(nrow(x)/twl)
  }

  rowcount <- 1

  #aggregate timestamp to session while inside the time window
  for (i in 2:nrow(x)){

    #falls into the current session time window and cut is not enforced
    if (x[i,"Timestamp"] <= endS & force.cut=="none" ){
      #extend the time window
      endS <- x[i,"Timestamp"] + twl

      #fall into current session window and cut is enforced.
    } else if (x[i,"Timestamp"] <= endS & force.cut=="twl"){


      #cluster by number of rows independent of time  or by number of overall clusters
    } else if (rowcount <= twl & (force.cut=="nrows" | force.cut=="ncluster")){
      rowcount <- rowcount+1

      #otherwise save the current session and start a new one
    } else {
      clusterID <- clusterID +1
      startS <- x[i,"Timestamp"]
      endS <- startS + twl
      rowcount <- 1
    }

    #assign cluster ID to current row
    x[i,]$clusterTS <- clusterID
  }
  message(paste((clusterID-clusterOffset+1), " timestamp clusters over ",nrow(x)," rows created."))

  x$clusterTS <- as.factor(x$clusterTS)

  x
}



#' @title Clusters consecutive ids
#'
#' @description Cluster timestamped rows into clusters based on Badge.ID column sequence. Simplest case is to
#' create a cluster while badge.IDs of consecutive rows are the same. If Badge.ID from row n to
#' row n+1 changes, the n+1 row falls into new cluster.
#'
#' @param x Dataframe with Badge.ID column
#' @param clusterOffSet Numeric start of cluster ID number
#'
#' @return Modified dataframe with additional column "clusterIDSeq".
#'
#' @seealso \code{\link{cluster_ts}}
#'
#' @export
cluster_ids <- function(x, clusterOffSet=1){
  UseMethod("cluster_ids", x)
}

cluster_ids.default <- function(x, clusterOffSet=1){
  return(x)
}


#' @describeIn cluster_ids Cluster badge IDs
#'
#' @export
cluster_ids.smtrx <- function(x, clusterOffSet=1){


  lastID = x[1,"Badge.ID"]

  #starting number of cluster
  cluster=clusterOffSet

  #assign start
  x$clusterIDSeq <- NULL;
  x[1,"clusterIDSeq"] <- cluster


  for (i in 2:nrow(x)){

    currentID <- x[i, "Badge.ID"]

    #create new cluster when badge ID changes from one row to the next
    if (currentID != lastID){
      cluster <- cluster +1
      lastID <- currentID
    }

    x[i,"clusterIDSeq"] <- cluster
  }

  x$clusterIDSeq <- as.factor(x$clusterIDSeq)

  message(paste((cluster-clusterOffSet+1), " badge clusters over ",nrow(x)," rows created."))

  x
}














