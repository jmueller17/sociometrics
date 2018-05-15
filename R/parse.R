#' @importFrom lubridate is.POSIXct
#' @importFrom lubridate force_tz
#'
#' @import stringr
#' @import tidyr
#' @import dplyr
#'
#'

#try to parse given date time string with list of provided formatting strings.
probe_ts_format <- function(x, format=NULL, tz=NULL){

  if (!is.null(format)){
    formats <- c(format)

  } else {
    formats <- c("%m/%d/%y %H:%M:%OS", "%Y-%m-%d %H:%M:%OS")

  }

  #add lubridate::guess_formats !!

  tz <- ifelse(is.null(tz), Sys.timezone(), tz)

  for (frmt in formats){
    if (!is.na(strptime(x[1], format=frmt))){
      x <- strptime(x, frmt, tz)
      break
    }
  }

  x
}


#' @title Parse sociometric data sheets
#'
#' @description Sociometric data sheets are often manufacturer dependent and come in different
#'  column configurations. The generic \code{parse} function provides an interface to write
#'  specific functions for each indiosyncractic data format and convert it into a tidy data.
#'  New implementations should create the corresponding S3 class instance and implement the
#'  specific parse function.
#'
#' @param raw_df Data frame usually returned by file read operations.
#' @param format String representation of timestamp format
#' @param as_posixct Logical. Format timestamps as \code{POSIXct}
#' @param tz String. Specify timezone. Default \code{tz=NULL} will use \code{Syst.timezone()}
#'
#' @return Object.
#'
#' @export
parse <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){
  UseMethod("parse")
}

parse.default <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){
  #warning("No suitable method for object of class ", class(x))
  return(raw_df)
}


#' @title Parse timestamp data.
#'
#' @description Converts string representation of timestamp value into date time object. Most
#'  exported sociometric measurements are timestamped. During the export (from Datalab) and import
#'  process (into R), the timestamp is stored as a string representation which needs to be converted
#'  into a date time object. The \code{col} parameter indicates the position of the column which
#'  holds the timestamp values, mostly the first column. This method is called by most \code{parse}
#'  functions to make sure the uniform naming and formatting of the timestamp column across all
#'  different sociometric data types. If the \code{raw_df} timestamp data is already a POSIXct object
#'  the data frame is returned directly.
#'
#' @param raw_df Data frame as result of \code{read_*} functions. One column holds a character representation of timestamp values.
#' @param format Formatting string for timestamp. \code{format=NULL} dispatches \code{probe_ts_format}
#'  to assign a format automatically from a pre-established list (most common formats). \code{format=1|2}
#'  is the index position to the vector of formats; \code{format="\%d/\%m/\%y"} indicates specific
#'  format to be used in  \code{\link[base]{strptime}}.
#' \enumerate{
#'   \item \code{"\%m/\%d/\%y \%H:\%M:\%OS"}
#'   \item \code{"\%Y-\%m-\%d \%H:\%M:\%OS"}
#' }
#'
#' @param as_posixct Logical. If resulting timestamp should be converted to POSIXct object.
#' @param ts_col Index or name of colum that holds timestamp data.
#' @param tz String. Specify timezone. Default \code{tz=NULL} will use \code{Syst.timezone()}
#'
#' @return Original data frame with timestamp column converted to datetime object. Data frame
#'  has class "smtrx".
#'
#' @export
parse.smtrx <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  cls <- class(raw_df)

  #check which col holds timestamp data by type and name.

  x <- raw_df[[ts_col]] #Timestamp data is usually in the first colum.

  #makes sure corresponding column is named correctly
  names(raw_df)[ts_col] <- c("Timestamp")

  tz <- ifelse(is.null(tz), Sys.timezone(), tz)

  if (lubridate::is.POSIXct(x)){
    raw_df$Timestamp <- lubridate::force_tz(raw_df$Timestamp, tz)
    return(raw_df)
  }


  #tmp save of ts entry
  tstmp <- x[1]

  #no format provided, try out a couple of common date formats
  if (is.null(format)){
    x <- probe_ts_format(x, tz)

  } else if (is.na(format)){
    x <- probe_ts_format(x, tz)

    #otherwise use provided format
  } else {
    x <- strptime(x, format, tz)
  }

  #check result
  if (all(is.na(x)) | is.null(x)){
    warning("Parsing timestamp string '",tstmp,"' with format '",format,"' produces NULL. \n")
  }

  #POSIXct (necessary for tidyr::gather)
  if (as_posixct)
    x <- as.POSIXct(x, tz)


  raw_df[[ts_col]] <- x

  class(raw_df) <- cls

  return(raw_df)
}



#' @title Parse ego-based data
#'
#' @description Many data sheets contain ego based data, i.e. one bage produces
#'  a certain measurement (Volumne, BM Activity, ...) per timestamp.
#'
#' @inheritParams parse.smtrx
#'
#' @return Data frame of class c("ego") of pattern c("Timestamp", "Badge.ID", "Measure","Source")
#'
#' @export
parse.ego <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  #passes raw_df to parent parse function and returns value
  #df<-raw_df via parent parse.
  df <- NextMethod("parse")

  if ("pitch" %in% class(df) | "sp" %in% class(df)){
    return(df)
  }

  df <- tidyr::gather(df, Badge.ID, Measure, -Timestamp, -Source)

  if (is.character(df$Measure)){
    df$Measure <- stringr::str_replace(df$Measure, ",", ".") #sometimes measures have ","
    df$Measure <- as.numeric(df$Measure)
  }

  #remove "B-" prefix from Badge.ID values
  df$Badge.ID <- stringr::str_replace(df$Badge.ID, "B-", "")

  df <- dplyr::select(df, Timestamp, Badge.ID, Measure, Source)

  class(df) <- class(raw_df)

  df
}




# @title Parse interaction data
#
# @description Interaction data has four columns:
#  cols=c("Timestamp", "Badge.ID", "Other.ID", "RSSI"). An additional column "Source"
#  indices the source data sheet retrieved by \code{\link{hash_source}}
#
# @inheritParams parse.smtrx
#
# @return Data frame of class "interact"
#
# @export
parse.interact <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  #passes raw_df to parent parse function and returns value
  #df<-raw_df via parent parse.
  df <- NextMethod("parse")

  names(df) <- c("Timestamp", "Badge.ID", "Other.ID", "RSSI", "Source")

  df$Badge.ID <- as.character(df$Badge.ID)
  df$Other.ID <- as.character(df$Other.ID)

  #class(df) <- class(raw_df)

  df

}


#parse combined interactions duration list
parse.ric <- function(raw_df, format=NULL, as_posixct=T, ts_col=c(3,4), tz=NULL){

  #parse start and end timestamp
  df <- NextMethod("parse", ts_col=3)
  df <- NextMethod("parse", ts_col=4)

  names(df) <- c("Badge.ID", "Other.ID", "Start", "End", "Source")

  df$Badge.ID <- stringr::str_sub(df$Badge.ID, 3, 6)
  df$Other.ID <- stringr::str_sub(df$Other.ID, 3, 6)

  df

}


# parse.rmatrix <- function(raw_df){
#
# }


# @title Parse audio volume data sheets
#
# @description Audio data has three columns:
#  cols=c("Timestamp", "Badge.ID", "Volume"). An additional column "Source"
#  indices the source data sheet retrieved by \code{\link{hash_source}}
#
# @inheritParams parse.smtrx
#
# @return Data frame of class c("vol", "ego", "smtrx")
#
# @export
parse.vol <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  #passes raw_df to parent parse function and returns value
  #df<-raw_df via parent parse.
  df <- NextMethod("parse")

  names(df) <- c("Timestamp", "Badge.ID", "Volume", "Source")

  class(df) <- class(raw_df)

  df
}


# @title Parse audio pitch data sheets
#
# @description Pitch data sheets have c("Timestamp", "Badge.ID", "Pitch", "Volume") columns.
#
# @inheritParams parse.smtrx
#
# @return Data frame with additional class of c("pitch", "ego", "smtrx")
#
# @export
parse.pitch <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  #passes raw_df to parent parse function and returns value
  #df<-raw_df via parent parse.
  df <- NextMethod("parse")

  bids <- stringr::str_extract(names(df), "[0-9]{4}")
  bids <- bids[!is.na(bids)]

  #construct new colnames,
  nnames <- c("Timestamp")

  for (bid1 in bids){
    namp <- paste0(bid1, "p")
    namv <- paste0(bid1, "v")
    nnames <- c(nnames, namp, namv)
  }

  names(df) <- c(nnames, "Source")
  df <- df[-1,]

  #split into two df
  df.p <- df[,c(1, seq(2, ncol(df)-1, by=2), ncol(df))]
  df.v <- df[,c(seq(1, ncol(df), by=2), ncol(df))]

  df.p <- tidyr::gather(df.p, -Timestamp, -Source, key="Badge.ID", value="Pitch")
  df.p$Badge.ID <- stringr::str_sub(df.p$Badge.ID, 1, 4)

  df.v <- tidyr::gather(df.v, -Timestamp, -Source, key="Badge.ID", value="Volume") %>%
    dplyr::select(-Source)

  df.v$Badge.ID <- stringr::str_sub(df.v$Badge.ID, 1, 4)

  df.p <- dplyr::left_join(df.p, df.v, by=c("Timestamp", "Badge.ID"))

  df <- df.p
  rm(df.p, df.v)

  suppressWarnings(
    df$Pitch <- as.numeric(df$Pitch)
  )
  suppressWarnings(
    df$Volume <- as.numeric(df$Volume)
  )

  #reorder columns
  df <- dplyr::select(df, Timestamp, Badge.ID, Pitch, Volume, Source)

  class(df) <- class(raw_df)

  df

}



# @title Parse speech profile Excel sheet
#
# @details Parse sub-columns for each badge in the Sociometric Excel file, containing
#  the following columns: c("speaking", "overlap", "listening", "silent", "total_speaking", "total_silent", "Badge.ID")
#
# @param Data frame with the original excel formats
#
# @return Tibble with tidy data
parse.sp <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  #passes raw_df to parent parse function and returns value
  #df<-raw_df via parent parse.
  df <- NextMethod("parse")


  #get all badge ids from automatic assigned colnames
  bids <- str_extract(names(df), "[0-9]{4}")
  bids <- bids[!is.na(bids)]

  #construct new colnames,
  nnames <- c("Timestamp","Speaking", "Overlap", "Listening", "Silent", "Total_speaking", "Total_silent", "Badge.ID")

  #remove first data row which contains xls action names.
  #and select first 7 columns containing timestamp and above listed cols.
  dft <- df[-1,1:7]

  #assig first badge id
  dft$Badge.ID <- bids[1]
  names(dft) <-   nnames

  #get all the rest of the badges of further *7 columns.
  for (i in 2:length(bids)){
    tmp <- df[,c(1,((i-1)*6+2:7))]
    tmp$Badge.ID <- bids[i]
    names(tmp) <-   nnames
    dft <- bind_rows(dft, tmp)
  }

  suppressWarnings(
    df <- mutate_all(dft, funs(as.numeric))
  )
  df$Badge.ID <- dft$Badge.ID
  df$Timestamp <- dft$Timestamp



  #get session id
  #if (ses_info) df$ses_info <- extract_session(file)

  #remove empty rows
  df <- df[complete.cases(df),]

  # TODO::
  #the row immediately following an empty row has wrong participation values > 1. All speech profile
  #readings however are between 0 and 1. The higher values get removed here.
  #Need to specify this only for corresponding columns, not all of them!!
  #df <- df %>% filter_at(vars(-Timestamp, -Badge.ID), all_vars(.<=1))

  df <- select(df, Timestamp, Badge.ID, Speaking, Overlap, Listening, Silent, Total_speaking, Total_silent)

  class(df) <- class(raw_df)

  df

}



#parse speech participation sheet
parse.part <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  #passes raw_df to parent parse function and returns value
  #df<-raw_df via parent parse.
  df <- NextMethod("parse")

  names(df) <- c("Timestamp", "Badge.ID", "Participation", "Source")

  class(df) <- class(raw_df)

  df
}



#Mirroring
parse.mirror <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  #passes raw_df to parent parse function and returns value
  #df<-raw_df via parent parse.
  df <- NextMethod("parse")

  #get all badge ids from automatic assigned colnames
  bids <- str_extract(names(df), "[0-9]{4}")
  bids <- bids[!is.na(bids)]

  #construct new colnames,
  nnames <- c("Timestamp")

  for (bid1 in bids){               #Badge id2 is mirroring (similar to) Badge id1
    for (bid2 in bids){
      nam <- paste0(bid1, "_", bid2)
      namlag <- paste0(nam, "_lag")
      nnames <- c(nnames, nam, namlag)
    }
  }
  names(df) <- nnames
  df <- df[-1,] #remove first row which contains xls names.

  #remove all self-mirroring columns, where bid1 == bid2
  remove <- NULL
  for (i in 2:length(nnames)){
    parts <- str_split(nnames[i], "_")
    if (parts[[1]][2] == parts[[1]][1]){
      remove <- c(remove, i)
    }
  }

  df <- df[,-remove]

  #split into two data frames, one for the mirror readings, one for the lag readings.

  df.m <- df[,c(1, seq(2, length(names(df))-1, 2)) ]
  df.l <- df[,c(seq(1, length(names(df)), 2)) ]

  #gather mirror/lag reading colums of each badge pair into two columns
  df.m <- gather(df.m, -Timestamp, key="Badges", value="Similar")
  df.l <- gather(df.l, -Timestamp, key="Badges", value="Lag")
  df.l$Badges <- str_sub(df.l$Badges, start=1, end=9) #remove "_lag" from col names

  #join two databases and separate the Badge col values into two columns.
  df.m <- left_join(df.m, df.l, by=c("Timestamp", "Badges"))
  df.m <- separate(df.m, Badges, into=c("Badge.ID", "Other.ID"), sep="_") #Badge.ID=Lead, Other.ID=Follow
  df.m$Similar <- as.double(df.m$Similar)
  df.m$Lag <- as.numeric(df.m$Lag)
  df.m$Source <- as.character(raw_df[1,"Source"])

  df <- df.m
  rm(df.m, df.l)

  df
}


#parse consistency sheet
parse.con <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  #passes raw_df to parent parse function and returns value
  #df<-raw_df via parent parse.
  df <- NextMethod("parse")

  names(df) <- c("Timestamp", "Badge.ID", "Consistency", "Source")

  class(df) <- class(raw_df)

  df
}


# Activity
parse.act <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  df <- NextMethod("parse")

  # print(class(raw_df))
  # print(names(df))


  names(df) <- c("Timestamp", "Badge.ID", "Activity", "Source")

  class(df) <- class(raw_df)

  df

}

# Rate
parse.rate <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  df <- NextMethod("parse")

  names(df) <- c("Timestamp", "Badge.ID", "Rate", "Source")

  class(df) <- class(raw_df)

  df

}


# Posture
parse.pos <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  #passes raw_df to parent parse function and returns value
  #df<-raw_df via parent parse.
  df <- NextMethod("parse")


  df <- df.bm

  bids <- str_extract(names(df), "[0-9]{4}")
  bids <- bids[!is.na(bids)]

  #construct new colnames,
  nnames <- c("Timestamp")

  for (bid1 in bids){
    namlr <- paste0(bid1, "lr")
    namfb <- paste0(bid1, "fb")
    nnames <- c(nnames, namlr, namfb)
  }

  names(df) <- nnames
  df <- df[-1,] #erase first row left-right, front-back names.

  #split into two df
  df.lr <- df[,c(1, seq(2, ncol(df)-2, by=2))]
  df.fb <- df[,seq(1, ncol(df)-1, by=2)]

  df.lr <- gather(df.lr, -Timestamp, key="Badge.ID", value="Left_Right")
  df.lr$Badge.ID <- str_sub(df.lr$Badge.ID, 1, 4) #exract id  from "3456lr" to "3456"

  df.fb <- gather(df.fb, -Timestamp, key="Badge.ID", value="Front_Back")
  df.fb$Badge.ID <- str_sub(df.fb$Badge.ID, 1, 4)

  df.lr <- left_join(df.lr, df.fb, by=c("Timestamp", "Badge.ID"))

  df <- df.lr
  rm(df.lr, df.fb)

  df$Left_Right <- as.numeric(df$Left_Right)
  df$Front_Back <- as.numeric(df$Front_Back)

  df$Source <- as.character(raw_df[1,"Source"])

  class(df) <- class(raw_df)

  df

}


#turn taking sheet
parse.tt <- function(raw_df, format=NULL, as_posixct=T){

  df <- NextMethod("parse")

  #df$SessionID <- extract.sessionID(filepath)
  # df$SessionID <- regmatches(filepath, regexpr("S[0-9]+", filepath))
  # df$SessionID <- substr(df$SessionID, 2, nchar(df$SessionID))
  # df$SessionID <- as.numeric(df$SessionID)
  #df$fileName <- basename(filepath)

  df <- select(df, c(13,1:7,11,12,14))
  names(df) <- c("SessionID", "Badge", "TotalTurns", "Turns", "TurnsAfter","SelfTurns","SpeakSeg", "SilentSeg", "Interrupt_Success", "Interrupt_Fail", "File")

  class(df) <- class(raw_df)

  df

}



# @title Parse frequency sheet
#
# @details Parse sub-columns for each badge in the Sociometric Excel file, containing
#  the following columns: hz_0, amp_0, hz_1, amp_1, hz_2, amp_3, hz_3, amp_3.
#
# @param Data frame with the original excel formats
#
# @return Tibble with tidy data
parse.frq <- function(raw_df, format=NULL, as_posixct=T, ts_col=1, tz=NULL){

  #passes raw_df to parent parse function and returns value
  #df<-raw_df via parent parse.
  df <- NextMethod("parse")


  #get all badge ids from automatic assigned colnames
  bids <- str_extract(names(df), "[0-9]{4}")
  bids <- bids[!is.na(bids)]

  #construct new colnames,
  nnames <- c("Timestamp","hz_0", "amp_0", "hz_1", "amp_1", "hz_2", "amp_2", "hz_3", "amp_3", "Badge.ID")

  #remove first data row which contains xls action names.
  #and select first 8 columns containing timestamp and above listed cols.
  dft <- df[-1,1:9]

  #assign first badge id
  dft$Badge.ID <- bids[1]
  names(dft) <-   nnames

  #repeat column extraction for all badges listed in bids
  for (i in 2:length(bids)){
    tmp <- df[,c(1,((i-1)*8+2:9) )]
    tmp$Badge.ID <- bids[i]
    names(tmp) <-   nnames
    dft <- bind_rows(dft, tmp)
  }

  df <- mutate_all(dft, funs(as.numeric))
  df$Badge.ID <- dft$Badge.ID
  df$Timestamp <- dft$Timestamp

  dft <- NULL

  #stack the three Hz and Amp bands
  for (i in seq(2,8,2)){
    bandstr <- paste0("Band_", (i-2))
    if (is.null(dft)){
      dft <- df[,c(1,i,(i+1),10)]
      dft$Band <- bandstr
      names(dft) <- c("Timestamp", "Hz", "Amplitude", "Badge.ID", "Band")
    } else {
      tmp <- df[, c(1, i,(i+1),10)]
      tmp$Band <- bandstr
      names(tmp) <- c("Timestamp", "Hz", "Amplitude", "Badge.ID", "Band")
      dft <- bind_rows(dft, tmp)
    }
  }


  df <- select(dft, Timestamp, Badge.ID, Band, Hz, Amplitude)

  df$Source <- as.character(raw_df[1,"Source"])

  class(df) <- class(raw_df)

  df

}



