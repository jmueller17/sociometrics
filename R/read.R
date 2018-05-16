#' @importFrom readr read_delim
#' @importFrom tibble tibble
#' @importFrom readxl read_excel
#'
#' @import stringr
#' @import tidyr
#' @import dplyr
#'
#'
#'
#' @title Import interaction data
#'
#' @description Read CSV, XLS or XLSX files from Sociometrics that contain Infrared-, Bluetooth- or both data types.
#'
#' @param file Path to source data file (xls, xlxs or csv).
#' @param type Indicates the type of interaction data to be imported (see details for available
#'  abbreviations).
#' @param undirect Logical. Convert directed to undirected edge: \code{A->B | B->A = A<->B}
#' @param replv Anonymization. Default \code{replv=FALSE} will leave original (badge) IDs in place. Set
#'  to \code{replv=TRUE} will replace IDs with numbers starting from 1..n. Provide \code{replv=data.frame} with
#'  values: First column holds original values, second column replacement values.
#' @param delim Single delimiter character for reading CSV data. Ignored for Excel files.
#' @param format Optional format for parsing timestamp data. If no format is specified two
#'  pre-established timestamp formats are tried ut. See  \code{\link{parse.smtrx}} for details.
#' @param tz String. Default \code{tz=NULL} will use system timezone (\code{Sys.timezone()}) and
#'  assign to timestamp. Useful for explicitly setting other than system timezone for timestamp
#'  data.
#' @param cls Vector of class names. Default \code{cls=NULL} uses pre-defined sociometric
#'  classes associated with the \code{type} abbreviation. However, class names can be specified
#'  explicitly as well.
#'
#' @return Tibble with data in tidy format
#'
#' @details The following data sheets can be specified by setting the appropriate \code{type}
#'  parameter:
#'
#'  \itemize{
#'    \item{"IR" - Infrared data}
#'    \item{"BT" - Bluetooth detects}
#'    \item{"IR_BT" - Both, Infrared and Bluetooth detects.}
#'    \item{"RIC" - Reads the interactions spell (or durations) between nodes. It contains "from", "to",
#'     columns and start/end timestamps combining both Infrared and Bluetooth readings.}
#'    \item{"NIC" - Reads the exported matrix of absolute interaction counts between all badges. Does
#'     not contain any timestamp data.}
#'  }
#'
#'  All specified types will produce a data frame with five columns. Timestamp, Badge.ID, Other.ID,
#'  RSSI and Source. The data frame format is the same for IR, BT, IR_BT imports. The RSSI (Radio
#'  Signal Strength Indictor) holds a numeric value for Bluetooth signals (usually ranging from -40 to
#'  -90) indicating the relative proximity during BT signal detects. Smaller values indicate a weaker
#'  signal. Infrared detects do not have a measure of signal strength. IR detects are stored as NA values
#'  in the RSSI column.
#'
#' @examples
#'
#' #load interaction including both BT and IR.
#' df1 <- read_interaction(file="data/datalab-full.xlsx", type="IR_BT")
#' df1
#'
#' #load only Bluetooth data, replace Badge ids with numbers starting 1...n
#' df2 <- read_interaction(file="data/datalab-full.xlsx", type="BT", replv=T)
#' df2
#'
#' #load only Infrared data, replace Badge ids with provided list
#' replv <- data.frame(mv=c(3182, 3189, 3183, 3178, 3184, 3081), rv=c(LETTERS[1:5]))
#' df3 <- read_interaction(file="data/datalab-full.xlsx", type="IR", replv=replv)
#' df3
#'
#' #load interaction data, adding an undirected pair column
#' df4 <- read_interaction(file="data/datalab-full.xlsx", type="IR_BT", undirect=T)
#' df4
#'
#'
#' @seealso \code{\link{read_audio}}, \code{\link{read_body}}
#'
#' @export
#'
read_interaction <- function(file, type, undirect=F,
                             replv=F, delim="\t", format=NULL, tz=NULL, cls=NULL, ...){

  raw_df <- read_smtrx_file(file, type=type, delim=delim)

  if (is.null(cls)){
    class(raw_df) <- c(attr(raw_df, "pclass"), class(raw_df))
  } else {
    class(raw_df) <- cls
  }

  df <- parse(raw_df, format=format, tz=tz)

  #anonymize Badge ID entries
  if (is.data.frame(replv)){
    df <- anonymize(df, ids=replv[[1]], replv=replv[[2]])

  } else if (replv == T){
    df <- anonymize(df)
  }

  #add extra column of undirected edge list
  if (undirect){
    df <- mreverse(df, into="Pairs")
  }

  df
}




#' @title Import audio data
#'
#' @description Read audio CSV, XLS or XLSX files from Sociometrics. Original Excel sheets have
#'  often a nested column structure where 1 or more badges produce data over several subcolumns.
#'  This strucutre will be converted into a tidy data format.
#'
#' @inheritParams read_interaction
#' @param type Indicates the type of audio data to be imported (see details for available
#'  abbreviations).
#' @param ses_info Logical. Extract and store session info from file path if available.
#' @param na.rm Logical. Calls \code{na.omit} on the entire data frame after conversion to
#'  tidy format.
#'
#' @details Volume, pitch and frequencies are avaible for the front- and back microphone which
#'  can be indicated by the "_F" or "_B" suffix on each abbreviation. If no suffix is included for
#'  both microphone data sheets (back + front) will be loaded. The following abbreviations are
#'  available for the \code{type} parameter:
#'
#'  \itemize{
#'    \item{"VOL[_F|_B]" - Volume. "VOL" will load both front- and back microphone sheet. "VOL_F"
#'     only front and "VOL_B" only back microphone data. Volume levels range between 0 and 1.
#'     Values < 0.01 indicate not speaking, 0.01 - 0.02 speaking quitely, 0.03 - 0.08 speaking louder, and
#'     > 0.08 speaking loudly.}
#'    \item{"PITCH[_F|_B]" - Pitch. Depending on the DataLab export settings pitch measures are
#'     aggregated over a given time interval, starting from 1 - 60+ seconds. Typical male fundamental
#'     frequency ranges from 85 to 180 Hz; typical adult female from 165 to 255 Hz.}
#'    \item{"SP" - Speech profile (no front/back microphone option). The speech profile indicates for each badge
#'     Speaking, Overlap, Listening, Silent, Total Speaking and Total Silent duration. Values depend on the
#'     chosen time interval: if the speech profile has been exported over a period of 60s, Speaking (and all
#'     other) measures indicate fractions over the 60 second period, ranging from 0 to 60.
#'     If the speech profile has been exported over 1 second intervals, the columns indicate values that
#'     range from 0 to 1 second. \emph{Speaking} indicates the total time fraction a particular badge
#'     wearer was speaking; \emph{Overlap} the time fraction a person was speaking while someone else
#'     was speaking; \emph{Listening} the time fraction a particular badge was silent while someone else was speaking; \emph{Silent} the time fraction nobody
#'     was speaking. \emph{Total Speaking} is the sum of Speaking + Overlap per badge.}
#'    \item{"PAR" - Speech participation (no front/back microphone option). Logical. Indicates if a particular
#'     badge was speaking or not during the given time interval.}
#'    \item{"VOL_MIR[_F|_B]" - Volume mirroring. \emph{Similar} indicates the similarity between volume
#'     readings between two badges and ranges between 0 (no match) and 1 (perfect match) within the
#'     given time interval. \emph{Lag} indicates the time lag between matches. }
#'    \item{"VOL_CON[_F|_B]" - Volume consistency of each badge’s front audio amplitude, as measured in
#'     Activity (volume) (front). Consistency ranges from 0 to 1, where 1 indicates no changes in speech
#'     amplitude, and 0 indicates the maximum amount of variation in speech amplitude.}
#'    \item{"FRQ_[_F|_B]" - Dominant frequency. Contains three frequency bands hz_0, hz_1...hz_2 and
#'     corresponding amplitude readings amp_0, ...amp_2. Converted to tidy format, the resulting
#'     tibble contains the usual Timestamp, Badge.ID column followed by \emph{Band} column indicating
#'     one of the three bands \emph{Band_0, ... Band_2}} and two further columns \emph{Hz} and \emph{Amplitude}.
#'     There are potentially 4 frequency bands shown, hz_0 & amp_0 is the strongest PEAK in cepstrum,
#'     hz_1 & amp_1 is the second strongest PEAK, and so on. If there are fewer than k peaks in cepstrum,
#'     the hz_k and larger values are empty. E.g if there are only two peaks in cepstrum, hz_2 and hz_3 are empty and not exported.
#'    \item{"TT" - Turn taking sheet. \emph{Speaking Segment}: Any continuous, uninterrupted length of
#'     speech made by a single person. \emph{Turns}: Turns are speaking segments that occur after and
#'     within 10 seconds of, another speaking segment. By default a speech segment must be made within
#'     10 seconds after the previous one ended in order to be considered a turn. \emph{Self-turn}: A
#'     speaker starts speaking, pauses for greater than 0.5 seconds (but less than 10 seconds), and then
#'     resumes speaking. \emph{Successful interruptions}: Person A is talking. Peron B starts talking over
#'      A. If Person A talks for less than 5 out of the next 10 seconds, then Person B successfully
#'      interrupted Person A. \emph{Unsuccessful interruptions}: Person A is talking. Peron B starts talking
#'      over A. If Person A talks for more than 5 out of the next 10 seconds, then Person B successfully
#'      interrupted Person A. \emph{Pause}: A pause is a period of time within which there is no speaking.
#'      All pauses are between .5s and 10s.
#'    }
#'  }
#'
#'
#' @return Tibble with data in tidy format
#'
#' @seealso \code{\link{read_body}} \code{\link{read_interaction}}
#'
#' @export
#'
read_audio <- function(file, type, ses_info=F, replv=F, delim="\t",
                       format = NULL, tz=NULL, na.rm=F, cls=NULL, ...){

  raw_df <- NULL

  ext <- tolower(tools::file_ext(file))

  #load either front or back microphone sheet for excel files
  if (ext == "xls" | ext == "xlsx") {

    if (length(type)>1){
      warning("Loading several sheets simultaneously likely causes column mismatch!")
    }

    for (tt in type){
      abbrs <- load_strings(tt) #which sheets?
      for (abbr in abbrs){
        tmp <- read_smtrx_file(file, type=abbr, delim=delim, ...)
        #tmp$Source <- abbr

        if (is.null(raw_df)){
          raw_df <- tmp
        } else {
          raw_df <- rbind(raw_df, tmp)
        }
      }
    }

  #type is ignored for CSV files which have no sheets
  } else {
    raw_df <- read_smtrx_file(file, type=type, delim=delim, ...)

  }


  if (is.null(cls)){
    class(raw_df) <- c(attr(raw_df, "pclass"), class(raw_df))
  } else {
    class(raw_df) <- cls
  }

  df <- parse(raw_df, format, tz)

  if (na.rm){
    df <- na.omit(df)
    class(df) <- class(raw_df)
  }

  #anonymize Badge ID entries
  if (is.data.frame(replv)){
    df <- anonymize(df, ids=replv[[1]], replv=replv[[2]])

  } else if (replv == T){
    df <- anonymize(df)
  }

  df
}


#' @title Import accelerometer data
#'
#' @description
#'
#' @inheritParams read_interaction
#' @param type Indicates the type of accelerometer data to be imported (see details for available
#'  abbreviations).
#' @param na.rm Logical. Calls \code{na.omit} on the entire data frame after conversion to
#'  tidy format.
#'
#' @return Object of type "act", "smtrx"
#'
#' @details The following accelerometer data sheets can be read by setting the \code{type} parameter
#'  to one of the following values:
#'  \itemize{
#'    \item{"BM" - Body movement activity. This is the absolute value of the first derivative of energy.
#'     This provides a more reliable measure of someone’s activity, while eliminating the accelerometer’s
#'     magnitude natural offset. Values < 0.002 indicate very low activity; values between
#'     0.003 - 0.19 moderate amounts of activity and > 0.2 movements like walking.}
#'    \item{"BM_ACC" - Body movement. Accelerometer’s energy magnitude over the 3 axes of measurement.}
#'    \item{"BM_RATE" - Body movement rate. This is the second derivative of energy. The sign
#'     (positive or negative) of Rate (BM) indicates the direction of the change in someone’s activity
#'     levels, as measured by Activity (BM). A positive Rate (BM) indicates the person’s activity is
#'     increasing. A negative Rate(BM) indicates the activity is decreasing.}
#'    \item{"BM_CON" - Body movement consistency. Consistency ranges from 0 to 1, where 1 indicates no
#'     changes in activity level, and 0 indicates the maximum amount of variation in activity levels.}
#'    \item{"BM_MIR" - Body movement mirroring. Mirroring (BM) values indicate how similar one badge’s
#'     Activity (BM) data series is to another badge’s Activity (BM) data series over time. The values
#'     range from 0 to 1, where 0 indicates no similarity and 1 indicates the two data series are identical.}
#'    \item{"POS" - Posture. Left-right / front-back.}
#'    \item{"POS_ACT" - Posture activity. Activity (Posture) shows the absolute angular velocity for
#'     every badge at every timestamp. }
#'    \item{"POS_RATE"- Posture rate. Rate (Posture) shows the angular acceleration for every badge at every timestamp.}
#'    \item{"POS_MIR" - Posture mirroring. See "BM_MIR".}
#' }
#'
#'
#'
#' @export
#'
read_body <- function(file, type, undirect=F,
                      replv=F, delim="\t", format=NULL, tz=NULL, na.rm=F, cls=NULL, ...){


  raw_df <- read_smtrx_file(file, type=type, delim=delim)

  if (is.null(cls)){
    class(raw_df) <- c(attr(raw_df, "pclass"), class(raw_df))
  } else {
    class(raw_df) <- cls
  }

  df <- parse(raw_df, format, tz)

  if (na.rm){
    df <- na.omit(df)
    class(df) <- class(raw_df)
  }


  #anonymize Badge ID entries
  if (is.data.frame(replv)){
    df <- anonymize(df, ids=replv[[1]], replv=replv[[2]])

  } else if (replv == T){
    df <- anonymize(df)
  }

  #add extra column for undirected edge list
  if (undirect & type %in% c("BM_MIR", "POS_MIR")){
    df <- mreverse(df, into="Pairs")
  }

  df
}


# Read xls or csv file
#
# @details Read xls or csv data file. The function returns the "raw" sociometric data sheet.
#
# @param file File path.
# @param type The abbreviated data type to be loaded. \See{hash_source}
# @param delim Single delimiting character for CSV files. Default is TAB
# @param ... Further parameters passed on to \code{read_delim()} or \code{read_excel()} functions
#
# @return Tibble of raw sociometric data sheet
#
read_smtrx_file <- function(file, type, delim="\t",...){

  fformat <- tolower(tools::file_ext(file))

  if (fformat == "xls" || fformat == "xlsx") {
    sheet <- hash_source(type)$sheet
    df <- readxl::read_excel(file, sheet=sheet, ...)

  } else if (fformat == "csv") {
    df <- readr::read_delim(file, delim=delim, ...)

  } else {
    stop("Can't recognize file format. Should be 'xlsx', 'xls' or 'csv': ", sQuote(fformat), call. = FALSE)
  }

  message("\nReading ", nrow(df), " rows from ", file, "\n")

  if (nrow(df) == 0){
    stop("File is empty.", call. = F)
  }

  df$Source <- type

  #suggest class
  attr(df, "pclass") <- hash_source(type)$pclass

  df
}



#' @title Map data type abbreviations to source names
#'
#' @description Sociometric DataLab exports several different data sheets with often
#'  cumbersome names. The following function convert abbreviated shortcuts
#'  to actual sheet names and suggests the S3 class which determines the corresponding parsing function.
#'
#' @param abbr String of abbreviated data types.
#'
#' @return List with source (sheet) name and likely S3 class for parsing
#'
#' @examples
#'
#' #produce a list of all currently available abbreviations
#' hash_source()
#'
#' #name of data sheet for Infrared data
#' hash_shource("IR")
#'
#' #name of data sheet for front microphone volume
#' hash_shource("VOL_F")
#'
#' @export
hash_source <- function(abbr=NULL){

  aabbr <- c("IR", "BT", "IR_BT", "RIC", "NIC",
             "VOL_F", "VOL_B", "PITCH_F", "PITCH_B", "PAR", "SP", "VOL_MIR_F", "VOL_MIR_B",
             "VOL_CON_F", "VOL_CON_B", "FRQ_F", "FRQ_B", "TT",
             "BM", "BM_ACC", "BM_RATE", "BM_CON", "BM_MIR",
             "POS", "POS_ACT", "POS_RATE", "POS_MIR")

  if (is.null(abbr)){
    return(aabbr)
  }

  if (abbr == "IR") {
    return(list(sheet="r_facetoface_matrix1", pclass=c("interact", "smtrx")))

  } else if (abbr == "BT"){
    return(list(sheet="r_proximity_matrix1", pclass=c("interact", "smtrx")))

  } else if (abbr == "IR_BT"){
    return(list(sheet="r_combined_matrix1", pclass=c("interact", "smtrx")))

  } else if (abbr == "RIC"){
    return(list(sheet="r_interactions_combined1", pclass=c("ric", "smtrx")))

  } else if (abbr == "NIC"){
    return(list(sheet="n_combined_matrix", plcass=c("rmatrix")))

  } else if (abbr == "VOL_F"){
    return(list(sheet="t_audio_front_volume1", pclass=c("vol", "ego", "smtrx")))

  } else if (abbr == "VOL_B") {
    return(list(sheet="t_audio_back_volume1", pclass=c("vol", "ego", "smtrx")))

  } else if (abbr == "PITCH_F") {
    return(list(sheet="t_audio_front_pitch1", pclass=c("pitch", "ego", "smtrx")))

  } else if (abbr == "PITCH_B"){
    return(list(sheet="t_audio_back_pitch1", pclass=c("pitch", "ego", "smtrx")))

  } else if (abbr == "PAR") {
    return(list(sheet="t_speech_participation1", pclass=c("par", "ego", "smtrx")))

  } else if (abbr == "SP") {
    return (list(sheet="t_speech_profile1", pclass=c("sp", "ego", "smtrx")))

  } else if (abbr == "VOL_MIR_F"){
    return(list(sheet="t_audio_front_vol_mirroring1", pclass=c("mirror")))

  } else if (abbr == "VOL_MIR_B") {
    return(list(sheet="t_audio_back_vol_mirroring1", pclass=c("mirror", "smtrx")))

  } else if (abbr == "VOL_CON_F") {
    return(list(sheet="t_audio_front_vol_consistency1", pclass=c("con", "ego", "smtrx")))

  } else if (abbr == "VOL_CON_B") {
    return(list(sheet="t_audio_back_vol_consistency1", pclass=c("con", "ego", "smtrx")))

  } else if (abbr == "FRQ_F"){
    return(list(sheet="t_audio_front_frequency1", pclass=c("frq", "ego", "smtrx")))

  } else if (abbr == "FRQ_B") {
    return(list(sheet="t_audio_back_frequency1", pclass=c("frq", "ego",  "smtrx")))

  } else if (abbr == "TT") {
    return(list(sheet="r_tt_turntaking1", pclass=c("tt")))

  } else if (abbr == "BM") {
    return(list(sheet="t_BM_activity1", pclass=c("act", "ego", "smtrx")))

  } else if (abbr == "BM_ACC") {
    return(list(sheet="t_BM_bm1", pclass=c("act", "ego", "smtrx")))

  } else if (abbr == "BM_RATE") {
    return(list(sheet="t_BM_rate1", pclass=c("rate","ego", "smtrx")))

  } else if (abbr == "BM_CON") {
    return(list(sheet="t_BM_consistency1", pclass=c("con", "ego", "smtrx")))

  } else if (abbr == "BM_MIR") {
    return(list(sheet="t_BM_mirroring1", pclass=c("mirror", "smtrx")))

  } else if (abbr == "POS") {
    return(list(sheet="t_posture_posture1", pclass=c("pos", "ego", "smtrx")))

  } else if (abbr=="POS_ACT"){
    return(list(sheet="t_posture_activity1", pclass=c("act", "ego", "smtrx")))

  } else if (abbr =="POS_RATE") {
    return(list(sheet="t_posture_rate1", pclass=c("rate", "ego", "smtrx")))

  } else if (abbr == "POS_MIR") {
    return(list(sheet="t_posture_mirroring1", pclass=c("mirror", "smtrx")))

  } else {
    stop("Unknown sheet type '", abbr, "'. Available abbreviations are: \n", paste(aabbr, sep=", "))
    return(abbr)
  }

}



# @title Constructs string abbreviations for loading excel data sheets.
#
# @description Excel sheets store audio data in two different data sheets: one for
#   front microphone and one for the back-mic. The loading abbreviation indicates
#   the sheet globally, e.g "VOL" indicates the "VOL_F" or "VOL_B" sheet. If both sheets
#   need to be loaded, "VOL" will be converted into the back and front sheet.
#   Some data such as speech participation or speech profile or turn-taking
#   do no have the front/back difference. This function constructs the necessary loading
#   abbreviations passed to \code{hash_source}
#
# @param abbr String. Global or front/back loading abbreviation.
#
# @return Vector of loading abbreviations.
load_strings <- function(abbr){

  single_sheets <- c("SP", "PAR", "TT")

  is_single <- stringr::str_detect(abbr, single_sheets)

  if (any(is_single)) {
    return(single_sheets[is_single])

  } else if (stringr::str_detect(abbr, "_F") | stringr::str_detect(abbr, "_B")){
    return(abbr)

  #speech participation or speech profile do not have back/front difference
  } else {
    return(c(paste0(abbr, "_F"), paste0(abbr, "_B")))
  }
}



