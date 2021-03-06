#' Sociometric example data
#'
#' Several data frames with parsed sociometric data, including interaction data, audio (microphone)
#' based data and accelerometer data (for body movement).
#'
#' @name smtrx
#'
#' @docType data
#'
#' @usage data("smtrx")
#'
#' @format Several data sheets are included
#'
#' \strong{df.interaction}
#' Interaction data frame contains both Bluetooth and Infrared data
#' \describe{
#'   \item{Timestamp}{Date time object}
#'   \item{Badge.ID}{Outgoing badge id}
#'   \item{Other.ID}{Incoming badge id}
#'   \item{RSSI}{Radion Signal Strength: strength of the Bluetooth signal. \code{RSSI=NA} indicates
#'     Infrared detecs.}
#'   \item{Source}{Source of the data: "IR"=Infrared, "BT"=Bluetooth, or "IR_BT" for both.}
#' }
#'
#' \cr\cr
#'
#' \strong{df.bm}
#' Accelerometer data representing body movement activity
#' \describe{
#'   \item{Timestamp}{Date time object}
#'   \item{Badge.ID}{Bbadge id}
#'   \item{Activity}{Energy level bound between [0,1]. Activity < 0.002 indicate very low activity
#'      0.003 - 0.019 moderate amounts of activity, Activity >= 0.2 large movements like walking }
#'   \item{Source}{Source file or sheet of the data}
#' }
#'
#' \cr\cr
#'
#' \strong{df.vol}
#' Audio volume data. There are two microphones in each badge, front- and back.
#' \describe{
#'   \item{Timestamp}{Date time object}
#'   \item{Badge.ID}{Badge id}
#'   \item{Volume}{Volume reading}
#'   \item{Source}{VOL_F or VOL_B for front and back microphone source}
#' }
#'
#' \cr\cr
#'
#' \strong{pitch}
#' Audio pitch data.
#' \describe{
#'   \item{Timestamp}{Date time object}
#'   \item{Badge.ID}{Badge id}
#'   \item{Pitch}{Pitch measure in Hz}
#'   \item{Volume}{Volume }
#'   \item{Source}{VOL_F or VOL_B for front and back microphone source}
#' }
#'
#'
NULL
