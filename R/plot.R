plot <- function(x){
  UseMethod("plot",x)
}

plot.default <- function(x){
  return(x)
}

#' @title Plot sociometric data
#'
#' @description Plot interaction data
#'
#' @param df Data frame of class "interact", "smtrx"
#'
#' @return A graphics object
#'
#' @export
plot.interact <- function(df){

  print("in")

  # g <- ggplot(df, aes(x=Timestamp, y=Badge.ID)) +
  #   geom_point()
  #
  # g

}
