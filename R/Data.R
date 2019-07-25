#' Wheat APSIM model data set
#'
#' Wheat APSIM model simulated database
#'
#' @docType data
#'
#' @usage data(Data)
#'
#' @format dataframe
#'
#' @keywords datasets
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{10.1371/journal.pone.0146385}{toolStability}
#'
#'
#' @examples
#' data(Data)
#' \donttest{boxplot(Yield~Genotype,data=Data,
#' col='lightblue',border='darkblue')}
"Data"
