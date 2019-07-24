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
#'
#' @references Casadebaig P, Zheng B, Chapman S, Huth N, Faivre R, et al. (2016) Plosone 11(1), e0146385
#' (\href{https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0146385}{Plosone})
#'
#'
#' @examples
#' data(Data)
#' \donttest{boxplot(Yield~Genotype,data=Data,
#' col='lightblue',border='darkblue')}
"Data"
