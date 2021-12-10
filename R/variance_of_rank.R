utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title variance of rank
#'
#' @description
#' \code{variance_of_rank} calculate variance of a genotype across environments.
#'
#' @keywords nonparametric approach
#'
#' @details
#' Variance of rank (Nassar and Huehn, 1987) is calculatd based on regression function.
#' Variety with low variance of rank is considered as stable.
#' Equation of variance of rank can be found in vignette file.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#' @param unit.correct logical, default is \code{FALSE}, returning the stability index with unit equals to squared unit of trait; when \code{TRUE}, returning stability index with the unit as same as unit of trait.#'
#' @return a data table with variance of rank
#'
#' @author Tien-Cheng Wang
#'
#' @references
#' \insertRef{nassar1987}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate mutate_at select rename
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' variance.of.rank <- variance_of_rank(
#'  data = Data,
#'  trait = "Yield",
#'  genotype = "Genotype",
#'  environment = "Environment",
#'  unit.correct = FALSE)
variance_of_rank <- function(data, trait, genotype, environment, unit.correct = FALSE) {
  if (!is.numeric(data[[trait]])) {
    stop("Trait must be a numeric vector")
  }
  # combine vectors into data table
  if (length(environment) == 1){
    Data <- data.table(X =  data[[trait]] ,
                       Genotype = data[[genotype]],
                       Environment = data[[environment]])

  }else { # if input is the vector containing the name that are going to combine in one column
    data$Environment <- interaction(data[,environment],sep = '_')

    Data <- data.table(X = data[[trait]] ,
                       Genotype = data[[genotype]],
                       Environment = data[['Environment']])
  }
  varnam <- paste0("Mean.",trait)
  X..bar <- mean(data[[trait]])

  res <- dplyr::select(
    summarise(
      group_by(
        mutate(
          group_by(
            mutate(
              group_by(Data, Genotype),
              corrected.X = X - mean(X) + X..bar
            ),
            Environment
          ),
          corrected.rank = rank(-corrected.X, na.last = "keep", ties.method = "min")
        ),
        Genotype
      ),
      Mean.trait = mean(X),
      mean.rank = mean(corrected.rank),
      variance.of.rank = sum((corrected.rank - mean.rank)^2 / (length(X) - 1))),
    c('Genotype','Mean.trait','variance.of.rank'))

  if (unit.correct==TRUE){
    res <- mutate_at(res,"variance.of.rank", sqrt)
  }
  names(res)[names(res) == "Mean.trait"] <- sprintf("Mean.%s", trait)
  return(res)
}
