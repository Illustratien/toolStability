utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title Deviation mean squares
#'
#' @description
#' \code{deviation_mean_squares} calculate variance of a genotype across environments.
#'
#' @keywords regression approach
#'
#' @details
#' Deviation mean squares (Eberhart and Russell, 1966) is calculatd based on regression function.
#' Variety with low stability variance is considered as stable.
#' Equation of deviation mean squares can be found in vignette file.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#' @param unit.correct logical, default is \code{FALSE}, returning the stability index with unit equals to squared unit of trait; when \code{TRUE}, returning stability index with the unit as same as unit of trait.
#'
#' @return a data table with deviation mean squares
#'
#' @author Tien Cheng Wang
#'
#' @references
#' \insertRef{eberhart1966}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate mutate_at select rename
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' deviation.mean.squares <- deviation_mean_squares(
#'  data = Data,
#'  trait = "Yield",
#'  genotype = "Genotype",
#'  environment = "Environment",
#'  unit.correct = FALSE)
deviation_mean_squares <- function(data, trait, genotype, environment, unit.correct = FALSE) {
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
  X..bar <- mean(Data$X) # overall mean of X

  res <- dplyr::rename(
    dplyr::select(
    summarise(
    mutate(
      group_by(
        mutate(
          group_by(Data, Environment), # for each environment
          Xj.bar = mean(X)
        ), # first calculate environmental mean
        Genotype
      ), # for each genotype
      Xi.bar = mean(X), # then calculate genotypic mean
      E = length(X), # number of environment
      s2d1 = ((X - Xi.bar - Xj.bar + X..bar)^2) / (E - 2),
      s2d2 = ((Xj.bar - X..bar)^2) / (E - 2),
      Bi1 = (X - Xi.bar - Xj.bar + X..bar) * (Xj.bar - X..bar),
      Bi2 = (Xj.bar - X..bar)^2
    ),
    Mean.trait = mean(X),
    Bi = 1 + (sum(Bi1, na.rm = TRUE) / sum(Bi2, na.rm = TRUE)),
    deviation.mean.squares = sum(s2d1, na.rm = TRUE) - ((Bi - 1)^2) * sum(s2d2, na.rm = TRUE)),
    c('Genotype','Mean.trait','deviation.mean.squares')),
    varnam = 'Mean.trait')


  if (unit.correct==TRUE){
    res <- mutate_at(res,"deviation.mean.squares", sqrt)
  }

  return(res)
}
