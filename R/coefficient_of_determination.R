utils::globalVariables(c("Bi", "Bi1", "Bi2", "E", "Environment", "Genotype", "Mean.Yield", "Mj", "X", "Xi.bar", "Xj.bar", "Xj.max", "corrected.X", "corrected.rank", "dev", "deviation", "mean.rank", "s2d1", "s2d2", "s2di", "s2xi", "sqr", "sqr1", "wi"))
#' @title Coefficient of determination
#'
#' @description
#' \code{coefficient_of_determination} calculate variance of a genotype across environments.
#'
#' @keywords regression approach
#'
#' @details
#' Coefficient of determination (Pinthus, 1976) is calculatd based on regression function.
#' Variety with low coefficient of determination is considered as stable.
#' Equation of coefficient of determination can be found in vignette file.
#'
#' @param data a dataframe containing trait, genotype and environment.
#' @param trait colname of a column containing a numeric vector of interested trait to be analysized.
#' @param genotype colname of a column containing a character or factor vector labeling different genotypic varieties
#' @param environment colname of a column containing a character or factor vector labeling different environments
#'
#' @return a data table with coefficient of determination
#'
#' @author Tien-Cheng Wang
#'
#' @references
#' \insertRef{pinthus1973}{toolStability}
#'
#' @importFrom dplyr group_by summarise mutate
#' @importFrom data.table data.table
#' @importFrom Rdpack reprompt
#'
#' @export
#'
#' @examples
#' data(Data)
#' coef.of.determination <- coefficient_of_determination(
#'  data = Data,
#'  trait = "Yield",
#'  genotype = "Genotype",
#'  environment = "Environment")
coefficient_of_determination <- function(data, trait, genotype, environment) {
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
  # overall mean of X
  X..bar <- mean(Data$X)
  # calculate doefficient determination
  res <- summarise(
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
      Bi2 = (Xj.bar - X..bar)^2,
      dev = ((X - Xi.bar)^2) / (E - 1)
    ),
    !!varnam := mean(X),
    Bi = 1 + (sum(Bi1, na.rm = TRUE) / sum(Bi2, na.rm = TRUE)),
    s2di = sum(s2d1, na.rm = TRUE) - ((Bi - 1)^2) * sum(s2d2, na.rm = TRUE),
    s2xi = sum(dev, na.rm = TRUE),
    coefficient.of.determination = 1 - (s2di / s2xi)
  ) # final product

  return(res[, c("Genotype",varnam, "coefficient.of.determination")])
}
