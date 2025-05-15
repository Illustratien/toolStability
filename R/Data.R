#' @title Wheat APSIM model simulated database
#'
#' @keywords datasets
#'
#' @docType data
#'
#' @usage data(Data)
#'
#' @description Multi-environment trail evaluating 5 genotypes in 4 locations for 4 years, with 2 nitrogen application rates, 2 sowing dates, and 2 CO2 levels of treatments (Casadebaig et al., 2016).
#'
#' @format A dataframe with 640 observations on the following 8 variables.
#' \itemize{
#' \item{Yield:       kg*ha^-1.}
#' \item{Genotype:    genotypes, 5 varieties.}
#' \item{Environment: 128 unique combination of environments for each genotype.}
#' \item{Year:        4 years.}
#' \item{Sites:     4 locations.}
#' \item{Nitrogen:    2 nitrogen application levels.}
#' \item{CO2:        2 CO2 concentration levels.}
#' \item{Sowing:     2 sowing dates.}
#'  }
#'
#' @importFrom Rdpack reprompt
#' @references
#' \insertRef{casadebaig2016}{toolStability}
#'
#'
#' @examples
#' data(Data)
#' \donttest{
#' ggplot2::ggplot(Data,ggplot2::aes(x=Sites,y=Yield,col=Genotype))+
#'   ggplot2::geom_boxplot()+
#'   ggplot2::facet_grid(Sowing~Nitrogen,labeller =ggplot2::label_both)+
#'   ggplot2::ylab(bquote('Wheat yield (ton' %.%'ha'^'-1'*')'))
#' }
"Data"
