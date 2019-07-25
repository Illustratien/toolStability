# definied the global variable just for checking purpose when check the package
utils::globalVariables(c("X", "Xi.bar","Xj.bar","Genotype","","."),
                       package="toolStability")
#' @title Stability variance
#'
#' @description
#' \code{stab_var} calculate variance of a genotype across environments.
#'
#' @details
#' This function calculate the unbiased estimation of genotypic and environmental \cr
#' interaction. The stability variance is a linear combination of the ecovalence.
#'
#' \deqn{\sigma^{2}_{i} = \frac{1}{(G-1)(G-2)(E-1)}\big[
#' \Simga_{j}(X_{ij} -bar(X_{i.})_{i.})^2-\Simga_{i}\Simga_{j}(X_{ij} -bar(X_{i.})_{i.})^2
#' \big]}
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{bar(X)_{i.}} and  \eqn{bar(X)_{.j}} \cr
#' denoting marginal means of genotype i and environemnt j,respectively. \cr
#' \eqn{bar(X)_{..}} denote the overall mean of X.
#'
#'
#' @param data a dataframe containing at least three column of trait of interest,
#'  genotype, and environment information.
#' @param trait the interested variable to be analysize.
#' @param Genotype a variable labeling different genotypic varieties
#' @param Environment a variable lableing different environmental parameters
#' @export
#' @importFrom magrittr %>%
#' @return a dataframe with stability variance indices
#' @author Tien Cheng Wang (\email{tien.wang@@gem.uni-hannover.de})
#' @references Shukla, G. K. "Some statistical aspects of partitioning genotype environmental components of variability."
#'  \emph{Heredity} 29.2 (1972): 237-245.
#'
#' @examples
#' data(Data)
#' stab_variance <- stab_var(Data,'Yield','Genotype','Environment')
#'
stab_var <- function(data,trait,Genotype,Environment){
# preprocessed the raw data
Data <- data_prep(data,trait,Genotype,Environment)
X..bar=mean(Data$X)                   # overall mean of X
G <- length(unique(Data$Environment)) # number of genotypes
E <- length(unique(Data$Genotype))    # number of environments

Data<- Data%>%
  dplyr::select(X,Xi.bar,Xj.bar,Genotype)%>%
  dplyr::group_by(Genotype)%>%
  dplyr::mutate(.,sqr=(X-Xi.bar-Xj.bar+X..bar)^2)%>%
  dplyr::summarise(wi= sum(sqr, na.rm=TRUE))%>%
  dplyr::mutate(msq=sum(wi,na.rm = TRUE))%>%# mean square of GE interactions
  dplyr::mutate(.,stab.var=sqrt((G*(G-1)*wi-msq)/((E-1)*(G-2)*(G-2))))%>%
  dplyr::select(Genotype,stab.var)

return(Data)
}
