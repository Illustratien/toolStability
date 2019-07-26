# definied the global variable just for checking purpose when check the package
utils::globalVariables(c("X", "Xi.bar","Genotype","d2","."),
                       package="toolStability")
#' @title Environmental variance
#'
#' @description
#' \code{env_var} is used to calculate variance of a genotype across environments.
#' @keywords static stability
#'
#' @details
#' Environmental variance (Roemer 1917) is calculated based on suming up all deviation from genotypic mean for each genotype.
#' The larger the environmental variance of one genotype is, the lower the stability.
#'
#' \deqn{S^{2}_{xi} = \frac{\sum_{j}(X_{ij} - \bar(X)_{i.})^2}{E-1} }
#' where \eqn{X_{ij}} is the observed phenotypic mean value of genotype i(i=1,..., G)
#' in environment j(j=1,...,E), with \eqn{bar(X)_{i.}} denoting marginal means of genotype i.
#' @param data a dataframe containing at least three column of trait of interest,
#'  genotype, and environment information.
#' @param trait the interested variable to be analysize.
#' @param Genotype a variable labeling different genotypic varieties
#' @param Environment a variable lableing different environmental parameters
#' @export
#' @importFrom magrittr %>%
#' @importFrom Rdpack reprompt
#' @return a dataframe with environmental variance indices
#' @author Tien Cheng Wang (\email{tien.wang@@gem.uni-hannover.de})
#' @references
#' \insertRef{roemer1917}{toolStability}
#' @examples
#' data(Data)
#' environmental_variance <- env_var(Data$'Yield',Data$Genotype,Data$environment)

  # preprocessed the raw data
  Data <- data_prep(data,trait,Genotype,Environment)
environmental_variance <- function(trait,genotype,...){
  if(!is.numeric(trait)){stop('Trait must be a numeric vector')}
  if(nargs()==2){# condition of only input trait and genotype
    # combine vectors into data table
    Data <- data.table(X=trait,Genotype=genotype)
    #calculate environmental variance
    res <- summarise(
           mutate(
           group_by(Data,Genotype),
            Xi.bar=mean(X),deviation=(X-Xi.bar)^2),#mutate
             env.var = sum(deviation, na.rm=TRUE)/(length(deviation)-1)#summarise
    )

    # res<- Data%>%
    #   group_by(Genotype)%>%
    #   mutate(.,Xi.bar=mean(X),deviation=(X-Xi.bar)^2)%>%
    #   summarise(env.var = sum(deviation, na.rm=TRUE)/(length(deviation)-1))
  }else{
    print('haha')
  }
  return(res)
}
