#' Function xtsumR
#'
#' This function does the same as the xtsum funtion in Stata.
#' @param df data.frame 
#' @param columns vector with selected colums' numbers
#' @param individuals column where the individuals are 
#' @keywords summary, panel data
#' @import tibble
#' @import dplyr
#' @importFrom stats aggregate sd
#' @return A data.frame with the summary
#' @examples
#' df<-data.frame(a=rnorm(n=20,mean=23,sd=6),
#'     b=rnorm(n=20,mean=18,sd=7),
#'     uf=sample(c("SP","MS","RS","AM","SE"),20,replace=TRUE))
#' xtsumR(df,c(1,2),"uf")
#' @export
xtsumR<-function(df,columns,individuals){
  df<-df[order(individuals),]
  panel<-data.frame()
  for (i in columns){
    v<-df %>% dplyr::group_by() %>%
      dplyr::summarize(
        mean=mean(df[[i]]),
        sd=sd(df[[i]]),
        min=min(df[[i]]),
        max=max(df[[i]])
      )
    v<-tibble::add_column(v,variacao="overal",.before=-1)
    v2<-stats::aggregate(df[[i]],list(df[[individuals]]),"mean")[[2]]
    sdB<-sd(v2)
    varW<-df[[i]]-rep(v2,each=12) #
    varW<-varW+mean(df[[i]])
    sdW<-sd(varW)
    minB<-min(v2)
    maxB<-max(v2)
    minW<-min(varW)
    maxW<-max(varW)
    v<-rbind(v,c("between",NA,sdB,minB,maxB),c("within",NA,sdW,minW,maxW))
    panel<-rbind(panel,v)
  }
  var<-rep(names(df)[columns])
  n1<-rep(NA,length(columns))
  n2<-rep(NA,length(columns))
  var<-c(rbind(var,n1,n1))
  panel$var<-var
  panel<-panel[c(6,1:5)]
  names(panel)<-c("variable","variation","mean","standard.deviation","min","max")
  panel[3:6]<-as.numeric(unlist(panel[3:6]))
  panel[3:6]<-round(unlist(panel[3:6]),2)
  return(panel)
}
