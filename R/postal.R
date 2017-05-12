#' Function postal
#'
#' This function return address based on Brazilian postal code
#' @param vector with Brazilian postal codes
#' @keywords postal code, cep
#' @import httr
#' @import stringr
#' @return A data.frame with the addresses
#' @examples
#' postal(02930000)
#' postal("02929020")
#' postal("02846-000")
#' @export
postal<-function(cep){
  cep<-str_replace(cep,"\\D","")
  cep<-str_pad(cep,8,side="left",pad="0")
  cep<-as.character(cep)
  df <- as.data.frame(setNames(replicate(6,numeric(0),
                                         simplify = F),
                               c("cep","tipoDeLogradouro","logradouro","bairro", "cidade", "estado")))
  for(i in seq_along(cep)){
    url<-paste0("http://correiosapi.apphb.com/cep/",cep)
    a<-GET(url[i])
    b<-content(a,as="parsed")
    b<-t(unlist(b))
    df<-rbind(df,b)
  }
  return(df)
}


