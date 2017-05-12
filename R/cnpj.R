#' Funtion cnpj
#'
#' This function returns information about Brazilian companies based on tax number
#' @param vector with Brazilian companies tax numbers
#' @keywords cnpj, tax number
#' @import jsonlite
#' @import stringr
#' @import plyr
#' @return A data.frame with companies' information
#' @examples
#' cnpj("60.746.948.0001-12")
#' cnpj("60746948000112)
#' @export
cnpj<-function(cnpj){
  cnpj<-str_replace_all(cnpj,"\\D+","")
  cnpj<-str_trim(cnpj)
  cnpj<-str_pad(cnpj,8,"left",0)
  df <- as.data.frame(setNames(replicate(30,numeric(0), simplify = F), 
                               c("atividade_principal.text","atividade_principal.code","data_situacao","nome","uf","telefone","email","atividades_secundarias.text1","atividades_secundarias.text2","atividades_secundarias.code1","atividades_secundarias.code2","situacao","bairro","logradouro","numero","cep","municipio","abertura","natureza_juridica","fantasia","cnpj","ultima_atualizacao","status","tipo","complemento","efr","motivo_situacao","situacao_especial","data_situacao_especial","capital_social")))
  for (i in seq(cnpj)){
    tryCatch({
      d<-paste0("http://receitaws.com.br/v1/cnpj/",cnpj[i])
      e<-jsonlite::fromJSON(d)
      f<-as.data.frame(t(unlist(e)),stringsAsFactors = F)
      df<-plyr::rbind.fill(df,f)
    }, error=function(m){
      m
    }, finally={
      next
    })
    Sys.sleep(.1)
    
  }
  return(df)
}


