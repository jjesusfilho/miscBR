#' Function cnpj
#'
#' This function returns information about Brazilian companies based on tax number
#' @param cnpj vector with Brazilian companies tax numbers
#' @param latlong boolean. Default TRUE, it also returns the latlong
#' @keywords cnpj, tax number, address, latlong
#' @return A data.frame with companies' information
#' @examples
#' cnpj("60.746.948.0001-12")
#' cnpj("60746948000112")
#' @export
cnpj<-function(cnpj,latlong=TRUE){
  df <- cnpj %>% 
    purrr::map_dfr(~{
      .x %>% 
        stringr::str_replace_all("\\D+","") %>% 
        stringr::str_trim() %>% 
        stringr::str_pad(8,"left",0) %>% 
      paste0("http://receitaws.com.br/v1/cnpj/",.) %>% 
        httr::GET() %>% 
        httr::content() %>% 
        unlist() %>% 
        t() %>% 
        tibble::as_tibble()
    })
  df2<- df %>%
    select(cep,logradouro,numero,complemento, bairro,municipio,uf) %>% 
    purrr::pmap_dfr(~{
      .x %>% 
        t() %>% 
        toString() %>% 
        ggmap::geocode()
    })
  df<-cbind(df,df2)
}


