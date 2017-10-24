#' Function via_cep
#'
#' This function returns address and latlong based on Brazilian postal code
#' @param cep vector with Brazilian postal codes
#' @param latlong boolean. Default=TRUE, it also returns the latlong
#' @keywords via_cep code, cep, latlong
#' @return A data.frame with the addresses and latlongs
#' @examples
#' via_cep(02930000)
#' via_cep("02929020")
#' via_cep("02846-000")
#' @export
via_cep<-function(cep,latlong=TRUE){
  cep %<>%stringr::str_replace("\\D","") %>% 
    stringr::str_pad(8,side="left",pad="0") %>% 
    as.character()
  df<-cep %>% 
    purrr::map_dfr(~{
      paste0("https://viacep.com.br/ws/",.x,"/json/") %>% 
        httr::GET() %>% 
        httr::content()
    })
  
  df2<-df %>% 
    purrr::pmap_dfr(~{
      .x %>% 
        t() %>% 
        toString() %>% 
        ggmap::geocode()
    })
  df<-cbind(df,df2)
  df
}


