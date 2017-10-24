#' Function number_br
#'
#' Converts strings to numbers to English format.
#' @param str vector of numbers in character strings's format
#' @keywords numbers
#' @import stringr
#' @return numeric vector
#' @export
number_br<-function(str){
  num<-str %>% 
  str_replace_all("\\.","") %>% 
  str_replace_all(",",".") %>% 
  str_trim() %>% 
  as.numeric()
  num
}
#' @examples
#' number_br(c("345.545,00","299,24"))


