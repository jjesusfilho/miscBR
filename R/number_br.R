#' Function number_br
#'
#' Converts numbers to English format.
#' @param vector of numbers in format of character strings
#' @keywords numbers
#' @import stringr
#' @return numeric vector
#' @export

number_br<-function(str){
  str<-str_replace_all(str,"\\.","")
  str<-str_replace_all(str,",",".")
  str<-str_trim(str)
  num<-as.numeric(str)
  return(num)
}
#' @examples
#' number_br(c("345.545,00","299,24"))


