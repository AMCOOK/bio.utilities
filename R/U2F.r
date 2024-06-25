#' @title U2F 
#' @description exploitation rate to fishing mortality or fishing mortality to exploitation if inverse=T
#' @family abysmally documented
#' @author  unknown
#' @export
U2F <- function(U,inverse=F){
	if(!inverse) return(-log(1-U))
  if(inverse) return(1-exp(-U))
}