sigmoid_xB <- function(x, B){
  T = (1/(1+((x/(1-x))^-B)))
  return(T)
}