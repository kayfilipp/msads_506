get_overfit <- function(coefficients){
  
  #reduce coefficient names to ar, ma, and in instead of ma1,ar1,etc.
  trimmed_names = substr(names(coefficients),1,2)
  
  #get an index of all ar coefficients and all ma coefficients.
  ar_indices = which(trimmed_names=="ar")
  ma_indices = which(trimmed_names=="ma")
  
  #separate the ar and ma coefficients into two lists, add 1.
  ar = c(1,coefficients[ar_indices])
  ma = c(1,coefficients[ma_indices])
  
  a = as.character(polyroot(ar))
  b = as.character(polyroot(ma))
  
  #return if their polyroots overlap at all.
  return(
    intersect(a,b)
  )
}

#mock up data - this should return a
x = rnorm(150,mean=5)
y = arima(x,order=c(2,0,1),fixed = c(-0.3,-0.4,0.5,0))
coefficients = y$coef 

get_overfit(coefficients)
