# I am a genric function. 
#-----------------------
# a has to be numeric
# b has to be numeric
# I will return the sum of both
genericFunction <- function(a, b){
  if (!(is.numeric(a) & is.numeric(b))){
    print('ERROR: either a or b is not numeric!')
    return()
  }
  else {
    c <- a + b
    return(c)
  }
}