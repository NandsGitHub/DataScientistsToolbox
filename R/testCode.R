myFunction <- function(){
x <- rnorm(10)
mean(x)
}

myFunction1 <- function(x){
x + rnorm(length(x))
}