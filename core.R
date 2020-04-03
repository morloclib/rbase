

map <- function(f, xs){
  sapply(xs, f)  
}

zipWith <- function(f, xs, ys){
  N <- min(length(xs), length(ys))
  zs <- as.list(rep(NA, N))
  for(i in seq_along(xs)){
    zs[[i]] <- f(xs[[i]], ys[[i]])
  }
  return(zs)
}

enumerateWith <- function(f, xs){
  ys <- list()
  for(i in seq_along(xs)){
    ys[[i]] <- f(xs[[i]], i)
  }
  return(ys)
}

fold <- function(f, init, xs){
  Reduce(f=f, x=xs, init=init, accumulate=FALSE)
}

scan <- function(f, init, xs){
  Reduce(f=f, x=xs, init=init, accumulate=TRUE)
}

add <- function(x,y) x + y
sub <- function(x,y) x - y
mul <- function(x,y) x * y
div <- function(x,y) x / y

plotPDF <- function(x, filename){
  pdf(filename)
  plot(x)
  dev.off()
}
