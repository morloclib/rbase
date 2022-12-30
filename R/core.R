mlc_id <- function(x){
  x
}

mlc_at <- function(i, xs){
  if (is.list(xs)){
    xs[[i]]
  } else {
    xs[i]
  }
}

mlc_map <- function(f, xs){
  sapply(xs, f)  
}

mlc_zipWith <- function(f, xs, ys){
  N <- min(length(xs), length(ys))
  zs <- as.list(rep(NA, N))
  for(i in seq_along(xs)){
    zs[[i]] <- f(xs[[i]], ys[[i]])
  }
  return(zs)
}

mlc_enumerateWith <- function(f, xs){
  ys <- list()
  for(i in seq_along(xs)){
    ys[[i]] <- f(xs[[i]], i)
  }
  return(ys)
}

mlc_fold <- function(f, init, xs){
  Reduce(f=f, x=xs, init=init, accumulate=FALSE)
}

mlc_scan <- function(f, init, xs){
  Reduce(f=f, x=xs, init=init, accumulate=TRUE)
}

mlc_add <- function(x,y) x + y
mlc_sub <- function(x,y) x - y
mlc_mul <- function(x,y) x * y
mlc_div <- function(x,y) x / y

plotPDF <- function(x, filename){
  pdf(filename)
  plot(x)
  dev.off()
  NULL
}

mlc_plotVectorPDF <- function(...){
  plotPDF(...)
}
