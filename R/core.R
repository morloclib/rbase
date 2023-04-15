mlc_fst <- function(x) x[[1]]
mlc_snd <- function(x) x[[2]]
mlc_thr <- function(x) x[[3]]

mlc_run <- function(f){
    f()
}

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
mlc_neg <- function(x) (-1) * x

plotPDF <- function(x, filename){
  pdf(filename)
  plot(x)
  dev.off()
  NULL
}

mlc_plotVectorPDF <- function(...){
  plotPDF(...)
}



mlc_gt <- function(x, y){
	x > y
}

mlc_lt <- function(x, y){
	x < y
}

mlc_ge <- function(x, y){
	x >= y
}

mlc_le <- function(x, y){
	x <= y
}

mlc_eq <- function(x, y){
	x == y
}

mlc_ne <- function(x, y){
	x != y
}

mlc_not <- function(x){
	!x
}

mlc_and <- function(x, y){
	x && y
}

mlc_or <- function(x, y){
	x || y
}
