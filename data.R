morloc_packMap <- function(d) {
  xs <- d[[1]]
  ys <- d[[2]]
  lapply(seq_along(d[[1]]), function(i) list(xs[[i]], ys[[i]]))
}

morloc_unpackMap <- function(xs){
  ys <- lapply(xs, function(x) x[[1]])  
  zs <- lapply(xs, function(x) x[[2]])  
  list(ys,zs)
}

