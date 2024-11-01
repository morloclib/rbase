# create a list from a pair of key/value lists
morloc_packMap <- function(d) {
  keys <- unlist(d[[1]])
  vals <- as.list(d[[2]])
  names(vals) <- keys
  vals
}

# create a pair of key/value lists from a named list
morloc_unpackMap <- function(xs){
  keys <- names(xs)
  vals <- unlist(xs)
  list(keys,vals)
}


morloc_packUnit <- function(x) {
  NULL
}

morloc_unpackUnit <- function(x){
  1
}
