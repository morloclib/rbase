library(jsonlite)

# Type specification syntax:
#   tuple
#   | record
#   | | a
#   | | | list
#   | | | | numeric
#   | | b
#   | | | logical
#
# # (Num, {a::[Num], b::Bool})
# list(
#   # tuple constructor
#   tuple=list(
#     list(scalar=list("numeric")),
#     list(record=list(
#         a=list(
#           list=list("numeric")
#         ),
#         b=list(
#           scalar=list("logical")
#         )
#       )
#     )
#   )
# )

pack.tuple <- function(x, t){
  stopifnot(length(x) == length(t))
  stopifnot(length(x) > 1)
  xs <- sapply(
    seq_along(x),
    function(i) {pack(x[[i]], t[[i]])}
  )
  paste0("[", paste0(xs, collapse=","), "]")
}

pack.record <- function(x, t){
  stopifnot(length(x) == length(t))
  stopifnot(length(x) > 1)
  xs <- sapply(
    seq_along(x),
    function(i) {paste0('"', names(t)[i], '"=', pack(x[[i]], t[[i]]))}
  )
  paste0("{", paste0(xs, collapse=","), "}")
}

pack.list <- function(x, t){
  if(t == "logical"){
    x <- ifelse(x, "true", "false")
  }
  paste0("[", paste0(x, collapse=","), "]")
}

pack.logical <- function(x, t){
  if(x){
    "true"
  } else {
    "false"
  }
}

pack.character <- function(x, t){
  paste0('"', x, '"')
}

pack.integer <- function(x, t) { x }

pack.numeric <- function(x, t) { x }

pack.null <- function(x, t){ force(t); "null" }
pack.NULL <- function(x, t){ force(t); "null" }

pack.data.frame <- function(x, t){
  toJSON(x, dataframe="columns")
}

pack.matrix <- function(x, t){
  toJSON(x)
}

pack <- function(x, t){
  if(is.null(names(t))){
    eval(parse(text=paste0("pack.", t)))(x, NULL)
  } else {
    eval(parse(text=paste0("pack.", names(t)[1])))(x, t[[1]])
  }
}



unpack.tuple <- function(x, t){ fromJSON(x) }
unpack.record <- function(x, t){ fromJSON(x) }
unpack.list <- function(x, t){ fromJSON(x) }
unpack.null <- function(x, t){ fromJSON(x) }
unpack.NULL <- function(x, t){ fromJSON(x) }
unpack.character <- function(x, t){ fromJSON(x) }
unpack.logical <- function(x, t){ fromJSON(x) }
unpack.integer <- function(x, t){ fromJSON(x) }
unpack.numeric <- function(x, t){ fromJSON(x) }
unpack.matrix <- function(x, t){ fromJSON(x) }
unpack.data.frame <- function(x, t){
  as.data.frame(fromJSON(x))
}

unpack <- function(x, t){
  if(is.null(names(t))){
    # damn this is evil, better way?
    eval(parse(text=paste0("unpack.", t)))(x, NULL)
  } else {
    eval(parse(text=paste0("unpack.", names(t)[1])))(x, t[[1]])
  }
}
