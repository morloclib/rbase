library(jsonlite)

# Generic pack/unpack functions
packGeneric   <- toJSON
unpackGeneric <- function(s) {fromJSON(as.character(s))}

packNull <- function(x) {force(x); "null"}
unpackNull <- function(x) {force(x); NULL}

# Extract R objects from JSON text

# scalar primitives
unpackScalarStr <- fromJSON
unpackScalarLog <- fromJSON
unpackScalarInt <- fromJSON
unpackScalarNum <- fromJSON
packScalarStr <- function(x) toJSON(x, auto_unbox=TRUE)
packScalarLog <- function(x) toJSON(as.logical(x), auto_unbox=TRUE)
packScalarInt <- function(x) toJSON(as.integer(x), auto_unbox=TRUE)
packScalarNum <- function(x) toJSON(as.numeric(x), auto_unbox=TRUE)

# vector primitives
packVectorStr <- function(x) toJSON(as.character(x), auto_unbox=FALSE)
packVectorLog <- function(x) toJSON(as.logical(x), auto_unbox=FALSE)
packVectorInt <- function(x) toJSON(as.integer(x), auto_unbox=FALSE)
packVectorNum <- function(x) toJSON(as.numeric(x), auto_unbox=FALSE)
unpackVectorStr <- fromJSON
unpackVectorLog <- fromJSON
unpackVectorInt <- fromJSON
unpackVectorNum <- fromJSON

# containers
packDataFrame <- function(json){ toJSON(json, dataframe="columns") }
packList      <- toJSON
packMatrix    <- toJSON
unpackDataFrame <- function(json){ as.data.frame(fromJSON(json)) }
unpackList <- function(json) {fromJSON(json, simplifyVector = FALSE)}
unpackMatrix <- fromJSON
