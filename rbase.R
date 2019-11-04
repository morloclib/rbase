library(jsonlite)
library(data.table)

# Generic pack/unpack functions
packGeneric   <- toJSON
unpackGeneric <- function(s) {fromJSON(as.character(s))}

# Extract R objects from JSON text
unpackDataFrame <- function(json){ as.data.frame(fromJSON(json)) }
unpackDataTable <- function(json){ as.data.table(fromJSON(json)) }
unpackCharacter <- fromJSON
unpackNumeric   <- fromJSON
unpackLogical   <- fromJSON
unpackList <- function(json) {fromJSON(json, simplifyVector = FALSE)}
unpackMatrix <- fromJSON

unpackString <- fromJSON
unpackInt <- fromJSON
unpackNum <- fromJSON
unpackBool <- fromJSON

packString <- function(x) toJSON(x, auto_unbox=TRUE)
packInt <- function(x) toJSON(as.integer(x), auto_unbox=TRUE)
packNum <- function(x) toJSON(as.numeric(x), auto_unbox=TRUE)
packBool <- function(x) toJSON(as.logical(x), auto_unbox=TRUE)

# Serialize R objects into JSON text
packDataFrame <- function(json){ toJSON(json, dataframe="columns") }
packDataTable <- packDataFrame
packCharacter <- function(x) toJSON(as.character(x), auto_unbox=TRUE)
packNumeric   <- function(x) toJSON(as.numeric(x), auto_unbox=TRUE)
packInteger   <- function(x) toJSON(as.integer(x), auto_unbox=TRUE)
packLogical   <- function(x) toJSON(as.logical(x), auto_unbox=TRUE)
packList      <- toJSON
packMatrix    <- toJSON
