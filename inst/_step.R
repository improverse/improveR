stepDf <- NULL
this <- NULL

workflow <- new.env()
lineage <- new.env()
usage <- new.env()
parent <- new.env()
children <- new.env()
lineage[["load"]]<- function() {}
usage[["load"]]<- function() {}
parent[["load"]]<- function() {}
children[["load"]]<- function() {}
