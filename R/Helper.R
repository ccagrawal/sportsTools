.ToSeconds <- function(x){
  if (!is.character(x)) stop("x must be a character string of the form H:M:S")
  if (length(x)<=0)return(x)
  
  unlist(
    lapply(x,
           function(i){
             i <- as.numeric(strsplit(i,':',fixed=TRUE)[[1]])
             if (length(i) == 3) 
               i[1]*3600 + i[2]*60 + i[3]
             else if (length(i) == 2) 
               i[1]*60 + i[2]
             else if (length(i) == 1) 
               i[1]
           }  
    )  
  )  
}

.CapLetters <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = '', collapse = ' ')
}

.YearToSeason <- function(x) {
  paste0(x - 1, '-', substring(x, 3, 4))
}

.CurrentYear <- function() {
  if (as.numeric(format(Sys.Date(), "%m")) >= 11) {
    return(as.numeric(format(Sys.Date(), "%Y")) + 1)
  } else {
    return(as.numeric(format(Sys.Date(), "%Y")))
  }
}