best <- function(state,outcome){
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character",header = TRUE)
  putdata <- as.data.frame(cbind(data[,2],
                                 data[,7],
                                 data[,11],
                                 data[,17],
                                 data[,23]))
  colnames(putdata) <- c("hospital","state","heart attack","heart failure","pneumonia")
  
  ## Valid state
  if(!state %in% putdata[,"state"]){
    stop("Invalid state")
  }
  else if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("Invalid Outcome")
  }
  else{
    fi <- which(putdata[,"state"]==state)
    fs <- putdata[fi,]
    oi <- as.numeric(fs[,outcome])
    
    min_val <- min(oi, na.rm = TRUE)
    result  <- fs[, "hospital"][which(oi == min_val)]
    output  <- result[order(result)]
  }
  return(output)
}