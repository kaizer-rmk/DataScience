rankall <- function(outcome,num="best"){
  data <- read.csv("outcome-of-care-measures.csv",colClasses = "character",header = TRUE)
  putdata <- as.data.frame(cbind(data[,2],
                                 data[,7],
                                 data[,11],
                                 data[,17],
                                 data[,23]))
  colnames(putdata) <- c("hospital","state","heart attack","heart failure","pneumonia")
  putdata[,outcome] <- as.numeric(putdata[,outcome])
  
  if(!outcome %in% c("heart attack","heart failure","pneumonia")){
    stop("Invalid Outcome")
  }
  else if(is.numeric(num)){
    by_state <- with(putdata,split(putdata,state))
    ordered <- list()
    for(i in seq_along(by_state)){
      by_state[[i]] <- by_state[[i]][order(by_state[[i]][,outcome],by_state[[i]][,"hospital"]),]
      ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"][1])
    }
    result <- do.call(rbind, ordered)
    output <- as.data.frame(result, row.names = result[, 2], stringsAsFactors = FALSE)
    names(output) <- c("hospital", "state")
  }
  else if(!is.numeric(num)){
    if(num=="best"){
      by_state <- with(putdata,split(putdata,state))
      ordered <- list()
      for(i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][,outcome],by_state[[i]][,"hospital"]),]
        ordered[[i]]  <- c(by_state[[i]][num, "hospital"], by_state[[i]][, "state"])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    }
    else if(num=="worst"){
      by_state <- with(putdata,split(putdata,state))
      ordered <- list()
      for (i in seq_along(by_state)){
        by_state[[i]] <- by_state[[i]][order(by_state[[i]][,outcome], 
                                             by_state[[i]][, "hospital"], 
                                             decreasing = TRUE), ]
        ordered[[i]]  <- c(by_state[[i]][1, c("hospital", "state")])
      }
      result <- do.call(rbind, ordered)
      output <- as.data.frame(result, stringsAsFactors = FALSE)
      rownames(output) <- output[, 2]
    }
    else{
      stop("Invalid num")
    }
  }
  return(output)
}