  rankhospital <- function(state,outcome,rank="best"){
    data <- read.csv("outcome-of-care-measures.csv",colClasses = "character",header = TRUE)
    putdata <- as.data.frame(cbind(data[,2],
                                   data[,7],
                                   data[,11],
                                   data[,17],
                                   data[,23]))
    colnames(putdata) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
    
    if(!state %in% putdata[,"state"]){
      stop("Invalid State")
    }
    else if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
      stop("Invalid Outcome")
    }
    else if(is.numeric(rank)){
      fi <- which(putdata[,"state"]==state)
      fs <- putdata[fi,]
      fs[,outcome] <- as.numeric(fs[,outcome])
      
      fs <- fs[order(fs[,outcome],fs[,"hospital"]),]
      output <- fs[, "hospital"][rank]
    }
    else if(!is.numeric(rank)){
      if(rank=="best"){
        output <- best(state,outcome)
      }
      else if(rank=="worst"){
        fi <- which(putdata[,"state"]==state)
        fs <- putdata[fi,]
        fs[,outcome] <- as.numeric(fs[,outcome])
        
        fs <- fs[order(fs[,outcome],fs[,"hospital"],decreasing = TRUE),]
        output <- fs[, "hospital"][1]
      }
      else{
        stop("Invalid rank")
      }
    }
    return(output)
  }