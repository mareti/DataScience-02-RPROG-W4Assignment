rankall <- function(outcome=NA, num="best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv"
                     , na.strings="Not Available"
                     #, colClasses="character"
                     )
    
    ## Check that state(?) and outcome are valid
    if (outcome=="heart failure")       i = 17
    else if (outcome=="heart attack")   i = 11
    else if (outcome=="pneumonia")      i = 23
    else stop("invalid outcome")
    
    ## For each state, find the hospital of the given rank
    y = subset( data.frame( data[ c(2,7,i) ] ) )
    colnames(y) = c("hospital", "state", "rate")

    if (num=="best") { 
        num = 1
        y = y[ order( y[3], y[1], na.last=TRUE, decreasing=FALSE ), ]
    }
    else if (num=="worst") {
        num = 1
        y = y[ order( -y[3], y[1], na.last=FALSE, decreasing=FALSE ), ]
    }
    else {
        y = y[ order( y[3], y[1], na.last=TRUE, decreasing=FALSE ), ]
    }
    
    y$rank = ave(y$rate, y$state
                 , FUN = function(x) rank(x, ties.method="first"))
    
    y = subset(y, y$rank == num)
    
    ## Return a data frame with the hospital names and the state name
    # print(y[num,1], row.names=FALSE, max.levels=0)
    y[, 1:2]
}