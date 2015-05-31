rankhospital <- function(state=NA, outcome=NA, num="best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv"
                     , na.strings="Not Available"
                     #, colClasses="character"
                     )
    
    ## Check that state and outcome are valid
    if (state %in% data$State == FALSE) {
        stop("invalid state")
    }
    
    if (outcome=="heart failure")       i = 17
    else if (outcome=="heart attack")   i = 11
    else if (outcome=="pneumonia")      i = 23
    else stop("invalid outcome")
    
    ## Return hospital name in that state with the given rank 30-day death rate
    y = subset(data.frame(data[c(2,7,i)]), data$State==state)
    colnames(y) = c("Name", "State", "Rate")
    y = y[ order( y[3], y[1], na.last=TRUE, decreasing=FALSE ), ]
    y$Rank = ave(y$Rate, FUN = function(x) rank(x, ties.method="first"))
    y = na.omit(y)
    
    if (num=="best") num=1
    if (num=="worst") num=max(y$Rank, na.rm=TRUE)
    
    as.character(print(y[num,1], row.names=FALSE, max.levels=0))
}