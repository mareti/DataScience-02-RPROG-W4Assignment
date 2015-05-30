## 
## 

best <- function(state=NA, outcome=NA) {
    ## Read outcome data 
    data <- read.csv("outcome-of-care-measures.csv"
                     , na.strings="Not Available"
                     #, colClasses="character"
                     )
    
    ## Check that state and outcome are valid
    # if (is.na(state) || nchar(state)>2) {
    if (state %in% data$State == FALSE) {
        stop("invalid state")
        # print("invalid state")
        # err = err + 1
    }
    
    if (outcome=="heart failure")
        i = 17
    else if (outcome=="heart attack") 
        i = 11
    else if (outcome=="pneumonia")
        i = 23
    else { 
        # err = err + 2
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death rate
    y = subset(data.frame(data[c(2,7,i)]), data$State==state)
    z = y[ which(y[3]==min(y[3], na.rm=TRUE)), ]
    z = z[order(z[1])]
    # z$Hospital.Name
    as.character(print(z[1,], row.names=FALSE, max.levels=0))
}