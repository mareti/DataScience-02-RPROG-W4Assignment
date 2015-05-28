## 
## 

best <- function(state=NA, outcome=NA) {
    ## Read outcome data 
    data <- read.csv("outcome-of-care-measures.csv", colClasses="character")
    err = 0
    
    ## Check that state and outcome are valid
    if (is.na(state) || nchar(state)>2) {
        # stop("invalid state")
        # print("invalid state")
        err = err + 1
    }
    
    if (is.na(outcome)
        || !outcome==c("heart attack","heart failure","pneumonia")) {
        # stop("invalid outcome")
        # print("invalid outcome")
        err = err + 2
    }
    
    if (err == 1) 
        stop("invalid state")
    else if (err == 2)
        stop("invalid outcome")
    else if (err == 3)
        stop("invalid state and outcome")
    
    
    
    ## Return hospital name in that state with lowest 30-day death rate
    print("The function works... ish...")
    print(state)
    print(outcome)
}