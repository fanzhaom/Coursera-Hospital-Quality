best <- function(state, outcome) {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character",
                             na.strings = "Not Available")
    
    ## Check that state and outcome are valid
    if (!state %in% outcome_data[, 7]) {
        stop("invalid state")
    }
    conditions <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% conditions) {
        stop("invalid outcome")
    }
    ## Return hospital name in that state with lowest 30-day death rate
    data <- subset(outcome_data, State == state)
    flag <- 0
    if (outcome == "heart attack") {
        flag <- 11
    }else if (outcome == "heart failure") {
        flag <- 17
    }else {
        flag <- 23
    }
    
    data <- data[, c(2, 7, flag)]
    ## rename the column
    names(data)[3] <- "Rate"
    ## change the rate column to numeric
    ## ignore the error msg
    data[, 3] <- as.numeric(data[, 3])
    ## order the data based on death rate
    data <- data[order(data$Rate, data$Hospital.Name) ,]
    data[1, 1]
}