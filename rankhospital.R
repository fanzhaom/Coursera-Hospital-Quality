rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
   
     ## Check that state and outcome are valid
    if (!state %in% outcome_data[, 7]) {
        stop("invalid state")
    }
    conditions <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% conditions) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with the given rank 30-day death rate
    data <- subset(outcome_data, State == state)
    flag <- 0
    if (outcome == "heart attack") {
        flag <- 11
    }else if (outcome == "heart failure") {
        flag <- 17
    }else {
        flag <- 23
    }
    ## Leave only the hospital name and the outcome we need for ranking
    data <- data[, c(2, flag)]
    ## Change the outcome column to numeric
    data[, 2] <- as.numeric(data[, 2])
    ## Reorder data, get rid of NA
    data_reorder <- data[order(data[, 2], data[, 1], na.last = NA) ,]
    ## Rename the column name for better display
    names(data_reorder) <- c("Hospital Name", "Rate")
    ## Add a new column for rank
    data_reorder$Rank <- seq.int(nrow(data_reorder))
    ## Get the result based on the rank num input
    if (num == "best") {
        num <- 1
    }else if (num == "worst") {
        num <- nrow(data_reorder)
    }else if (num > nrow(data_reorder)) {
        NA 
    }
    result <- subset(data_reorder, data_reorder$Rank == num)
    result[1, 1]
}