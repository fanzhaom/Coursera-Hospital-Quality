rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcome_data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that outcome is valid
    conditions <- c("heart attack", "heart failure", "pneumonia")
    if (!outcome %in% conditions) {
        stop("invalid outcome")
    }
    
    ## For each state, find the hospital of the given rank
    
    ## Leave only the data columns we need according to the outcome
    flag <- 0
    if (outcome == "heart attack") {
        flag <- 11
    }else if (outcome == "heart failure") {
        flag <- 17
    }else {
        flag <- 23
    }
    data <- outcome_data[, c(2, 7, flag)]
    ## Change the third column name to "Rate"
    names(data)[3] <- "Rate"
    ## Change the Rate column to numeric
    ## ignore the warning on NA coercion
    data[, 3] <- as.numeric(data[, 3])
    
    ## get rid of rows with rate as NA
    data <- data[!is.na(data$Rate) ,]
    
    ## split data based on state
    data_split <- split(data, data$State)
    
    result <- sapply(data_split, function(x, num) {
        x <- x[order(x$Rate, x$Hospital.Name) ,]
        if (num == "best") {
            return(x$Hospital.Name[1])
        }else if (num == "worst") {
            return(x$Hospital.Name[nrow(x)])
        }else {
            return(x$Hospital.Name[num])
        }
    }, num)
    data.frame(hospital = result, state = names(result))
}