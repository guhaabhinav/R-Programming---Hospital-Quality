best <- function(state, outcome) {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        
        ## Check that state and outcome are valid
        flag <- 0
        all_states <- unique(data[,7])
        for (i in 1:length(all_states)){
                if (state == all_states[i]){
                        flag <- 1
                } 
        } 
        if(flag == 0 ){
                stop("incorrect state entry")
        }
        if(!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia"))){
                stop("incorrect outcome entry")
        }
               ## print(state)
        
        ## Return hospital name in that state with lowest 30-day death rate
        
        hospital_list <- data[data$State==state, 2] ## hospitals in the state; 2nd col - Hospital.Name
        
        ## 11 - heart attack
        if (outcome == "heart attack") {outcome_col <- 11}
        ## 17 - heart faiure
        if (outcome == "heart failure") {outcome_col <- 17}
        ## 23 - pneumonia 
        if (outcome == "pneumonia") {outcome_col <- 23}
        
        outcome_list <- data[data$State==state, outcome_col]
        outcome_list <- as.numeric(outcome_list)
        ## combine hospital names and outcomes in a list
        ## and get the indices of the min values in a vector
        hospital_outcome_list <- list(hospital_list, outcome_list)
        index_mins <- which(outcome_list == min(outcome_list, na.rm = TRUE))
        hospital_ties <- NULL
        ##character vector of hospital names with min mortality
        for(i in index_mins){ hospital_ties <- c(hospital_ties,hospital_outcome_list[[1]][i])} 
        best_hos <- sort(hospital_ties)
        best_hos[1]
       
        
        
}