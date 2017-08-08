rankhospital <- function(state, outcome, num = "best") {
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
        ## combine hospital names and outcomes in a data frame
        ## and remove na observations
        hospital_outcome_data <- data.frame(hospital_list, outcome_list)
        hospital_outcome_data <- hospital_outcome_data[complete.cases(hospital_outcome_data), ]
        ## sorted data frame by outcome column, then hospital alphabetic order
        srt_dat <- hospital_outcome_data[order(hospital_outcome_data$outcome_list, hospital_outcome_data$hospital_list), ]
        
        if(num == "best" ){
                print(as.character(srt_dat[1,1]))
        }else if(num == "worst"){
                print(as.character(srt_dat[which.max(srt_dat[,2]),1])) 
        }else if(num > dim(srt_dat)[1]){
                return(NA)
        }else
                print(as.character(srt_dat[num,1])) 
        
        
        
}