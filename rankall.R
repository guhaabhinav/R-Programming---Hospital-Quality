rankall <- function(outcome, num = "best") {
        ## Read outcome data
        data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
        srt_dat <- NULL
        
        if(!((outcome == "heart attack") | (outcome == "heart failure") | (outcome == "pneumonia"))){
                stop("invqalid outcome")
        }
        ## print(state)
        
        ## Return hospital name in that state with lowest 30-day death rate
        
        
        ## 11 - heart attack
        if (outcome == "heart attack") {outcome_col <- 11}
        ## 17 - heart faiure
        if (outcome == "heart failure") {outcome_col <- 17}
        ## 23 - pneumonia 
        if (outcome == "pneumonia") {outcome_col <- 23}
        
        ## new data frame - hospital, outcome, state
        subset_dat <- data.frame(hospital = data[, 2], outcome = data[,outcome_col], state = data$State, stringsAsFactors=FALSE) 
        
        ## all states, segregated into separate data frames 
        all_states_charvec <- sort(unique(data$State))
        sep_list <- NULL
        for(i in all_states_charvec){
                sep_list[which(all_states_charvec==i)] <- list(subset_dat[subset_dat$state==i,])
        }
        
        ## converting outcome columns to numeric, remove NAs, and sort ordering
        for (i in 1:length(all_states_charvec)) {
                sep_list[[i]][,2] <- as.numeric(sep_list[[i]][,2])
                sep_list[[i]] <- sep_list[[i]][complete.cases(sep_list[[i]]), ]
                sep_list[[i]] <- sep_list[[i]][order(sep_list[[i]]$outcome, sep_list[[i]]$hospital), ]
        }
        
        if(num == "best" ){
                for (i in 1:54) {srt_dat <- rbind(srt_dat,sep_list[[i]][1,]) }
                return(srt_dat)
        }else if(num == "worst"){
                for (i in 1:54) {srt_dat <- rbind(srt_dat,sep_list[[i]][dim(sep_list[[i]])[1],]) }
                return(srt_dat) 
        
        }else
                
                for (i in 1:54) {
                        
                        if((is.integer(num)) & (num > dim(sep_list[[i]])[1]))
                                {sep_list[[num]]<-c(NA,NA,all_states_charvec[i])}
                        srt_dat <- rbind(srt_dat,sep_list[[i]][num,]) 
                        
                }
                
        
        ## quick fix; due to NA issue in 3rd column 
                srt_dat[,3] <- all_states_charvec 
                return(srt_dat) 
        
        ##### works for best, and worst...need to fix for arbitrary rank (work in progress)
        
}