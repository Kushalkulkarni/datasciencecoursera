rankall <- function(condition, rank = "best") {
        outcome_dt <- read.csv("C:/Users/Admin/Desktop/R Course/Assignment 3/outcome-of-care-measures.csv", 
                               na.strings = "Not Available", stringsAsFactors = FALSE)
        outcome_less <- outcome_dt[, c(2, 7, 11, 17, 23)]
        conditions <- c('heart attack'=3, 'heart failure'=4, 'pneumonia'=5)
        #Checking validity of the arguments
        if ((condition %in% names(conditions)) == FALSE) {
                stop("Invalid Condition")
        } else if (!(rank == "best" || rank == "worst" || class(rank) == "numeric")){
                stop("Invalid Rank")
        } else {
                #If rank is "best" or numeric
                if (!(rank == "worst")) {
                        #Ordering Ascending
                        ordered <- outcome_less[order(outcome_less$State, #1 State
                                                      outcome_less[, conditions[condition]], #2 Condition
                                                      outcome_less$Hospital.Name), ] #3 Hospital Name
                        #Splitting
                        splitBYstate <- split(ordered, ordered$State)
                        #lapply
                        if (rank == "best") {
                                hospName <- unlist(lapply(splitBYstate, function(RankRow) RankRow[1, 1]))
                        } else {
                                hospName <- unlist(lapply(splitBYstate, function(RankRow) RankRow[rank, 1]))
                        }
                } else if (rank == "worst") {
                        #Ordering Descending (to pick first row of every state)
                        ordered <- outcome_less[order(outcome_less$State, #1 State
                                                      outcome_less[, conditions[condition]], #2 Condition
                                                      outcome_less$Hospital.Name,  #3 Hospital Name
                                                      decreasing = TRUE), ]
                        #Splitting
                        splitBYstate <- split(ordered, ordered$State)
                        #lapply
                        hospName <- unlist(lapply(splitBYstate, function(RankRow) RankRow[1, 1])) #Here, 1st row is last rank
                }
                #Converting to dataframe
                matx <- matrix(hospName, ncol = 1, byrow = TRUE)
                #state matrix
                us <- unique(outcome_less$State)
                matus <- matrix(sort(us), ncol = 1, byrow = TRUE)
                #join both matrices after sorting "us"
                matfin <- cbind(matx, matus)
                #convert the combined matrix into a dataframe
                fin <- as.data.frame(matfin, stringsAsFactors = FALSE)
                names(fin) <- c("Hospital", "State")
                fin #OUTPUT
        }
}