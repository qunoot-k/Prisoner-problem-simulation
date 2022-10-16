# Contributors:
# Qunoot Khaleeq - s2314595

# repo: 

# Contribution:
# Qunoot - Implement strategy 1, 2, Pone for startegy 1, 2, Pall for strategy 1

strategy_one <- function(n, prisoner, card){

	# Calculate the probability (0 or 1) a prisoner finds their number 
	# in 2n boxes given n tries, starting with box number k

	# Args:

	# Input: 
	# n - number of tries
	# prisoner - prisoner's number
	# card - vector of unique random generated noumbers of length 2n  

	# Output:
	# success - 1 if the prisoner finds the number, 0 otherwise
	
	box <- c(prisoner)
	success = 0
	for (i in 1:n) {
		#cat("box - ", box, "card - ", card[box], "\n")
		if (card[box] == prisoner) {
			#cat("Success\n")
			success = 1
			break
		} 
		box <- card[box]
	}
	success
}

strategy_two <- function(n, prisoner, card){

        # Calculate the probability (0 or 1) a prisoner finds their number
        # in 2n boxes given n tries, starting with a random box number.
	# If a card contains a number of a box previously opened, it is failure 

        # Args:

        # Input:
        # n - number of tries
        # prisoner - prisoner's number
        # card - vector of unique random generated noumbers of length 2n

        # Output:
        # success - 1 if the prisoner finds the number, 0 otherwise

        box <- sample(1:(2*n),1)
        success = 0
        for (i in 1:n) {
		current_number <- card[box[i]]
                #cat("box - ", current_number, "card - ", card[current_number], "\n")
                if (card[current_number] == prisoner) {
                        #cat("Success\n")
                        success = 1
                        break
                }
		else if (current_number %in% box) {
			#cat("Loop failure\n")
			break
		}
                box[i+1] <- current_number
        }
        success
}


Pone <- function(n, k, strategy, nreps) {

	# Estimates the probability of a single prisoner 
	# succeeding in finding their number

	# Args:

	# Input:
	# n - number of tries
	# k - prisoner's number
	# strategy - 1, 2, or 3 to be implememnted
	# nreps - number of replicate simulations

	# Output:
	# probability of a prisoner succeeding
	
	cat("The probability of a prisoner succeeding, using strategy ", strategy, " is ")
	if (strategy == 1) {
		found <- sapply(1:nreps, function(x) {
			#cat("Rep - ", x, "\n")
			card <- sample(1:(2*n),2*n)
			#cat("cards: ", card, "\n")
			strategy_one(n, k, card)
		})
		#str(found)
		cat(sum(unlist(found))/nreps, "\n")
	}

        if (strategy == 2) {
		#cat("Prisoner ", k, "\n")
                found <- sapply(1:nreps, function(x) {
                        #cat("Rep - ", x, "\n")
                        card <- sample(1:(2*n),2*n)
                        #cat("cards: ", card, "\n")
                        strategy_two(n, k, card)
                })
                #str(found)
                cat(sum(unlist(found))/nreps, "\n")
        }
}

Pall <- function(n, strategy, nreps) {

        # Estimates the probability of all prisoners
        # succeeding in finding their number to be set free

        # Args:

        # Input:
        # n - number of tries
        # strategy - 1, 2, or 3 to be implememnted
        # nreps - number of replicate simulations

        # Output:
        # probability of all prisoners succeeding

	cat("The probability of all prisoners succeeding, using strategy ", strategy, " is ")
        if (strategy == 1) {
                found <- sapply(1:nreps, function(x) {
			#cat("Rep - ", x, "\n")
                        card <- sample(1:(2*n),2*n)
                        #cat("cards: ", card, "\n")
			all_success <- sapply(1:(2*n), function(y) {
				#cat("Prisinor - ",y, ", number of boxes - " ,n, "\n")
				strategy_one(n, y, card)
			})
			if (length(which(unlist(all_success)==0))>0) {
				0
			}
			else {
				1
			}
                })
                #str(found)
                cat(sum(unlist(found))/nreps, "\n")
        }
}

n <- c(5)
nreps <- c(10)
#k <- sample(1:(2*n), 1)
#Pall(n, 1, nreps)
#Pone(n, k, 1, nreps)
#Pone(n, k, 2, nreps)

