# Contributors:
# Qunoot Khaleeq - s2314595, Morgan Gallagher - s2463128, Banan Alharthi - s2451066

# repo: 

# Contribution:
# Qunoot - Implement strategy 1, 2, Pone for strategy 1, 2, Pall for strategy 1
# Morgan - Implement strategy 3, Pone for strategy 3, Pall for strategy 2 and 3, 
      #wrote what is interesting about results

strategy_one <- function(n, k, card){

	# Calculate the probability (0 or 1) a prisoner finds their number 
	# in 2n boxes given n tries, starting with box number k

	# Args:

	# Input: 
	# n - number of tries
	# prisoner - prisoner's number
	# card - vector of unique random generated numbers of length 2n  

	# Output:
	# success - 1 if the prisoner finds the number, 0 otherwise
	
  box <- c(k)
  success = 0
  for (i in 1:n) {
    if (card[box] == k) {success = 1; break} 
		box <- card[box]}
  success
  }

strategy_two <- function(n, k, card){

        # Calculate the probability (0 or 1) a prisoner finds their number
        # in 2n boxes given n tries, starting with a random box number.
	# If a card contains a number of a box previously opened, it is failure 

        # Args:

        # Input:
        # n - number of tries
        # prisoner - prisoner's number
        # card - vector of unique random generated numbers of length 2n

        # Output:
        # success - 1 if the prisoner finds the number, 0 otherwise

  box <- sample(1:(2*n),1)
  success = 0
  for (i in 1:n) {
    current_number <- card[box[i]]
    if (current_number == k) {success = 1; break}
    else if (current_number %in% box) {break}
    box[i+1] <- current_number}
  success
  }

strategy_three <- function(n, k, card){

        # Calculate the probability (0 or 1) a prisoner finds their number
        # Open n random boxes out of 2n
        # If none of the n random boxes contains the prisoner number, it is a failure
        
        #Args:
  
        #Input:
        # n - number of tries, or random boxes opened
        # prisoner - prisoner's number
        # card - vector of unique random generated numbers of length 2n
  
        # Output:
        # success - 1 if the prisoner finds the number, 0 otherwise

  rand_boxes <- sample(1:(2*n),n)
  success = 0
  for (i in rand_boxes) {
    if (card[i] == k) {success = 1; break}}
  success
  }


Pone <- function(n, k, strategy, nreps) {

	# Estimates the probability of a single prisoner 
	# succeeding in finding their number

	# Args:

	# Input:
	# n - number of tries
	# k - prisoner's number
	# strategy - 1, 2, or 3 to be implemented
	# nreps - number of replicate simulations
	# Output:
	# probability of a prisoner succeeding
	
  cat("The probability of a prisoner succeeding, using strategy ", strategy, " is ")
  if (strategy == 1) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
			strategy_one(n, k, card)})
    cat(sum(unlist(found))/nreps, "\n")}
  
  else if (strategy == 2) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      strategy_two(n, k, card)})
    cat(sum(unlist(found))/nreps, "\n")}
  
  else if (strategy == 3) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      strategy_three(n, k, card)})
    cat(sum(unlist(found))/nreps, "\n")}
}

Pall <- function(n, strategy, nreps) {

        # Estimates the probability of all prisoners
        # succeeding in finding their number to be set free

        # Args:

        # Input:
        # n - number of tries
        # strategy - 1, 2, or 3 to be implemented
        # nreps - number of replicate simulations

        # Output:
        # probability of all prisoners succeeding

  cat("The probability of all prisoners succeeding, using strategy ", strategy, " is ")
  if (strategy == 1) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      all_success <- sapply(1:(2*n), function(y) {strategy_one(n, y, card)})
      if (length(which(unlist(all_success)==0))>0) 
        {0} else {1}})
    cat(sum(unlist(found))/nreps, "\n")}

  else if (strategy == 2) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      all_success <- sapply(1:(2*n), function(y) {strategy_two(n, y, card)})
      if (length(which(unlist(all_success)==0))>0) 
        {0} else {1}})
    cat(sum(unlist(found))/nreps, "\n")}
  
  else if (strategy == 3) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      all_success <- sapply(1:(2*n), function(y) {strategy_three(n, y, card)})
      if (length(which(unlist(all_success)==0))>0) 
        {0} else {1}})
    cat(sum(unlist(found))/nreps, "\n")}
  }


n <- c(5)
nreps <- c(10)
k <- sample(1:(2*n), 1)
#Pall(n, 1, nreps)
#Pone(n, k, 1, nreps)
#Pone(n, k, 2, nreps)


#Results: What's surprising is that there is a little over 30% chance that all prisoners
#will go free if they implement strategy 1. Without much thought we would expect this 
#probability to be much smaller. The probability of one prisoner getting out using 
#strategy 1 is around 50%. If we assumed independence for the rest of the prisoners 
#during their turn, we would expect Pall using strategy 1 to return almost 0, 0.5^(2n).
#But this isn't the case, instead the probability is about 0.3. We don't find it difficult
#to believe Pall using strategy 2 and 3 are 0. 
