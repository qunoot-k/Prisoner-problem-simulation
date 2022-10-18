# Contributors:
# Qunoot Khaleeq - s2314595, Morgan Gallagher - s2463128, Banan Alharthi - s2451066

# repo: 

# Contribution:
# Qunoot - Implement strategy 1, 2, Pone for strategy 1, 2, Pall for strategy 1
# Morgan - Implement strategy 3, Pone for strategy 3, Pall for strategy 2 and 3, 
      #example code for each strategy n=5 and n=50, wrote remarks on results 
# Banan - 

strategy_one <- function(n, k, card){

	# Calculate the probability (0 or 1) a prisoner finds their number 
	# in 2n boxes given n tries, starting with box number k, the prisoner's number

	# Args:

	# Input: 
	# n - number of tries/max boxes prisoner can open
	# k - prisoner's number
	# card - vector of unique random generated numbers of length 2n  

	# Output:
	# success - 1 if the prisoner finds the number, 0 otherwise
	
  box <- c(k)
  success = 0
  for (i in 1:n) {
    if (card[box] == k) {success = 1; break} 
		box <- card[box]}
  return(success)
  }

strategy_two <- function(n, k, card){

        # Calculate the probability (0 or 1) a prisoner finds their number
        # in 2n boxes given n tries, starting with a random box number.
	      # If a card contains a number of a box previously opened, it is failure 

        # Args:

        # Input:
        # n - number of tries/max boxes prisoner can open
        # k - prisoner's number
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
  return(success)
  }

strategy_three <- function(n, k, card){

        # Calculate the probability (0 or 1) a prisoner finds their number
        # Open n random boxes out of 2n
        # If none of the n random boxes contains the prisoner number, it is a failure
        
        #Args:
  
        #Input:
        # n - number of tries, or number of random boxes opened
        # k - prisoner's number
        # card - vector of unique random generated numbers of length 2n
  
        # Output:
        # success - 1 if the prisoner finds the number, 0 otherwise

  rand_boxes <- sample(1:(2*n),n)
  success = 0
  for (i in rand_boxes) {
    if (card[i] == k) {success = 1; break}}
  return(success)
  }


Pone <- function(n, k, strategy, nreps) {

	# Estimates the probability of a single prisoner 
	# succeeding in finding their number

	# Args:

	# Input:
	# n - number of tries/max boxes prisoner can open
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
        # n - number of tries/max boxes prisoner can open
        # strategy - 1, 2, or 3 to be implemented
        # nreps - number of times to replicate Pone

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


nreps <- c(10000)
k <- sample(1:(2*n), 1)
n <- c(5)


n <- c(50)

#Estimating the individual success probabilities for n = 5, k = 7 for each strategy
#Running simulation 10,000 times to get probability 
Pone(5, 7, 1, 10000)
Pone(5, 7, 2, 10000)
Pone(5, 7, 3, 10000)

#Estimating the joint success probabilities for n = 5 for each strategy
#Running simulation 10,000 times to get probability 
Pall(5, 1, 10000)
Pall(5, 2, 10000)
Pall(5, 3, 10000)

#Estimating the individual success probabilities for n = 50, k = 42 for each strategy
#Running simulation 10,000 times to get probability 
Pone(50, 42, 1, 10000)
Pone(50, 42, 2, 10000)
Pone(50, 42, 3, 10000)

#Estimating the joint success probabilities for n = 50 for each strategy
#Running simulation 10,000 times to get probability 
Pall(50, 1, 10000)
Pall(50, 2, 10000)
Pall(50, 3, 10000)


#Results: 
#What's surprising is that there is a little over 30% chance that all prisoners
#will go free if they implement strategy 1. All strategies have a positive probability 
#under Pone but strategies 2 and 3 seem to be ineffective under Pall heading towards
#probability 0 of success. Strategy 1 continues to have a surprisingly consistent/high 
#probability compared to the others under Pall. 

#The probability of strategy 1 using Pall is interesting. Because we would expect the
#event of one prisoner going in and using strategy 1 to be independent of the next
#prisoner. But this is not the case because Pall using strategy 1 has a probability of 
#about 0.3, and not 0. If the events were independent we would expect Pall to be 
#(Pone*Pone*Pone...*Pone) n times, so (0.5)^n = 0, which is not true according to the
#simulations. So the event of one prisoner going in and implementing strategy 1 is not 
#independent of another going in and also implementing strategy 1. 


## For the sake of comparison 

dloop <- function(n,nreps) {
  
  # description 
  
  # Arguments:
  
  # Input:
  
  # Output:
  
  
  v <- array(0,2*n)             ## 2n-vector of probabilities
  
  for (i in 1:nreps) {
    u = sample(1:(2*n),2*n)     ## card numbers and indices are box numbers
    k = sample(1:(2*n),1)       ## prisoner number
    l = k                       ## variable to hold nested values
    
    for (ii in 1:(2*n)) {
      if (u[l] == k) {v[ii] <- v[ii] + 1; break}
      l = u[l]}
    }
  v <- v/nreps                  ## calculate the probabilities
  return(v)
  }    

## example n = 50
#n <- 50
#v <- dloop(n,nreps)
#cat("\n mean =",mean(v), "\n standard deviation =", sd(v))
#hist(v)
