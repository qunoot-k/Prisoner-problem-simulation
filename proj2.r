# Contributors:
# Qunoot Khaleeq_s2314595, Morgan Gallagher_s2463128, Banan Alharthi_s2451066

# repo: https://github.com/qunoot-k/Practical2.git

# Contribution:
# Qunoot - Implement strategy 1, 2, Pone for strategy 1, 2, Pall for strategy 1
# Morgan - Implement strategy 3, Pone for strategy 3, Pall for strategy 2 and 3, 
      #example code for each strategy n=5 and n=50, wrote remarks on results 
# Banan - created dloop function and ran the example and plot


#Objective: 
#This code intends to simulate the prisoner problem to determine which strategy
#is the best for all prisoners to be released. 
#The prisoner problem is set up as follows, there are 2n prisoners that have a 
#unique number from 1 to 2n. The prison has a room that contains 2n boxes. Each 
#box contains a random card that has a unique number, 1 to 2n. The prisoners' 
#objective is to find the card with their number on it by opening a maximum of 
#n boxes. After one prisoner's turn the room is returned to its original state. 
#If all prisoners succeed in finding their number, all prisoners will be 
#released. We assume that the prisoners can implement 1 of 3 strategies. 

  #Strategy one is that the prisoner starts with the box that has 
  #their number on it. They open the box and read what number is
  # on the card. If the number on the card is not their number they
  #proceed to the box with the number on the card until they find 
  #their card number

  #Strategy two is the same procedure as strategy one, but the prisoner
  #starts at a random box instead of the box with their prisoner number

  #Strategy three is the prisoner opens n random boxes and checks if the
  #cards inside matches their prisoner number

#To simulate all prisoners being released we first create 3 functions that 
#return if one prisoner is successful in finding their number while following 
#each of the three strategies. We then create function Pone which tells us the 
#probability of one prisoner being successful. We then construct Pall to tell us 
#the probability that all prisoners find their number. To further support our 
#results from Pall to determine which strategy is best, we construct function 
#dloop to examine the probability of loops smaller than n existing in a random 
#shuffling of the cards in the boxes. We visualize this using a histogram. 


  
strategy_one <- function(n, k, card){

	# Calculate the probability (0 or 1) a prisoner finds their number 
	# in 2n boxes given n tries, starting with box number k, the prisoner's number

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

	# Estimates the probability of a single prisoner succeeding in finding their 
  #number. The function runs the chosen strategy function nreps times to 
  #obtain the probability of one prisoner succeeding given the chosen strategy,
  #the number of times the prisoner is successful over the total number of 
  #times the simulation is run, nreps. 

	# Input:
	# n - number of tries/max boxes prisoner can open
	# k - prisoner's number
	# strategy - 1, 2, or 3 to be implemented
	# nreps - number of replicate simulations
  
	# Output:
	# probability of a prisoner succeeding
	
  cat("The probability of a prisoner succeeding, using strategy "
      , strategy, " is ")
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

  # Estimates the probability of all prisoners succeeding in finding their 
  #number to be set free. The function runs whichever strategy is chosen 2n 
  #times to see if any prisoners have failed. If so, the entire group has 
  #failed, only if all are successful then the group is successful. We repeat 
  #this process nreps times to calculate the probability of success for all 
  #prisoners, the number of times the chosen strategy ends in success for the 
  #entire group divided by the total number of times the chosen strategy is run.

  # Input:
  # n - number of tries/max boxes prisoner can open
  # strategy - 1, 2, or 3 to be implemented
  # nreps - number of times to replicate Pone

  # Output:
  # probability of all prisoners succeeding

  cat("The probability of all prisoners succeeding, using strategy "
      , strategy, " is ")
  if (strategy == 1) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      all_success <- sapply(1:(2*n), function(y) {strategy_one(n, y, card)})
      if (length(which(unlist(all_success)==0))>0) {0} else {1}})
    cat(sum(unlist(found))/nreps, "\n")}

  else if (strategy == 2) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      all_success <- sapply(1:(2*n), function(y) {strategy_two(n, y, card)})
      if (length(which(unlist(all_success)==0))>0) {0} else {1}})
    cat(sum(unlist(found))/nreps, "\n")}
  
  else if (strategy == 3) {
    found <- sapply(1:nreps, function(x) {
      card <- sample(1:(2*n),2*n)
      all_success <- sapply(1:(2*n), function(y) {strategy_three(n, y, card)})
      if (length(which(unlist(all_success)==0))>0) {0} else {1}})
    cat(sum(unlist(found))/nreps, "\n")}
  }


nreps <- c(10000)
n <- c(5)
k <- sample(1:(2*n), 1)
cat("\n Running", 2*n,"prisoners simulation:\n")
#Estimating the individual success probabilities for n = 5 for each strategy
#Running simulation 10,000 times to get probability 
Pone(n, k, 1, nreps)
Pone(n, k, 2, nreps)
Pone(n, k, 3, nreps)

#Estimating the joint success probabilities for n = 5 for each strategy
#Running simulation 10,000 times to get probability 
Pall(n, 1, nreps)
Pall(n, 2, nreps)
Pall(n, 3, nreps)

n <- c(50)
k <- sample(1:(2*n), 1)
cat("\n Running", 2*n,"prisoners simulation:\n")
#Estimating the individual success probabilities for n = 50 for each strategy
#Running simulation 10,000 times to get probability 
Pone(n, k, 1, nreps)
Pone(n, k, 2, nreps)
Pone(n, k, 3, nreps)

#Estimating the joint success probabilities for n = 50 for each strategy
#Running simulation 10,000 times to get probability 
Pall(n, 1, nreps)
Pall(n, 2, nreps)
Pall(n, 3, nreps)


#Results: 
#What's surprising is that there is a little over 30% chance that all prisoners
#will go free if they implement strategy 1. All strategies have a positive  
#probability under Pone but strategies 2 and 3 seem to be ineffective under Pall 
#heading towards probability 0 of success. Strategy 1 continues to have a  
#surprisingly consistent/high probability compared to the others under Pall. 

#The probability of strategy 1 using Pall is interesting. Because we would 
#expect the event of one prisoner going in and using strategy 1 to be 
#independent of the next prisoner. But this is not the case because Pall using  
#strategy 1 has a probability of about 0.3, and not 0. If the events were  
#independent we would expect Pall to be (Pone*Pone*Pone...*Pone) n times, so 
#(0.5)^n = 0, which is not true according to the simulations. So the event of 
#one prisoner going in and implementing strategy 1 is not independent of another
#going in and also implementing strategy 1. 


# To look further into this matter, dloop function will be created to calculate 
# the probabilities of each length of the nested loop of cards and boxes 
# occurring until it's equal to k.

dloop <- function(n,nreps) {
  
  # Simulate nreps trials of a 2n vector that represents cards inside boxes  
  # and estimate the probabilities for each length of the nested loops 
  # being equal to the initial box k
  
  # Input:
  # n - half of the number of cards and boxes in each trial
  # nreps - number of trials simulated
  
  # Output:
  # v - 2n-vector of probabilities of each length of the nested loops occurring
  
  v <- array(0,2*n)                     # initiating 2n-vector
  
  for (rep in 1:nreps) {
    u <- sample(1:(2*n),2*n)            # card shuffle
    c <- array(0,2*n)                   # loop length counter vector
    for (i in 1:(2*n)) {
      k <- u[i]                         # initial box
      l <- k                            # variable to hold nested values
      for (j in 1:(2*n)) {
        if (u[l] == k) {c[j] <- c[j] + 1; break} 
        l <- u[l]}}
    c <- c/c(1:(2*n))                   # no. of loop lengths for a card shuffle
    v <- v + c}                         # sum of loop lengths nreps trials
  
  v <- v/(sum(v)); return(v)            # calculate the probabilities
  }    

n <- 50; nreps <- 10000
cat("\n Running the nested loops simulation for n =",n,": \n")
v <- dloop(n,nreps)
cat("The probability of no nested loops longer than 50 is",sum(v[1:n]),"\n")
plot(v,type="h", 
     main="Histogram of Loop Length", xlab="Loop Length", ylab="Probability")

#The simulation suggests there's around 85% chance of finding loops shorter than
#n, which supports our argument of using strategy 1 in the prisoner simulation.
