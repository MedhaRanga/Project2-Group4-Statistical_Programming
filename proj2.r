#-----------------------------------------------Question 1------------------------------------------------------
Pone <- function(n, k, strategy, nreps) {
  #Function to determine the probability that a single prisoner gets free using any one of the 3 given strategies 
  #Arguements
  # n - number of boxes that can opened by one prisoner
  # k - the prisoner number
  # strategy - strategy used by the prisoners to get free. Can be 1,2 or 3
  # nreps - number of simulations
  
  #Returns
  #Probability that a single prisoner got free
  
  #------------------------------------------------CODE---------------------------------------------------------
  freePrisoners = 0 #keeps track of the number of simulation in which prisoner number 'k' got free
  if(strategy == 1) {
    for(rep in 1:nreps) {
      cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      #vector of length 2n with index representing the box number and the 
      #corresponding element representing the card number inside
      currentCard = cardsInBoxes[k];
      for(i in 1:n) {
        if(currentCard == k) { #if the card that we got = the prisoner number => prisoner gets free
          freePrisoners = freePrisoners+1 #adding one because in this simulation our prisoner got free
          break
        }
        else { 
          currentCard = cardsInBoxes[currentCard] #else we go and open that box whose number we got in our card in the
          #previous box we opened
        }
      }
    }
    return (freePrisoners/nreps) # prob = favourable number of outcomes/ total number of outcomes
  }
  else if(strategy == 2) {
    for(rep in 1:nreps) {
      cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      #vector of length 2n with index representing the box number and the 
      #corresponding element representing the card number inside
      startingBox <- sample(1: (2*n), 1) #randomly selecting the box to be opened first
      currentCard = cardsInBoxes[startingBox]
      for(i in 1:n) {
        if(currentCard == startingBox) { #if the card that we got = the prisoner number => prisoner gets free
          freePrisoners = freePrisoners+1 #adding one because in this simulation our prisoner got free
          break
        }
        else {
          currentCard = cardsInBoxes[currentCard] #else we go and open that box whose number we got in our card in the
          #previous box we opened
        }
      }
    }
    return (freePrisoners/nreps) # prob = favourable number of outcomes/ total number of outcomes
  }
  else if(strategy == 3) {
    for(rep in 1:nreps) {
      cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      nRandomBoxes <- c(sample(1 : (2*n), n, replace = FALSE))
      for(box in nRandomBoxes) {
        if(cardsInBoxes[box] == n) { #if the card that we got = the prisoner number => prisoner gets free
          freePrisoners = freePrisoners + 1
          break
        }
      }
    }
    return (freePrisoners/nreps) #return probability
  }
  else {
    return (0.0) #because wronf strategy has been entered
  }
}

#---------------------------------------------Question 2----------------------------------------------------

Pall <- function(n, strategy, nreps) {
  #Function to determine the probability that all prisoners get free using any one of the 3 given strategies 
  #Arguements
  # n - number of boxes that can opened by one prisoner
  # strategy - strategy used by the prisoners to get free. Can be 1,2 or 3
  # nreps - number of simulations
  
  #Returns
  #Probability that all prisoners got free
  
  #---------------------------------------------CODE---------------------------------------------------------
  allPrisonersFree = 0 #keeps track of the number of simulations in which all prisoners got free
  if(strategy == 1) {
    for(rep in 1:nreps) {
      cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      #vector of length 2n with index representing the box number and the 
      #corresponding element representing the card number inside
      freePrisoners = 0
      for(prisoner in 1: (2*n)) {
        currentCard = cardsInBoxes[prisoner] 
        for(i in 1:n) {
          if(currentCard == prisoner) { #if the card that we got = the prisoner number => prisoner gets free
            freePrisoners = freePrisoners + 1
            break
          }
          else {
            currentCard = cardsInBoxes[currentCard] #otherwise we go see the card and go to that box number
          }
        }
      }
      if(freePrisoners == (2*n)) {
        allPrisonersFree = allPrisonersFree + 1 #increment the count of simulation in which all prisoners got free by 1
      }
    }
    return (allPrisonersFree/nreps) #return probability
  }
  else if(strategy == 2) {
    for(rep in 1:nreps) {
      freePrisoners = 0
      cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      #vector of length 2n with index representing the box number and the 
      #corresponding element representing the card number inside
      for(prisoner in 1: (2*n)) {
        currentBox = sample(1 : (2*n), 1)
        currentCard = cardsInBoxes[currentBox]
        for(i in 1:n) {
          if(currentCard == prisoner) { #if the card that we got = the prisoner number => prisoner gets free
            freePrisoners = freePrisoners + 1
            break
          }
          else {
            currentCard = cardsInBoxes[currentCard] #otherwise we go see the card and go to that box number
          }
        }
      }
      
      if(freePrisoners == (2*n)) {
        allPrisonersFree = allPrisonersFree + 1 #increment the count of simulation in which all prisoners got free by 1
      }
    }
    return (allPrisonersFree/nreps) #return probability
  }
  else if(strategy == 3) {
    for(rep in 1:nreps) {
      freePrisoners = 0
      cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      #vector of length 2n with index representing the box number and the 
      #corresponding element representing the card number inside
      # nRandomBoxes <- c(sample(1 : (2*n), n, replace = FALSE))
      for(prisoner in 1:(2*n)) {
        nRandomBoxes <- c(sample(1 : (2*n), n, replace = FALSE)) #selecting n raondom boxes out of 2n boxes
        for(box in nRandomBoxes) {
          if(cardsInBoxes[box] == prisoner) { #if the card that we got = the prisoner number => prisoner gets free
            freePrisoners = freePrisoners + 1
            break
          }
        }
      }
      if(freePrisoners == (2*n)) {
        allPrisonersFree = allPrisonersFree + 1 #increment the count of simulation in which all prisoners got free by 1
      }
    }
    return (allPrisonersFree/nreps) #return probability
  }
}

#---------------------------------------------Question 3-------------------------------------------------
# running Pone for n=5 
print(Pone(5, 4, 1, 10000))
print(Pone(5, 4, 2, 10000))
print(Pone(5, 4, 3, 10000))
#running Pone for n=50
print(Pone(50, 4, 1, 10000))
print(Pone(50, 4, 2, 10000))
print(Pone(50, 4, 3, 10000))
#running Pall for n=5
print(Pall(5, 1, 10000))
print(Pall(5, 2, 10000))
print(Pall(5, 3, 10000))
# running Pall for n=50
print(Pall(50, 1, 10000))
print(Pall(50, 2, 10000))
print(Pall(50, 3, 10000))

#--------------------------------------Question 4-------------------------------------------






#--------------------------------------Question 5-------------------------------------------
dloop <- function(n, nreps) {
  # Function to to estimate, by simulation, the probability of each loop length from 1 to 2n 
  # occurring at least once in a random shuffling of cards to boxes
  

  #Arguements
  #n - number of boxes that can be opened
  #nreps - number of simulations
  times_Vector <- rep(0, (2*n)) #empty vector to store probabilites. Initialised each element to 0 
  for(rep in 1:nreps) {  
    cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE)) 
    #vector of length 2n with index representing the box number and the 
    #corresponding element representing the card number inside
    cycleLengthFound <- rep(F, (2*n))
    #vector to check whether a loop of specific length has been found or not. Initialised each element to 'F'(False)
    for(box in 1:(2*n)) {
      currentLength = 0 #variable to keep a track of loop length
      currentCard = cardsInBoxes[box] 
      maxIterations = n #maximum number of boxes we can open
      
      while(currentCard != box && maxIterations > 0) {
        #since current card number is not equal to box number, we increase length of loop by 1
        currentCard = cardsInBoxes[currentCard]
        currentLength = currentLength + 1 #increase length by 1
        maxIterations = maxIterations - 1 #decrease iterations by 1 becasue 1 box has been opened
      }
      if(currentLength != 0) {
        cycleLengthFound[currentLength] = TRUE #if length found then we chnage the element at that index to True
      }
    }
    for(i in 1:(2*n)) {
      if(cycleLengthFound[i]) { 
        times_Vector[i] = times_Vector[i] + 1 #increase the count of that specific loop length
      }
    }
  }
  probabilityVector <- c(rep(0, (2*n)))
  for(i in 1:(2*n)) {
    probabilityVector[i] = times_Vector[i]/nreps #prob = success/total number of obs
    #in this proob = (count of specific loop length)/(total number of simulations)
  }
  return (probabilityVector)
}
x<-c(1:100)
p = dloop(50,100)
print(p)
plot(x,p)

