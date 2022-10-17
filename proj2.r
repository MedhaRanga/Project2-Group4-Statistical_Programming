Pone <- function(n, k, strategy, nreps) {
  #Function to determine the probability that a single prisoner gest free using any one of the 3 given strategies 
  #Arguements
  # n - number of boxes that can opened by one prisoner
  # k - the prisoner number
  # strategy - strategy used by the prisoners to get free. Can be 1,2 or 3
  # nreps - number of simulations
  
  #--------------------CODE--------------------------------
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
    return (freePrisoners/nreps)
  }
  else {
    return (0.0)
  }
}

Pall <- function(n, strategy, nreps) {
  #Function to determine the probability that all prisoners get free using any one of the 3 given strategies 
  #Arguements
  # n - number of boxes that can opened by one prisoner
  # strategy - strategy used by the prisoners to get free. Can be 1,2 or 3
  # nreps - number of simulations
  
  #--------------------CODE--------------------------------
  # escapedPrisoners = rep(0, nreps)
  #freePrisoners = 0
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
            currentCard = cardsInBoxes[currentCard]
          }
        }
      }
      if(freePrisoners == (2*n)) {
        allPrisonersFree = allPrisonersFree + 1
      }
    }
    return (allPrisonersFree/nreps)
  }
  else if(strategy == 2) {
    for(rep in 1:nreps) {
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
            currentCard = cardsInBoxes[currentCard]
          }
        }
      }
      
      if(freePrisoners == (2*n)) {
        allPrisonersFree = allPrisonersFree + 1
      }
    }
    return (allPrisonersFree/nreps)
  }
  else if(strategy == 3) {
    for(rep in 1:nreps) {
      cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      #vector of length 2n with index representing the box number and the 
      #corresponding element representing the card number inside
      # nRandomBoxes <- c(sample(1 : (2*n), n, replace = FALSE))
      for(prisoner in 1:(2*n)) {
        nRandomBoxes <- c(sample(1 : (2*n), n, replace = FALSE))
        for(box in nRandomBoxes) {
          if(cardsInBoxes[box] == prisoner) { #if the card that we got = the prisoner number => prisoner gets free
            freePrisoners = freePrisoners + 1
            break
          }
        }
      }
      if(freePrisoners == (2*n)) {
        allPrisonersFree = allPrisonersFree + 1
      }
    }
    return (allPrisonersFree/nreps)
  }
}

print(Pone(50, 4, 1, 10000))
print(Pone(50, 4, 2, 10000))
print(Pone(50, 4, 3, 10000))
print(Pall(50, 1, 10000))
print(Pall(50, 2, 10000))
print(Pall(50, 3, 10000))
