######################################################################################


#function for stratergy1
stratergy1=function(cardsInBoxes,k,n,freePrisoners)
{
  currentCard = cardsInBoxes[k];
  for(i in 1:n) {
    if(currentCard == k) { 
      freePrisoners = freePrisoners+1
      break
    }
    else { 
      currentCard = cardsInBoxes[currentCard] 
    }
  }
  return(freePrisoners)
}


#function for stratergy2
stratergy2=function(cardsInBoxes,k,n,freePrisoners)
{ 
  startingBox = sample(1: (2*n), 1) 
  currentCard = cardsInBoxes[startingBox]
  for(i in 1:n) {
    if(k == currentCard) { 
      freePrisoners = freePrisoners+1 
      break
    }
    else {
      currentCard = cardsInBoxes[currentCard]
    }
  }
  return(freePrisoners)
}


#function for stratergy3
stratergy3=function(cardsInBoxes,k,n,freePrisoners)
{
  nRandomBoxes = c(sample(1 : (2*n), n, replace = FALSE))
  for(box in nRandomBoxes) 
    {
      if(cardsInBoxes[box] == k) 
        { 
          freePrisoners = freePrisoners + 1
          break
        }
    }
}

#####################################################################################################################################################
Pone <- function(n, k, strategy, nreps) 
{
  freePrisoners = 0
  
  if(strategy == 1) 
    {
      for(rep in 1:nreps) 
        {
          cardsInBoxes = c(sample(1 : (2*n), 2*n, replace = FALSE))
          freePrisoners=stratergy1(cardsInBoxes,k,n,freePrisoners)
        }
      return(freePrisoners/nreps)
    }
  
  else if(strategy == 2)
    {
      for(rep in 1:nreps) 
        {
          cardsInBoxes = c(sample(1 : (2*n), 2*n, replace = FALSE))
          freePrisoners=stratergy2(cardsInBoxes,k,n,freePrisoners)
        }
      return(freePrisoners/nreps)
    }
  
  else if(strategy == 3)
    {
      for(rep in 1:nreps) 
        {
          cardsInBoxes = c(sample(1 : (2*n), 2*n, replace = FALSE))
          freePrisoners=stratergy3(cardsInBoxes,k,n,freePrisoners)
        }
      return (freePrisoners/nreps)
    }
  
  else 
    {
    return (0.0)
    }
    
}

#####################################################################################################################################################

Pall <- function(n, strategy, nreps) {
  allPrisonersFree = 0 
  if(strategy == 1) 
    {
      for(rep in 1:nreps) 
        {
          cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))
          freePrisoners = 0
          for(prisoner in 1: (2*n)) 
            {
            freePrisoners=stratergy1(cardsInBoxes,prisoner,n,freePrisoners)
            }
          if(freePrisoners == (2*n)) 
            {
            allPrisonersFree = allPrisonersFree + 1
            }
         }
      return (allPrisonersFree/nreps)
    }
  else if(strategy == 2) 
    {
      for(rep in 1:nreps) 
        {
          cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))
          freePrisoners = 0
          for(prisoner in 1: (2*n)) 
            {
            freePrisoners=stratergy2(cardsInBoxes,prisoner,n,freePrisoners)
            }
          if(freePrisoners == (2*n)) {
            allPrisonersFree = allPrisonersFree + 1
            }
        }
      return (allPrisonersFree/nreps)
  }
  
  else if(strategy == 3) 
    {
      for(rep in 1:nreps) 
        {
          cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))
          freePrisoners = 0
          for(prisoner in 1:(2*n)) 
            {
            freePrisoners=stratergy3(cardsInBoxes,prisoner,n,freePrisoners)
            }
          if(freePrisoners == (2*n)) 
            {
            allPrisonersFree = allPrisonersFree + 1
            }
         }
      return (allPrisonersFree/nreps)
    }
}




####################################################################################################################################################

dloop=function(n,nreps=1000)
{ result_vector=array(0,dim=(2*n))
for(rep in 1:nreps) {
  cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))
  freePrisoners = 0
  k=0
  for(prisoner in 1: (2*n)) {
    currentCard = cardsInBoxes[prisoner] 
    for(i in 1:n) {
      k=k+1
      if(currentCard == prisoner) { #if the card that we got = the prisoner number => prisoner gets free
        freePrisoners = freePrisoners + 1
        break
      }
      else {
        currentCard = cardsInBoxes[currentCard]
      }
    }
    result_vector[k]=result_vector[k]+1
  }
  result=result_vector/nreps
}
cat(result)
}






