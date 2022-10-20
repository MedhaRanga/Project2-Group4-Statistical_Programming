#1.   
# Function to find P(single prisoner succeeding in finding their number)  
Pone <- function(n, k, strategy, nreps) {
  Success_count = 0 
  if(strategy == 1){
    for(rep in 1:nreps) {
      card_number <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      currentCard = card_number[k];
      for(i in 1:n) {
        if(currentCard == k){
          Success_count = Success_count+1 
          break
        }
        else { 
          currentCard = card_number[currentCard] 
        }
      }
    return (Success_count/nreps) 
    }
  }
  else if(strategy == 2) 
  {
    for(rep in 1:nreps){
      card_number <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      opening_Box <- sample(1: (2*n), 1) 
      currentCard = card_number[opening_Box]
      for(i in 1:n) 
      {
        if(currentCard == opening_Box){
          Success_count = Success_count+1 
          break
        }
        else {
          currentCard = card_number[currentCard]
        }
      }
    return (Success_count/nreps) 
    }
  }
  else if(strategy == 3) {
    for(rep in 1:nreps) 
    {
      card_number <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      nBoxes <- c(sample(1 : (2*n), n, replace = FALSE))
      for(j in nBoxes) 
      {
        if(card_number[j] == n)
         { 
          Success_count = Success_count + 1
          break
        }
      }
    }  
    return (Success_count/nreps)
  }
  else {
    return (0.0)
  }
}


#2. 
# Function to find P(all prisoners finding their number and getting free) 
Pall <- function(n, strategy, nreps) {
  all_Free = 0 
  if(strategy == 1) {
    for(rep in 1:nreps) {
      card_number <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      Success_count = 0
      for(p in 1: (2*n)) {
        currentCard = card_number[p] 
        for(i in 1:n) {
          if(currentCard == p) {
            Success_count = Success_count + 1
            break
          }
          else {
            currentCard = card_number[currentCard]
          }
        }
      }
      if(Success_count == (2*n)) {
        all_Free = all_Free + 1
      }
    return (all_Free/nreps)
  }
  }
  else if(strategy == 2) {
    for(rep in 1:nreps) {
      card_number <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      Success_count = 0
      for(p in 1: (2*n)) {
        presentBox = sample(1 : (2*n), 1)
        currentCard = card_number[presentBox]
        for(i in 1:n) {
          if(currentCard == p) { 
            Success_count = Success_count + 1
            break
          }
          else {
            currentCard = card_number[currentCard]
          }
        }
      }
      if(Success_count == (2*n)) {
        all_Free = all_Free + 1
      }
    }
    return (all_Free/nreps)
  }
  else if(strategy == 3) {
    for(rep in 1:nreps) {
      card_number <- c(sample(1 : (2*n), 2*n, replace = FALSE))
      Success_count = 0
      for(p in 1:(2*n)) {
        nBoxes <- c(sample(1 : (2*n), n, replace = FALSE))
        for(j in nBoxes) {
          if(card_number[j] == p) { 
            Success_count = Success_count + 1
            break
          }
        }
      }
      if(Success_count == (2*n)) {
        all_Free = all_Free + 1
      }
    }
    return (all_Free/nreps)
  }
}

#3. 
#When n = 5
print(Pone(5, 3, 1, 10000))
print(Pone(5, 3, 2, 10000))
print(Pone(5, 3, 3, 10000))
print(Pall(5, 1, 10000))
print(Pall(5, 2, 10000))
print(Pall(5, 3, 10000))
#When n = 50
print(Pone(50, 20, 1, 10000))
print(Pone(50, 20, 2, 10000))
print(Pone(50, 20, 3, 10000))
print(Pall(50, 1, 10000))
print(Pall(50, 2, 10000))
print(Pall(50, 3, 10000))


#5
dloop <- function(n, nreps) {
  Success_Vector <- rep(0, (2*n)) 
  for(rep in 1:nreps) {  
    card_number <- c(sample(1 : (2*n), 2*n, replace = FALSE)) 
    cycleLengthFound <- rep(F, (2*n))
    for(j in 1:(2*n)) {
      currentLength = 0 
      currentCard = card_number[j] 
      max_boxes_open = n 
      
      while(currentCard != j && max_boxes_open > 0) {
        currentCard = card_number[currentCard]
        currentLength = currentLength + 1 
        max_boxes_open = max_boxes_open - 1 
      }
      if(currentLength != 0) {
        cycleLengthFound[currentLength] = TRUE 
      }
    }
    for(i in 1:(2*n)) {
      if(cycleLengthFound[i]) { 
        Success_Vector[i] = Success_Vector[i] + 1 
      }
    }
  }
  final_vector <- c(rep(0, (2*n)))
  for(i in 1:(2*n)) {
    final_vector[i] = Success_Vector[i]/nreps 
  
  }
  return (final_vector)
}
