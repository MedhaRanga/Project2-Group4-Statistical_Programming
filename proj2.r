#Saksham Joshi(S2435635)
#Medha Ranga(S2417159)
#Richa Suresh Divkar(s2335095)

#----------------------------------Contributions--------------- ----------------
#Saksham Joshi - Q1,Q2,Q3,Q5,Q6(code and comments)
#Richa Suresh Divkar - q1,Q4,Q5,Q6(code)
#Medha Ranga - q1,Q2,Q3,Q4(code)

#-----------------------------------------------Question 1----------------------
Pone <- function(n, k, strategy, nreps=10000) {
  #Function to determine the probability that a single prisoner gets 
  #free using any one of the 3 given strategies 
  #Arguements
  # n - number of boxes that can opened by one prisoner
  # k - the prisoner number
  # strategy - strategy used by the prisoners to get free. Can be 1,2 or 3
  # nreps - number of simulations : default value 10000
  
  #Returns
  #Probability that a single prisoner got free using strategy 1,2 or 3
  
  #Working : During each simulation We create a vector of length 2n where the 
  #index of the vector represent the box number and the corresponding element 
  #represent the card number. We create a counter variable "freePrisoners" 
  #initialized at value 0 to keep a track of the number of simulations in which 
  #the prisoner got free.After all simulations have been executed we return the 
  #probability as freeprisoners/nreps
  
  #--------------------------------------CODE-----------------------------------
  freePrisoners = 0 #keeps track of the number of simulation in which prisoner 
  #number 'k' got free
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
          currentCard = cardsInBoxes[currentCard] #else we go and open that box 
          #whose number we got in our card in the previous box we opened
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
        if(currentCard == k) { #if the card that we got = the prisoner number => prisoner gets free
          freePrisoners = freePrisoners+1 #adding one because in this simulation our prisoner got free
          break
        }
        else {
          currentCard = cardsInBoxes[currentCard] #else we go and open that box 
          #whose number we got in our card in theprevious box we opened
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
        if(cardsInBoxes[box] == k) { #if the card that we got = the prisoner 
          #number => prisoner gets free
          freePrisoners = freePrisoners + 1
          break
        }
      }
    }
    return (freePrisoners/nreps) #return probability
  }
  else {
    statement<-"please input correct strategy number"
    return(statement)
    #because wrong strategy has been entered
  }
}

#---------------------------------------------Question 2----------------------------------------------------

Pall <- function(n, strategy, nreps) {
  #Function to determine the probability that all prisoners get free using any 
  #one of the 3 given strategies 
  
  #Arguements
  # n - number of boxes that can opened by one prisoner
  # strategy - strategy used by the prisoners to get free. Can be 1,2 or 3
  # nreps - number of simulations
  
  #Returns
  #Probability that all prisoners got free using strategy 1,2 or 3
  
  #Working : During each simulation We create a vector of length 2n where the 
  #index of the vector represent the box number and the corresponding element 
  #represent the card number. We create a counter variable to keep 
  #a track of whether a prisoner got free or not and another variable to keep a 
  #count of the simulations in which all prisoners got free. Whenever a prisoner
  #gets free we increment the "freeprisoners" variable by 1.At the end of each 
  #simulation we check if "freeprisoners"=2n then we increment our 
  #"allprisonersfree" variable by 1.And we calculate
  #the probability by the formula allprisonersfree/nreps
  
  #---------------------------------------------CODE---------------------------------------------------------
  allPrisonersFree = 0 #keeps track of the number of simulations in which all prisoners got free
  if(strategy == 1) {
    for(rep in 1:nreps) {
      cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))   #vector of 
      #length 2n with index representing the box number and the corresponding 
      #element representing the card number inside
      freePrisoners = 0
      for(prisoner in 1: (2*n)) {
        currentCard = cardsInBoxes[prisoner] 
        for(i in 1:n) {
          if(currentCard == prisoner) { #if the card that we got = the prisoner number
            #=> prisoner gets free
            freePrisoners = freePrisoners + 1
            break
          }
          else {
            currentCard = cardsInBoxes[currentCard] #otherwise we go see the card 
            #and go to that box number
          }
        }
      }
      if(freePrisoners == (2*n)) {
        allPrisonersFree = allPrisonersFree + 1 #increment the count of 
        #simulation in which all prisoners got free by 1
      }
    }
    return (allPrisonersFree/nreps) #return probability
  }
  else if(strategy == 2) {
    for(rep in 1:nreps) {
      freePrisoners = 0
      cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE)) #vector of 
      #length 2n with index representing the box number and the #corresponding
      #element representing the card number inside
      for(prisoner in 1: (2*n)) {
        currentBox = sample(1 : (2*n), 1)
        currentCard = cardsInBoxes[currentBox]
        for(i in 1:n) {
          if(currentCard == prisoner) { #if the card that we got = the prisoner number 
            #=> prisoner gets free
            freePrisoners = freePrisoners + 1
            break
          }
          else {
            currentCard = cardsInBoxes[currentCard] #otherwise we go see the 
            #card and go to that box number
          }
        }
      }
      if(freePrisoners == (2*n)) {
        allPrisonersFree = allPrisonersFree + 1 #increment the count of 
        #simulation in which all prisoners got free by 1
      }
    }
    return (allPrisonersFree/nreps) #return probability
  }
  else if(strategy == 3) {
    for(rep in 1:nreps) {
      freePrisoners = 0
      cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE))#vector of 
      #length 2n with index representing the box number and the #corresponding 
      #element representing the card number inside
      # nRandomBoxes <- c(sample(1 : (2*n), n, replace = FALSE))
      for(prisoner in 1:(2*n)) {
        nRandomBoxes <- c(sample(1 : (2*n), n, replace = FALSE)) #selecting n 
        #random boxes out of 2n boxes
        for(box in nRandomBoxes) {
          if(cardsInBoxes[box] == prisoner) { #if the card that we got = the prisoner number
            #=> prisoner gets free
            freePrisoners = freePrisoners + 1
            break
          }
        }
      }
      if(freePrisoners == (2*n)) {
        allPrisonersFree = allPrisonersFree + 1 #increment the count of
        #simulation in which all prisoners got free by 1
      }
    }
    return (allPrisonersFree/nreps) #return probability
  }
  else{
    statement<-"please input correct strategy number"
    return(statement) #because wrong strategy was entered
  }
}

#---------------------------------------------Question 3-------------------------------------------------
# running Pone for n=5 
cat(Pone(5, 4, 1, 10000))
cat(Pone(5, 4, 2, 10000))
cat(Pone(5, 4, 3, 10000))
#running Pone for n=50
cat(Pone(50, 4, 1, 10000))
cat(Pone(50, 4, 2, 10000))
cat(Pone(50, 4, 3, 10000))
#running Pall for n=5
cat(Pall(5, 1, 10000))
cat(Pall(5, 2, 10000))
cat(Pall(5, 3, 10000))
# running Pall for n=50
cat(Pall(50, 1, 10000))
cat(Pall(50, 2, 10000))
cat(Pall(50, 3, 10000))

#------------------------------------Question 4---------------------------------

# --> The probability of all prisoners escaping is higher in strategy 1 than rest.
# 
# -->This is because in strategy 1, prisoner opens box in a loop until he finds
#    a box with his card number (prisoner number).Each box will point to another
#    unique box.As prisoner starts with his numbered box, the loop will definitely 
#    contain his card number.For example, if there are "2n" boxes and prisoner can 
#    only open "n" boxes, in order to be successful the maximum loop length should 
#    be less than "n".If its unsuccessful there can be only one loop with length 
#    greater than "n", others must be less than "n" in total.
#
#    pr(success)= 1 - pr(loop length >= "n")
# 
# -->In strategy 2 loop is formed, but since prisoner starts with a random box, 
#    there is no guarantee that he finds his number in that loop.
#    
#    Finally, in strategy 3, the selection of boxes is completely random with 
#    no loop formation, hence both strategies have low probability.
#         
#

#--------------------------------------Question 5-------------------------------
dloop <- function(n, nreps=10000) {
  # Function to to estimate, by simulation, the probability of each loop length from 1 to 2n 
  # occurring at least once in a random shuffling of cards to boxes

  #Arguements
  #n - number of boxes that can be opened
  #nreps - number of simulations - default value = 10000
  
  
  #Working - we create a vector "times_Vector" in which index represent the loop 
  #length and the corresponding element is the number of times the loop of length
  #'i' where 'i' is the index occured across 'nreps' number of simulations. In each
  #simulation we create a vector "cycleLengthFound" of length 2n where each 
  #element is equal to F(false). No we iterate over our box loop and determine 
  #the length of loop. We use the same logic as we did in q1 strategy 1 but this
  #time we loop till 2n. If our currentCard = box then we break out of loop. Now
  #if our length is not equal to 0 we change the 'lth' index element of the 
  #cyclelengthloop to 'T'(True) where l is equal to value of currentloop and after 
  #the we increment the loop 'lth' elememt of the times_vector by 1.Note that 
  #we increment the times_Vector inside the simulation only because even if a loop 
  #specific length occurs more than once in a single simulation we will still 
  #count that as 1. The we return the probability as times_vector/nreps
  
  
  times_Vector <- rep(0, (2*n)) #empty vector to store probabilities. Initialized 
  #each element to 0 
  for(rep in 1:nreps) {  
    cardsInBoxes <- c(sample(1 : (2*n), 2*n, replace = FALSE)) 
    #vector of length 2n with index representing the box number and the 
    #corresponding element representing the card number inside
    cycleLengthFound <- rep(F, (2*n))
    #vector to check whether a loop of specific length has been found or not. 
    #Initialized each element to 'F'(False)
    for(box in 1:(2*n)) {
      currentLength = 0 #variable to keep a track of loop length
      currentCard = cardsInBoxes[box] 
      maxIterations = 2*n #maximum number of boxes we can open
      
      while(currentCard != box && maxIterations > 0) {
        #since current card number is not equal to box number, we 
        #increase length of loop by 1
        currentCard = cardsInBoxes[currentCard]
        currentLength = currentLength + 1 #increase length by 1
        maxIterations = maxIterations - 1 #decrease iterations by 1 because 1 
        #box has been opened
      }
      if(currentLength != 0) {
        cycleLengthFound[currentLength] = TRUE #if length found then 
        #we change the element at that index to True
      }
    }
    for(i in 1:(2*n)) {
      if(cycleLengthFound[i]) { 
        times_Vector[i] = times_Vector[i] + 1 #increase the count of that 
        #specific loop length
      }
    }
  }
  probabilityVector <- c(rep(0, (2*n)))
  for(i in 1:(2*n)) {
    probabilityVector[i] = times_Vector[i]/nreps #prob = success/total number of obs
    #in this prob = (count of specific loop length)/(total number of simulations)
  }
  return (probabilityVector)
}

#----------------------------------Question 6-----------------------------------
#dloop for n=50
x<-c(1:100)
p = dloop(50)
#plot to show the probabilitiee corresponding to each loop_length
plot(x,p,type="p",col="red",xlab="loop_length",ylab = "P(loop_length)")
#Now prob that there is no loop greater than n(in this case n=50)
cat("Probability that there is not loop greater than 50 is ",(1-sum(p[51:100])))
# This tells us that the probability that all prisoners get free is 0.33