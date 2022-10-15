pone<-function(n,k,strategy,nreps=10000){
  i<-0 #variable to keep count of simulations
  prisoner_gets_free = 0
  #the above keeps track of the number of simulations
  #in which the prisoner gets free
  number_of_boxes<-2*n
  if(strategy==1)
  {
    while(i<nreps){
      #creating a vector of random values from 1 and 2n
      box_and_cards <-c(sample(1:number_of_boxes, number_of_boxes, replace=FALSE))
      #in the above vector the index of the vector represent
      #the box numbers and the element corresponding to each
      #index represent the card number in that box
      
      j<-0 #another counter variable
      
      #here k is the prisoner number
      check_box_index = k #since the first box to be opened should have the same number as that of the prisoner
      while(j<n){
        if(box_and_cards[check_box_index] == check_box_index)
        {
          #prisoner will get free
          prisoner_gets_free = prisoner_gets_free+1
          break
        }
        else
        {
          check_box_index = box_and_cards[check_box_index]
          #else we check that box whose card were found
          #in the previous box
        }
        j<-j+1
      }
      i<-i+1
    }
    probabilty_of_getting_free = prisoner_gets_free/nreps
    cat(probabilty_of_getting_free)
  }
  else if(strategy==2)
  {
    while(i<nreps){
      #creating a vector of random values from 1 and 2n
      # number_of_boxes<-2*n
      box_and_cards <-c(sample(1:number_of_boxes, number_of_boxes, replace=FALSE)) #since the first box to be opened can be selected randomly
      #in the above vector the index of the vector represent
      #the box numbers and the element corresponding to each
      #index represent the card number in that box
      
      j=0 #another counter variable
      
      #here k is the prisoner number
      check_box_index = sample(1:number_of_boxes,1,replace=FALSE) 
      while(j<n){
        if(box_and_cards[check_box_index] == check_box_index)
        {
          #prisoner will get free
          prisoner_gets_free = prisoner_gets_free+1
          break
        }
        else
        {
          check_box_index = box_and_cards[check_box_index]
          #else we check that box whose card were found
          #in the previous box
        }
        j<-j+1
      }
      i<-i+1
    }
    probabilty_of_getting_free = prisoner_gets_free/nreps
    cat(probabilty_of_getting_free)
  }
  else if(strategy==3)
  {
    probability_of_getting_free = n/number_of_boxes 
    cat(probability_of_getting_free)
  }
  else
  {
    cat("Wrong Strategy")
  }
}

pone(50,4,1)
