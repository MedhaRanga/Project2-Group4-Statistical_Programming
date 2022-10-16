pone<-function(n,k,strategy,nreps=10000){
  i<-0
  prisoner_gets_free = 0
  number_of_boxes<-2*n
  if(strategy==1)
  {
    while(i<nreps){
      set.seed(i)    ### consider i because for every prisoner will go throgh same simulation
      box_and_cards <-c(sample(1:number_of_boxes, number_of_boxes, replace=FALSE))
      j<-0
      check_box_index = k 
      while(j<n){
        if(box_and_cards[check_box_index] == check_box_index)
        {
          prisoner_gets_free = prisoner_gets_free+1
          break
        }
        else
        {
          check_box_index = box_and_cards[check_box_index]
        }
        j<-j+1
      }
      i<-i+1
    }
    probabilty_of_getting_free = prisoner_gets_free/nreps
    #cat(probabilty_of_getting_free)
    return(probabilty_of_getting_free)
  }
  else if(strategy==2)
  {
    while(i<nreps){
      set.seed(i)
      box_and_cards <-c(sample(1:number_of_boxes, number_of_boxes, replace=FALSE))
      j=0 
      check_box_index = sample(1:number_of_boxes,1,replace=FALSE) 
      while(j<n){
        if(box_and_cards[check_box_index] == check_box_index)
        {
          prisoner_gets_free = prisoner_gets_free+1
          break
        }
        else
        {
          check_box_index = box_and_cards[check_box_index]
        }
        j<-j+1
      }
      i<-i+1
    }
    probabilty_of_getting_free = prisoner_gets_free/nreps
    #cat(probabilty_of_getting_free)
    return(probabilty_of_getting_free)
  }
  else if(strategy==3)
  {
    probability_of_getting_free = n/number_of_boxes 
    #cat(probability_of_getting_free)
    return(probabilty_of_getting_free)
  }
  else
  {
    cat("Wrong Strategy")
    return(0)
  }
}


###############################################################################2nd function#########################################################



#pall=function(n,strategy,nreps){
#  i=1
 # total_prob=1
#  count=0
 # total_prob_in_each_Simulation=rep(0,nreps)
  #while(i<nreps+1)
   # {
    #  if(strategy==1){
     # for (prisoner_no in 1:2*n) {
      #  each_prisoner_prob=pone(50,prisoner_no,strategy,i)
       # \
      #}
      #if(total_prob==0)
      #{print("All prisoners are not released")
      #  total_prob_in_each_Simulation[i]=NA}
      #else
      #  {print("All prisoners are released with probability:")
          
       #   if(i==1){
        #     total_prob_in_each_Simulation[i]=total_prob
         # }
        #  else{total_prob_in_each_Simulation[i]=sum(total_prob_in_each_Simulation[-i]-total_prob)
         # }
            
        #i=i+1
         
        #}
        #cat(total_prob_in_each_Simulation)
  #  }
#  }
#} 
  
  
  
#################################################################################  
  
pall=function(n,strategy,nreps){ 
  i=1
  count=0
  while(i<nreps+1)
  {
    total_prob=1
    for(prisoner_no in 1:2*n)
    {
      each_prisoner_prob=pone(50,prisoner_no,strategy,i)
      total_prob=total_prob*each_prisoner_prob
      
    }
    if(total_prob!=0)
    {
      cat(each_prisoner_prob)
      count=count+1
     }
    else
     {
       #cat("all prisoner not released in simulation",i)
       
     }
    i=i+1
  }
  total_probability=count/nreps
  cat("total_probability",total_probability)
  }

  
  
  
  
  
  
  
  
  
  
  
  
  

























