#1.   
# Function to find P(single prisoner succeeding in finding their number)  

Pone <- function(n=3,k=6,strategy,nreps){
  success_count<-0
  failure_count<-0
  prob_success<- 1/(2*n)
  prob_failure<- 1 - prob_success
  number_of_attempts<-1
  mat<- matrix(replicate(nreps,sample(1:(2*n),(2*n))),2*n,nreps)
  if (strategy == 1){
    # Rows: numbers in box
    # 1 columns: 1 replicate
    
    for (i in 1:nreps){  
      p<- k
      while (number_of_attempts < n+1 & success_count != 1 ){
        print(number_of_attempts)
        new_no= mat[p,i] 
        print("p")
        print(p)
        print("new_no")
        print(new_no)
        if (p != new_no) {
          p = new_no
          failure_count<- failure_count + 1
          number_of_attempts<-number_of_attempts+1
        }
        else {
          success_count <- success_count + 1
        }
      }
      print("Simulation :")
      print(i)
      print("######################")
      print("success count is")
      print(success_count)
      print("######################")
      print("failure count is")
      print(failure_count)
      print("######################")
      cat("prob of prisoner",k,"for simulation",i,"is")
      total_prob<- (prob_success**success_count)*(prob_failure**failure_count)
      print(total_prob)
      #return(prob)     
    }
  }
  else if (strategy ==2 )
  {
    p<- sample(1:(2*n),1)
    for (i in 1:nreps){  
      while (number_of_attempts < n+1 & success_count != 1 ){
        print(number_of_attempts)
        new_no= mat[p,i] 
        if (p != new_no) {
          p = new_no
          failure_count<- failure_count + 1
          number_of_attempts<-number_of_attempts+1
        }
        else {
          success_count <- success_count + 1
        }
      }
      print("Simulation :")
      print(i)
      print("######################")
      print("success count is")
      print(success_count)
      print("######################")
      print("failure count is")
      print(failure_count)
      print("######################")
      print("total attempts")
      print(number_of_attempts)
      print("######################")
      cat("prob of prisoner",k,"for simulation",i,"is")
      total_prob<- (prob_success**success_count)*(prob_failure**failure_count)
      print(total_prob)
      #return(prob)     
    }
  }
}