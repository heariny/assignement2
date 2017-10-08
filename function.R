#----------------1 function-----------------------#
sum_column <- function(d, var) {
  result <- NULL
  x <- d[[var]] 
  if (!is.null(x)) { 
    if(is.numeric(x)){
      result<-sum(x)
    }
  }
  return (result)
}

#----------------2 function-----------------------#
my_sum<-function(vector){
  sum <-0                     #at first the summary is 0
  for(i in 1:length(vector)){ #this loop is to take every value out one by one
    sum<-sum+vector[i]        #add these values one by one
  }
 return(sum)
}

#----------------3 function-----------------------#
sum_divided_by<-function(vector,number){
  somme<-0
  #here the 'if' sentence to verify if the two arguments are numeric,
  #if one of them is not---> return the NULL
  #if they are both numeric--->add the vector get the summary and divide the summary by 'number'
  if (!is.numeric(vector)||!is.numeric(number)){
    return (NULL)
  }
  else{
    for(i in 1:length(vector)){ 
      somme<-somme+vector[i]
    }
    return (somme/number)
  }
}

#----------------3 function-----------------------#
my_mean<-function(vector){
  if (!is.numeric(vector)){#verify the argument type
    return (NULL)
  }
  else{
    len<-length(vector)#we want to get the sum divided by the number of elements
                       #so we need the number of elements,which is just the length of vector
    return(sum_divided_by(vector,len))
  }
}