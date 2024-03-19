
#Setting conditions
cat("CONDITIONS OF VALUES:

    \nFactory Production rate per factory should be >= 0.10 and <= 0.40 \nTotal Production Rate of all factories should = 1
    
    \nFactory Defect rate per factory should be >= 0.01 and <=0.05 \nTotal Defect Rate of all factories should = 0.12")

#Validating user inputs
while(1){
  factory1_p <- readline(prompt = "Factory 1 production rate:");
  factory2_p <- readline(prompt = "Factory 2 production rate:");
  factory3_p <- readline(prompt = "Factory 3 production rate:");
  
  f1_defect <- readline(prompt = "Factory 1 Defect rate:");
  f2_defect <- readline(prompt = "Factory 2 Defect rate:");
  f3_defect <- readline(prompt = "Factory 3 Defect rate:");
  
  if(is.na(as.numeric(factory1_p)) | is.na(as.numeric(factory1_p)) | is.na(as.numeric(factory1_p)) | 
     is.na(as.numeric(f1_defect)) | is.na(as.numeric(f2_defect)) | is.na(as.numeric(f3_defect))){
    cat("Inputed values should be numeric")
    
  }else{
    
    #Data type convertions
    factory1_p <- as.numeric(factory1_p)
    factory2_p <- as.numeric(factory2_p)
    factory3_p <- as.numeric(factory3_p)
    f1_defect <- as.numeric(f1_defect)
    f2_defect <- as.numeric(f2_defect)
    f3_defect <- as.numeric(f3_defect)
    
    sum_produce <- sum(factory1_p,factory2_p,factory3_p)
    sum_defect <- sum(f1_defect, f2_defect, f3_defect)
    
    if(sum_produce !=1 | round(sum_defect,2) != 0.120
       |(factory1_p < 0.1)|(factory1_p > 4)|(factory2_p<0.1)|(factory2_p>4)|(factory3_p<0.1)|(factory3_p>4)
       |(f1_defect<0.01)|(f1_defect>0.05)|(f2_defect<0.01)|(f2_defect>0.05)|(f3_defect<0.01)|(f2_defect>0.05))
    {
      cat("ERROR: Values do not allign with conditions!!")
    }else{
      cat("continue ")
      break;
    }
  }
}

overall_defect_rate <-(( factory1_p*f1_defect) +(factory2_p*f2_defect) + (factory3_p*f3_defect))
overall_defect_rate
cat("The chance of having a defective product from all of the factories is",overall_defect_rate *100, "% ")