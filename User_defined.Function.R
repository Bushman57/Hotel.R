# Data cleaning Function

# Steps:

# Phase 1

# 0.Compute the managu and Ngano variable by using the number of plates for each data
# NB: For the Ngano variable from the month of April we introduced a change where we would store the different snacks being prepared
# -Using if else statement for the two scenario
# 1.Takes in a data frame containing all the sales and the stock section
# We need to separate using columns from amount : Rice `Excluding the Remaining Food as the data is still not sufficient to be added the model`


# Trial  001
Clean_data<-function(data_name,Remain = FALSE){
  
  # Managu
  data_name %>% dplyr::select(contains('Man'),-Managu) %>% rowSums()->data_1
  
  data_name %>% dplyr::select(-Managu) %>% mutate(Managu=data_1)->data_2
  
  data_2 %>% dplyr::select(Amount:Managu,Soda,Dasani_.5ltr:Water_1ltr,Eggs) %>% mutate(Date =as.Date(Date,tryFormats = "%d/%m/%Y"))->data_3
  
  if(Remain==TRUE){
    
    return(data_3)
  }else{
    
    # Ngano
    data_3 %>% dplyr::select(contains('Ngano')) %>% rowSums()->data_4
    data_3 %>% dplyr::select(-contains('Ngano'),-Remaining_Food) %>% mutate(Ngano =data_4) ->data_5 
    return(data_5)
    
    
  }
  
}


# Trial  002
Train_function<-function(data_name,Remain = FALSE){
  # The remain parameter apply to data that was collected before April check the column names to understand more
  # Managu
  data_name %>% dplyr::select(contains('Man'),-Managu) %>% rowSums()->data_1
  
  data_name %>% dplyr::select(-Managu) %>% mutate(Managu=data_1)->data_2
  
  data_2 %>% dplyr::select(Amount:Managu,Soda,Dasani_.5ltr:Water_1ltr,Eggs) %>% mutate(Date =as.Date(Date,tryFormats = "%d/%m/%Y"))->data_3
  
  if(Remain==TRUE){
    
    return(data_3)
  }else{
    
    # Ngano
    data_3 %>% dplyr::select(contains('Ngano')) %>% rowSums()->data_4
    data_3 %>% dplyr::select(-contains('Ngano'),-Remaining_Food) %>% mutate(Ngano =data_4) ->data_5 
    return(data_5)
    
    
  }
  
}

# Phase 2

# Merge Function to combine all the  datasets


