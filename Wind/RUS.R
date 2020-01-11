detect <- function(dataset){
  
  ## remove first column (time)
  dataset = dataset[2:ncol(dataset)]
  
  # Define buffer ength
  buff_length = 1440
  buff_step = 1
  
  ## initialize model and data
  if (FALSE == exists('stored_data')){
    
    ## Packages
    list.of.packages <- c("pracma", "ebmc")
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages, repos='http://cran.us.r-project.org')
    
    ## Libraries
    library(pracma)
    library(ebmc)
    
    ## load model
    load('model.RData')
    
    ## create data frame
    stored_data = dataset[rep(1, buff_length),]
    
    ## assign to global variable
    assign('stored_data', stored_data, envir = globalenv())
    assign('model', model, envir = globalenv())
    assign('treshold', treshold, envir = globalenv())
  }
  
  ## add new row (remove oldest row)
  stored_data <<- rbind(stored_data[2:buff_length,], dataset)
  
  # ## Impute missing data
  if (sum(is.na(dataset)) > 0){ # If there is NA values

    if (sum(is.na(dataset)) > 1){
      stored_data[buff_length, is.na(dataset)] <<- apply(stored_data[(buff_length-30):(buff_length-1), is.na(dataset)], 2, mean) # mean imputing
    }

    else{
      stored_data[buff_length, is.na(dataset)] <<- mean(stored_data[(buff_length-30):(buff_length-1), is.na(dataset)])
    }
  }
  
  ## Last Imputing
  # if (sum(is.na(dataset)) > 0){
  #   stored_data[buff_length, is.na(dataset)] <<- stored_data[buff_length-1, is.na(dataset)]
  # }
  
  ## Detrend data
  detrended_stored_data = apply(stored_data[seq(buff_step, buff_length, buff_step),], 2, detrend) # detrend
  detrended_size = nrow(detrended_stored_data)
  
  ## Feature engineering
  original = stored_data[buff_length,]
  detrended = detrended_stored_data[detrended_size,]
  diff_1 = stored_data[buff_length,] - stored_data[buff_length-1,]
  diff_2 = stored_data[buff_length,] - stored_data[buff_length-2,]
  diff_4 = stored_data[buff_length,] - stored_data[buff_length-4,]
  diff_8 = stored_data[buff_length,] - stored_data[buff_length-8,]
  diff_16 = stored_data[buff_length,] - stored_data[buff_length-16,]
  diff_32 = stored_data[buff_length,] - stored_data[buff_length-32,]
  mean_30 = stored_data[buff_length,] - apply(stored_data[(buff_length-30):buff_length,], 2, mean)
  sd_30 = apply(stored_data[(buff_length-30):buff_length,], 2, sd)
  max_30 = stored_data[buff_length,] - apply(stored_data[(buff_length-30):buff_length,], 2, max)
  min_30 = stored_data[buff_length,] - apply(stored_data[(buff_length-30):buff_length,], 2, min)
  
  ## unite data
  X = data.frame(t(c(original, detrended, diff_1, diff_2, diff_4, diff_8, diff_16, diff_32, mean_30, sd_30, max_30, min_30)))
  for (i in 1:ncol(X))
  {
    X[,i] <- as.numeric(X[,i])
  }
  X[2,] = X[1,]
  
  ## predict data
  event = predict(model, X[,7:12]) >= treshold
  
  ## return prediction
  return(event[1])
}

destruct <- function(){
  
  ## remove global variables
  remove(model)
  remove(stored_data)
}

getOutline <- function(){
  competitor.name <- "Victor H. A. Ribeiro & Gilberto Reynoso Meza"
  competitor.institution <- "Pontifical Catholic University of Parana"
  
  return (list(NAME=competitor.name, INSTITUTION=competitor.institution));
}