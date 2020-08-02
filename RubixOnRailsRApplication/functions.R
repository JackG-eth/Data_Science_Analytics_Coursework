weather_vals<-c("clear", "few_clouds", "broken_clouds", "cloudy",
                "light_rain", "thunderstorm", "snowfall")
names(weather_vals)<-c(1,2,3,4,7,10,26)

column_names<-c("datestamp", "timestamp", "t1", "t2", "hum", "wind_speed", "weather_code", "is_holiday",
                     "is_weekend", "season", "cnt")
column_numbers<-c(1,2,3,4,5,6,7,8,9,10,11)
names(column_numbers)<-column_names

weather_code_column_number<-column_numbers["weather_code"]
time_series_bin_size<-3
time_period_column_number<-column_numbers["timestamp"]

manualTypes <- data.frame()

# ************************************************
# Nmae: Calculate the RMSE statistic
#
# INPUT: actual_y vector of real numbers indicating the known class
#        y_predicted vector of real numbers indicating the predicted class
#
# OUTPUT : double - RMSE
# ************************************************
Nrmse<-function(actual_y,y_predicted){
  
  return(sqrt(mean((actual_y-y_predicted)^2)))
}

# ************************************************
# Nmae: Calculate the MAE statistic
#
# INPUT: actual_y vector of real numbers indicating the known class
#        y_predicted vector of real numbers indicating the predicted class
#
# OUTPUT : double - MAE
# ************************************************
Nmae<-function(actual_y,y_predicted){
  
  return((mean(abs(actual_y-y_predicted))))
}


# ************************************************
# Calculate the r2 measure
#
# INPUT:      vector - double values for expected values
#             vector - double values of predicted values
# OUTPUT :    double - calculated N2
# ************************************************
Nr2<-function(actual,preds){
  
  rsq<-cor(preds, actual) ^ 2
  
  return(rsq)
}

# Function reads in dataset and removes punctuation
NreadDataset<-function(csvFilename){
  
  dataset<-read.csv(csvFilename,encoding="UTF-8",stringsAsFactors = FALSE)
  
  # The field names "confuse" some of the library algorithms
  # As they do not like spaces, punctuation, etc.
  names(dataset)<-NPREPROCESSING_removePunctuation(names(dataset))
  
  print(paste("CSV dataset",csvFilename,"has been read. Records=",nrow(dataset)))
  return(dataset)
}

# Function removed punction
NPREPROCESSING_removePunctuation<-function(fieldName){
  return(gsub("[[:punct:][:blank:]]+", "", fieldName))
}

# Unscale data using min and max
N_unscale0_to_1 <- function(x,minv,maxv){
  return(((maxv-minv)*x)+minv)
}

# ************************************************
# encodeTime()
#
# Encode time in a string to two input values
# encoded to the second
#
# INPUT:   string - timeAsString - time as HH:MM:SS
#
# OUTPUT : list - double - $x, $y
#          range is [-1,1]
# ************************************************
encodeTime<-function(timeAsString){ 
  
  MAXTIME_SECONDS<-(23*60*60)+(59*60)+59 
  seconds<-as.numeric(as.difftime(timeAsString, format = "%H:%M:%S", units = "secs"))
  featurea1<-cos((2*pi*seconds)/MAXTIME_SECONDS)
  featurea2<-sin((2*pi*seconds)/MAXTIME_SECONDS) 
  retList<-list(  "x"=featurea1,
                  "y"=featurea2
  )
  return(retList) 
} #endof myEncodeTime() 

# ************************************************
# NPREPROCESSING_discreetNumeric() :
#
# Test NUMERIC field if DISCREET or ORDINAL
#
# INPUT: data frame      - dataset     - input data
#        vector strings  - field_types - Types per field, either {NUMERIC, SYMBOLIC}
#        int             - cutoff      - Number of empty bins needed to determine discreet (1-10)
#
# OUTPUT : vector strings - Updated with types per field {DISCREET, ORDINAL}
# ************************************************
# Uses histogram
# Plots histogram for visulisation
# ************************************************
NPREPROCESSING_discreetNumeric<-function(dataset,field_types,cutoff){
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields that are all numeric
    if (field_types[field]==TYPE_NUMERIC) {
      
      #Scale the whole field (column) to between 0 and 1
      scaled_column<-Nrescale(dataset[,field])
      
      #Generate the "cutoff" points for each of 10 bins
      #so we will get 0-0.1, 0.1-0.2...0.9-1.0
      cutpoints<-seq(0,1,length=11)
      
      #This creates an empty vector that will hold the counts of ther numbers in the bin range
      bins<-vector()
      
      #Now we count how many numbers fall within the range
      #length(...) is used to count the numbers that fall within the conditional
      for (i in 2:11){
        bins<-append(bins,length(scaled_column[(scaled_column<=cutpoints[i])&(scaled_column>cutpoints[i-1])]))
      }
      
      # the 10 bins will have a % value of the count (i.e. density)
      bins<-(bins/length(scaled_column))*100.0
      
      graphTitle<-"AUTO:"
      
      #If the number of bins with less than 1% of the values is greater than the cutoff
      #then the field is deterimed to be a discreet value
      
      if (length(which(bins<1.0))>cutoff)
        field_types[field]<-TYPE_DISCREET
      else
        field_types[field]<-TYPE_ORDINAL
      
      #Bar chart helps visulisation. Type of field is the chart name
      barplot(bins, main=paste(graphTitle,field_types[field]),
              xlab=names(dataset[field]),
              names.arg = 1:10,bty="n")
      
    } #endif numeric types
  } #endof for
  return(field_types)
}

# ************************************************
# NPREPROCESSING_categorical() :
#
# Transform SYMBOLIC or DISCREET fields using 1-hot-encoding
#
# INPUT: data frame    - dataset      - symbolic fields
#        vector string - field_types  - types per field {ORDINAL, SYMBOLIC, DISCREET}
#
# OUTPUT : data frame    - transformed dataset
# ************************************************
# Small number of literals only otherwise too many dimensions
# Uses 1-hot-encoding if more than 2 unique literals in the field
# Otherwise converts the 2 literals into one field of {0,1}
# ************************************************
NPREPROCESSING_categorical<-function(dataset,field_types){
  
  #This is a dataframe of the transformed categorical fields
  catagorical<-data.frame(first=rep(NA,nrow(dataset)),stringsAsFactors=FALSE)
  
  #For every field in our dataset
  for(field in 1:(ncol(dataset))){
    
    #Only for fields marked SYMBOLIC or DISCREET
    if ((field_types[field]==TYPE_SYMBOLIC)||(field_types[field]==TYPE_DISCREET)) {
      
      #Create a list of unique values in the field (each is a literal)
      literals<-as.vector(unique(dataset[,field]))
      numberLiterals<-length(literals)
      
      #if there are just two literals in the field we can convert to 0 and 1
      if (numberLiterals==2){
        transformed<-ifelse (dataset[,field]==literals[1],0.0,1.0)
        catagorical<-cbind(catagorical,transformed)
        colnames(catagorical)[ncol(catagorical)]<-colnames(dataset)[field]
        
      } else
      {
        #We have now to one-hot encoding FOR SMALL NUMBER of literals
        if (numberLiterals<=MAX_LITERALS){
          for(num in 1:numberLiterals){
            nameOfLiteral<-literals[num]
            hotEncoding<-ifelse (dataset[,field]==nameOfLiteral,1.0,0.0)
            
            # 5/3/2018 - do not convert the field if their are too few literals
            # Use log of number of recrods as the measure
            literalsActive<-sum(hotEncoding==1)
            
            catagorical<-cbind(catagorical,hotEncoding)
            #060819 field name has the "_" seperator to make easier to read
            colnames(catagorical)[ncol(catagorical)]<-paste(colnames(dataset)[field],
                                                            "_",
                                                            NPREPROCESSING_removePunctuation(nameOfLiteral),
                                                            sep="")
            
          }
        } else {
          stop(paste("Error - too many literals in:",names(dataset)[field], numberLiterals))
        }
        
      }
    }
  }
  
  return(catagorical[,-1]) #Remove that first column that was full of NA due to R
}

# ************************************************
# N_DEEP_Initialise()
# Initialise the H2O server
#
# INPUT:
#         Bool       - reproducible       - TRUE if model must be reproducable each run
#
# OUTPUT : none
# ************************************************
N_DEEP_Initialise<-function(reproducible=TRUE){
  
  library(h2o)
  
  print("Initialise the H2O server")
  #Initialise the external h20 deep learning local server if needed
  #130517NRT - set nthreads to -1 to use maximum so fast, but set to 1 to get reproducable results
  #080819NRT - use reproducable parameter
  if (reproducible==TRUE)
    nthreads<-1
  else
    nthreads<- -1
  
  h2o.init(max_mem_size = "5g",nthreads = nthreads)
  
  h2o.removeAll() # 261019NRT clean slate - just in case the cluster was already running
  #h2o.no_progress()
}

# ************************************************
# N_DEEP_Train()
#
# h2O NEURAL NETWORK : DEEP LEARNING CLASSIFIER TRAIN
#
# INPUT:  Frame      - train              - scaled [0.0,1.0], fields & rows
#         String     - fieldNameOutput    - Name of the field to classify
#         Int Vector - hidden             - Number of hidden layer neurons for each layer
#         int        - stopping_rounds    - Number of times no improvement before stop
#         double     - stopping_tolerance - Error threshold
#         double     - rho                - Adaptive learning rate time decay factor
#         double     - epsilon            - Adaptive learning rate time smoothing factor
#         String     - activation         - Name of activation function
#         Bool       - reproducible       - TRUE if model must be reproducable each run
#         Bool       - regression         - TRUE to predict values rather than classification
#
# OUTPUT: object     - trained neural network
# ************************************************
N_DEEP_Train<- function(train,
                        fieldNameOutput,
                        hidden,
                        stopping_rounds,
                        stopping_tolerance,
                        rho,
                        epsilon,
                        activation,
                        reproducible=TRUE,
                        regression=FALSE){
  
  #positionOutput<-which(names(test)==fieldNameOutput)
  
  #Creates the h2o training dataset
  if (regression==FALSE) {
    train[fieldNameOutput] <- lapply(train[fieldNameOutput] , factor) #Output class has to be a R "factor"
  }
  
  train_h2o <- as.h2o(train, destination_frame = "traindata")
  
  # Create validation dataset for early stopping
  splits <- h2o.splitFrame(train_h2o, 0.9, seed=1234)
  nntrain  <- h2o.assign(splits[[1]], "nntrain.hex") # 90%
  nnvalid  <- h2o.assign(splits[[2]], "nnvalid.hex") # 10%
  
  #This lists all the input field names ignoring the fieldNameOutput
  predictors <- setdiff(names(train_h2o), fieldNameOutput)
  
  # Deep training neural network
  # Updated 13/5/17 - set reproducible = TRUE so that the same random numbers are used to initalise
  # 281019NRT - added validation dataset for early stopping
  
  deep<-h2o::h2o.deeplearning(x=predictors,
                              y=fieldNameOutput,
                              training_frame = nntrain,
                              validation_frame=nnvalid,
                              epochs=BASICNN_EPOCHS,
                              hidden=hidden,
                              adaptive_rate=TRUE,
                              stopping_rounds=stopping_rounds,
                              stopping_tolerance=stopping_tolerance,
                              stopping_metric = "AUTO",
                              rho = rho,
                              epsilon = epsilon,
                              fast_mode=FALSE,
                              activation=activation,
                              seed=1234,
                              l1 = 1e-2,
                              l2 = 1e-2,
                              reproducible = TRUE)
  
  return(deep)
}

# ************************************************
# N_EVALUATE_DeepNeural() :
#
# Evaluate Deep Neural Network classifier
# Generates probabilities from the classifier
#
# INPUT: Data Frame    -  test             - scaled [0.0,1.0], fields & rows
#        String        -  fieldNameOutput  - Name of the field that we are training on (i.e.Status)
#        Object         - deep             - trained NN including the learn weights, etc.
#         boolean      - plot              - TRUE = output charts/results
#         string       - myTitle           - title on results
#
# OUTPUT :
#         list - metrics from confusion matrix
# ************************************************
# Uses   library(h2o)
N_EVALUATE_DeepNeural<-function(test,
                                fieldNameOutput,
                                deep,regression=FALSE) {
  
  #Creates the h2o test dataset
  if (regression==FALSE)
    # Output class has to be a R "factor" for classification
    test[fieldNameOutput] <- lapply(test[fieldNameOutput] , factor)
  
  test_h2o <- as.h2o(test, destination_frame = "testdata")
  
  pred <- as.vector(h2o::h2o.predict(deep, test_h2o))
  
  return(pred)
}


# ************************************************
# NPREPROCESSING_initialFieldType() :
#
# Test each field for NUMERIC or SYNBOLIC
#
# INPUT: Data Frame - dataset - data
#
# OUTPUT : Vector - Vector of types {NUMERIC, SYMBOLIC}
# ************************************************
NPREPROCESSING_initialFieldType<-function(dataset){
  
  field_types<-vector()
  for(field in 1:(ncol(dataset))){
    
    entry<-which(manualTypes$name==names(dataset)[field])
    if (length(entry)>0){
      field_types[field]<-manualTypes$type[entry]
      next
    }
    
    if (is.numeric(dataset[,field])) {
      field_types[field]<-TYPE_NUMERIC
    }
    else {
      field_types[field]<-TYPE_SYMBOLIC
    }
  }
  return(field_types)
}



# ************************************************
# Nrescale() :
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Nrescale<-function(input){
  
  minv<-min(input)
  maxv<-max(input)
  result<-(input-minv)/(maxv-minv)
  return(result)
}

# ************************************************
# Nrescaleentireframe() :
#
# Rescle the entire dataframe to [0.0,1.0]
#
# INPUT:   data frame - dataset - numeric data frame
#
# OUTPUT : data frame - scaled numeric data frame
# ************************************************
Nrescaleentireframe<-function(dataset){
  
  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}
Nunscaleentireframe<-function(dataset,maxVal,minVal){
  
  scaled<-sapply(as.data.frame(dataset),N_unscale0_to_1,maxVal=maxVal,minVal=minVal)
  return(scaled)
}

# ************************************************
# outputResults() :
# ************************************************
outputResults<-function(dfResults){
  
  tidyTable<-data.frame((dfResults))
  
  tidyTable<-cbind(tidyTable,data.frame(error=tidyTable$expected-tidyTable$predicted))
  
  tidyTable$percent<-round((tidyTable$error/tidyTable$expected)*100,digits=2)
  
  tidy<-formattable::formattable(tidyTable,list(
    error = formatter("span",style = x ~ style(color = "black"),~sprintf("%.2f",error))))
  print(tidy)
}


N_visualiseResults<-function(expected,
                             predicted,
                             z_scaled_matrix,
                             dates,
                             scaledMin,
                             scaledMax,
                             title,
                             yaxis_title,
                             measuresFLag){
  
  #Scale this back into the real values and multiply back detrend offset
  scaleExp<-sapply(expected,N_unscale0_to_1,minv=scaledMin,maxv=scaledMax)
  b = attr(z_scaled_matrix, 'scaled:scale')[colnames(z_scaled_matrix)=="cnt"]
  a = attr(z_scaled_matrix, 'scaled:center')[colnames(z_scaled_matrix)=="cnt"]
  scaleExp<-scaleExp * b + a
  
  scaledPred<-sapply(predicted,N_unscale0_to_1,minv=scaledMin,maxv=scaledMax)
  b = attr(z_scaled_matrix, 'scaled:scale')[colnames(z_scaled_matrix)=="cnt"]
  a = attr(z_scaled_matrix, 'scaled:center')[colnames(z_scaled_matrix)=="cnt"]
  scaledPred<-scaledPred * b + a
  
  #Create a non-scaled and scaled dataframe with the expected values and then the predictions with dates
  non_scaled_exp_pred<-data.frame(expected=expected,predicted=predicted)
  exp_pred<-data.frame(expected=scaleExp,predicted=scaledPred)
  colnames(exp_pred)<-c("expected","predicted")
  row.names(exp_pred)<-dates
  
  #Uses the timeSeries library
  #Plot nice timeseries stock price chart
  #Show actual and predicted in seperate panels
  plot(as.timeSeries(exp_pred), at="chic", plot.type="m", cex.axis=0.75, type=c("l", "l"),ylab=yaxis_title)
  
  
  ## Works up to here/
  type<-ifelse(measuresFLag==TRUE,c("l", "p"),c("l", "l"))
  png("wide.png", width=50000, height = 1000)
  plot(as.timeSeries(exp_pred), at="pretty", plot.type="s", minor.ticks="month",
       cex.axis=0.75, type=type,cex.pch=0.5,ylab="Count Value")
  dev.off()
  legend("bottomleft", yaxis_title, col=1:ncol(exp_pred), lty=1, cex=.8)
  
  if (measuresFLag==TRUE){
    
    #Calculate measures
    RMSE<-Nrmse(non_scaled_exp_pred$expected,non_scaled_exp_pred$predicted)
    r2<-Nr2(non_scaled_exp_pred$expected,non_scaled_exp_pred$predicted)
    MAE<-Nmae(non_scaled_exp_pred$expected,non_scaled_exp_pred$predicted)
    print(RMSE)
    print(r2)
    print(MAE)
    
    title(main = paste(title,"- RMSE",round(RMSE,digits=2),
                       "MAE",round(MAE,digits=2),
                       "R2",round(r2,digits=4)))
  } else {
    title(main = title)
  }
  
  return(exp_pred)
  
}

# ************************************************
# Nrescale() :
#
# These are the real values, that we scale between 0-1
# i.e. x-min / (max-min)
#
# INPUT:   vector - input - values to scale
#
# OUTPUT : vector - scaled values to [0.0,1.0]
# ************************************************
Nrescale<-function(input){
  
  minv<-min(input)
  maxv<-max(input)
  result<-(input-minv)/(maxv-minv)
  return(result)
}

# ************************************************
# Nrescaleentireframe() :
#
# Rescle the entire dataframe to [0.0,1.0]
#
# INPUT:   data frame - dataset - numeric data frame
#
# OUTPUT : data frame - scaled numeric data frame
# ************************************************
Nrescaleentireframe<-function(dataset){
  
  scaled<-sapply(as.data.frame(dataset),Nrescale)
  return(scaled)
}

# ************************************************
# Nrescaleentireframe() :
#
# Revert scaling the entire dataframe to [0.0,1.0]
#
# INPUT:   maximum value prescale - double
# INPUT:   minimum value prescale - double
#
# OUTPUT : data frame - scaled numeric data frame
# ************************************************
Nunscaleentireframe<-function(dataset,maxVal,minVal){
  
  scaled<-sapply(as.data.frame(dataset),N_unscale0_to_1,maxVal=maxVal,minVal=minVal)
  return(scaled)
}

### Time Sequence Functions ###
# ************************************************
# pre_process_bins :
#
# pre_process dataset so that timestamp will be split into bins rather than 24 hours
#
# INPUT:   data frame - dataset 
#
# OUTPUT : data frame - dataset with an altered timestamp column with numeric bin value
#      as well as mean value / modal value for each row variable
# ************************************************
pre_process_bins<-function(dataframe){
  #counter keeps track of how many rows have been added
  counter<-0
  #bin number
  timeperiod<-1
  #create new dataframe based of old one
  df<-data.frame(dataframe[1,], stringsAsFactors = FALSE)
  #allows us to change weather code column
  df[,7] <- sapply(df[,weather_code_column_number], as.numeric)
  
  for(i in 1:nrow(dataframe)){
    #create new row based on current
    row<-dataframe[i,]
    
    if(counter==0){
      #create new row every time a bin has been created
      newRow<-row
      #list of weather code per bin
      weather_code_counter<-c(row[[weather_code_column_number]])
    }
    else{
      for(l in 1:length(row)){
        #add up numeric row variables
        if(l != 1 && l != 2 ){
          newRow[l]<-newRow[l] + row[l]
        }
      }
      #add the weather code to list
      weather_code_counter<-c(weather_code_counter, row[[weather_code_column_number]])
    }
    counter<-counter+1
    #if bin sie has been reached
    if(counter==time_series_bin_size){
      for(i in c(3,4,5,6,8,9,10)){
        #find average of results
        newRow[i]<-newRow[i]/time_series_bin_size
      }
      #set weather code to modal average 
      weather_t<-table(weather_code_counter)
      weather_v<-names(weather_t[weather_t==max(weather_t)])
      #if one weather code does not appear mutiple times get the middle value
      if(length(weather_v)>1){
        newRow[[weather_code_column_number]]<-weather_code_counter[2]
      }
      else{
        newRow[[weather_code_column_number]]<-weather_v[1]
      }
      counter<-0
      newRow[,time_period_column_number]<-timeperiod
      timeperiod<-timeperiod+1
      #add row to dataframw
      df[nrow(df)+1,]<-newRow
    }
    if(timeperiod>(24/time_series_bin_size)){
      timeperiod<-1
    }
  }
  #return the dataframe excluding first row which is a duplicate of initial dataset
  return(df[-1,])
}

###Regression Functions###

# ************************************************
# Nmae: Calculate the RMSE statistic
#
# INPUT: actual vector of real numbers indicating the known class
#        predicted vector of real numbers indicating the predicted class
#
# OUTPUT : double - RMSE
# ************************************************
Nrmse<-function(actual,predicted){
  
  return(sqrt(mean((actual-predicted)^2)))
}


# ************************************************
# Nmae: Calculate the MAE statistic
#
# INPUT: actual vector of real numbers indicating the known class
#        predicted vector of real numbers indicating the predicted class
#
# OUTPUT : double - MAE
# ************************************************
Nmae<-function(actual,predicted){
  
  return((mean(abs(actual-predicted))))
}

# ************************************************
# Calculate the r2 measure
#
# INPUT:      vector - double values for expected values
#             vector - double values of predicted values
# OUTPUT :    double - calculated N2
# ************************************************
r2<-function(linearModel){
  
  rsq<-summary(linearModel)$adj.r.squared
  
  return(rsq)
}

# ************************************************
# Non-linear regression model using one predictor field against outcome
# INPUT:      data frame - datasetTrain - create model
#             data frame - datasetTest - test model
#             data frame - datasetTest - test model
#             String - name of field to predict
#             String - name of single predictor field
#             int - polynominal order
# OUTPUT :    None
# ************************************************
NscatterPlotNonLinearRegression<-function(datasetTrain, datasetTest, outputName,predictorName, polyOrder){
  
  formular<-paste(outputName,"~","poly(",predictorName,",",polyOrder,")")
  linearModel<-lm(formular,data=datasetTrain)
  
  predictorInput<-data.frame(datasetTest[,predictorName])
  names(predictorInput)<-predictorName
  
  y_actual<-datasetTest[,outputName]
  y_predicted<-predict(linearModel, predictorInput)
  
  RMSE<-round(Nrmse(y_actual,y_predicted),digits=2)
  mae<-round(Nmae(y_actual,y_predicted),digits=2)
  r2<-round(r2(linearModel),digits=2)
  
  scatter<-data.frame(predictorInput,y_actual)
  scatter<-scatter[order(predictorInput),]
  
  par(mar=c(5.1,4.1,4.1,2.1))
  plot(scatter[,1],scatter[,2],pch=4,
       ylab=outputName,xlab=predictorName,
       main=paste("Non-Linear Regression:",outputName,
                  sub=paste("Polynominal order:",polyOrder,"MAE=",mae,"RMSE=",RMSE," R2=",r2)))
  
  # Plot the model prediction line on the chart
  topredict<-data.frame(seq(min(scatter[,1]),max(scatter[,1]),by=.1))
  names(topredict)<-predictorName
  y_predicted<-predict(linearModel, topredict)
  lines(topredict[,predictorName],y_predicted,col="red",lwd=4)
  return(
    list(MAE=mae,
         RMSE=RMSE,
         r2=r2)
    )
}

# ************************************************
# Non-linear regression model using one predictor field against outcome
# INPUT:       train - dataset to create the linear model
#              test -  dataset to evaluate the model
#              output- name of the value we'e predicting
#              predictor - name of the predictor field
# OUTPUT :    list of values RMSE, R^2 and MAE
# ************************************************
nscatterPlotData<-function(datasetTrain, datasetTest, outputName,predictorName){
  
  #print("Inside NscatterPlotError")
  
  # Creates a "formula" and the trains model on TRAIN dataset
  formular<-paste(outputName,"~",predictorName)
  linearModel<-lm(formula=formular,data=datasetTrain)
  
  # Extract predictor (input) values from TEST dataset into a data frame
  predictorInput<-subset(datasetTest,select=predictorName)
  
  # Get predictions from the model using the TEST dataset
  y_predicted<-predict(linearModel, predictorInput)
  
  # Extract the expected response (output) values from TEST dataset
  # into a data frame
  y_actual<-datasetTest[,outputName]
  
  # Calculate the metrics using functions in lab2functions.R
  RMSE<-round(Nrmse(actual=y_actual,predicted=y_predicted),digits=2)
  mae<-round(Nmae(actual=y_actual,predicted=y_predicted),digits=2)
  r2<-round(r2(linearModel),digits=2)
  
  # Calculate the error (residual) for each row in TEST dataset
  error<-y_actual-y_predicted
  
  # Create a data frame, so that we can sort these
  #   the input predictor (x)
  #   the expected value (actual_y)
  #   and the residuals in the model
  results<-data.frame(predictorInput,y_actual,error)
  
  # order from lowest to highest the valexpected values
  # for ease of visulisation
  results<-results[order(y_actual),]
  
  # Plots each point from the dataset as a "x"
  plot(results[,predictorName],
       results$y_actual,
       pch=4,
       ylab=outputName,
       xlab=predictorName,
       main="Linear Regression Errors",
       sub=paste("MAE=",mae,"RMSE=",RMSE," R2=",r2))
  
  #Plot the linear model as a straight line
  abline(linearModel,col = "blue", lwd=3)
  
  # Plot verticle lines from the actual points to the predicted value,
  # highlighting the error magnitude
  #suppressWarnings(arrows(results[,predictorName],
  #                        results$y_actual,
  #                        results[,predictorName],
  #                        results$y_actual-results$error,
  #                        length=0.05,angle=90,code=3,col="red"))
  
  return(
    list(MAE=mae,
         RMSE=RMSE,
         r2=r2))
  
} 

# ************************************************
# Non-linear regression model using one predictor field against outcome
# INPUT:      data frame - datasetTrain - create model
#             data frame - datasetTest - test model
#             data frame - datasetTest - test model
#             String - name of field to predict
#             String - name of single predictor field
#             int - polynominal order
# OUTPUT :    None
# ************************************************
nonLinearRegression<-function(datasetTrain, datasetTest, outputName,predictorName){
  
  #print("Inside NscatterPlotError")
  
  # Creates a "formula" and the trains model on TRAIN dataset
  formular<-paste(outputName,"~",predictorName)
  linearModel<-lm(cnt ~ polym(timestamp, degree = 3),data=datasetTrain,)
  r2<-round(Nr2(nonlinearModel),digits=2)
  print(paste("Non linear regression: r^2 with lstat+age+rm=",r2))
  
  # Extract predictor (input) values from TEST dataset into a data frame
  predictorInput<-subset(datasetTest,select=predictorName)
  
  # Get predictions from the model using the TEST dataset
  y_predicted<-predict(linearModel, predictorInput)
  
  # Extract the expected response (output) values from TEST dataset
  # into a data frame
  y_actual<-datasetTest[,outputName]
  
  # Calculate the metrics using functions in lab2functions.R
  RMSE<-round(Nrmse(actual=y_actual,predicted=y_predicted),digits=2)
  mae<-round(Nmae(actual=y_actual,predicted=y_predicted),digits=2)
  r2<-round(r2(linearModel),digits=2)
  
  # Calculate the error (residual) for each row in TEST dataset
  error<-y_actual-y_predicted
  
  # Create a data frame, so that we can sort these
  #   the input predictor (x)
  #   the expected value (actual_y)
  #   and the residuals in the model
  results<-data.frame(predictorInput,y_actual,error)
  
  # order from lowest to highest the valexpected values
  # for ease of visulisation
  results<-results[order(y_actual),]
  
  # Plots each point from the dataset as a "x"
  plot(results[,predictorName],
       results$y_actual,
       pch=4,
       ylab=outputName,
       xlab=predictorName,
       main="Linear Regression Errors",
       sub=paste("MAE=",mae,"RMSE=",RMSE," R2=",r2))
  
  #Plot the linear model as a straight line
  abline(linearModel,col = "blue", lwd=3)
  
  # Plot verticle lines from the actual points to the predicted value,
  # highlighting the error magnitude
  #suppressWarnings(arrows(results[,predictorName],
  #                        results$y_actual,
  #                        results[,predictorName],
  #                        results$y_actual-results$error,
  #                        length=0.05,angle=90,code=3,col="red"))
  
  return(
    list(MAE=mae,
         RMSE=RMSE,
         r2=r2))
  
} 

# ************************************************
# Creates a multiple linear regression model on the two named fields
# Visulises on 3d graph
# Evaluation is output to the console
#
# INPUT:        frame - dataset
#               String - name of the field to predict
#               String - x predictor field
#               String - y predictor field
#               Boolean - TRUE if log of the name in xname
# OUTPUT :      None
# ************************************************
# Uses library(scatterplot3d)
NscatterPlotMultiLinearRegression<-function(datasetTrain, datasetTest, fieldNameOutput,xname,zname, linearModel){
  formular<-paste(fieldNameOutput,"~",xname,"+",zname)

  x<-datasetTrain[,xname]
  y<-datasetTrain[,fieldNameOutput]
  z<-datasetTrain[,zname]
  
  # Get the predicted values from the model in TEST dataset
  dframe<-datasetTest[,c(fieldNameOutput,xname,zname)]
  dframe<-datasetTest
  y_actual<-dframe[,zname]
  y_predicted<-as.vector(predict(linearModel,dframe))
  
  # Calculate metrics
  RMSE<-round(Nrmse(y_actual,y_predicted),digits=2)
  mae<-round(Nmae(y_actual,y_predicted),digits=2)
  r2<-round(r2(linearModel),digits=2)
  
  print(RMSE)
  print(mae)
  print(r2)
  
  s3d<-scatterplot3d(x,y,z, main="2 Predictor Multi-linear Regression Model",
                     xlab=xname,ylab=fieldNameOutput,zlab=zname,
                     pch=16,highlight.3d = TRUE,
                     sub=paste("MAE=",mae,"RMSE=",RMSE," R2=",r2))
  s3d$plane3d(linearModel)
}

# ************************************************
# NPREPROCESSING_outlier() :
#
# Determine if a value of a record is an outlier for each field
#
# INPUT:   data frame - ordinals   - numeric fields only
#          double     - confidence - Confidence above which is determined an outlier [0,1]
#                                  - Set to negative Confidence if NOT remove outliers
#
# OUTPUT : data frame - ordinals with any outlier values replaced with the median of the field
# ************************************************
# ChiSquared method
# Uses   library(outliers)
NPREPROCESSING_outlier<-function(ordinals,confidence, field){
  
  for(field in 1:(ncol(ordinals))){
    
    sorted<-unique(sort(ordinals[,field],decreasing=TRUE))
    outliers<-which(outliers::scores(sorted,type="chisq",prob=abs(confidence)))
    #NplotOutliers(sortezd,outliers,colnames(ordinals)[field])
    
    #If found records with outlier values
    if ((length(outliers>0))){
      
      #070819NRT If confidence is positive then replace values with their means, otherwise do nothing
      if (confidence>0){
        outliersGone<-rm.outlier(ordinals[,field],fill=TRUE)
        sorted<-unique(sort(outliersGone,decreasing=TRUE))
        #NplotOutliers(sorted,vector(),colnames(ordinals)[field])
        ordinals[,field]<-outliersGone #Put in the values with the outliers replaced by means
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers),"Replaced with MEAN"))
      } else {
        print(paste("Outlier field=",names(ordinals)[field],"Records=",length(outliers)))
      }
    }
    
  }
  return(ordinals)
}

# ************************************************
# pre_process_24hours :
#
# pre_process dataset so that timestamp will be split into numeric rather than 24 hours
#
# INPUT:   data frame - dataset 
#
# OUTPUT : data frame - dataset with an altered timestamp column with numeric values 1-24
# ************************************************
pre_process_24hrs<-function(dataframe){
  counter<-0
  timeperiod<-1
  df<-data.frame(dataframe[1,], stringsAsFactors = FALSE)
  for(i in 1:nrow(dataframe)){
    row<-dataframe[i,]
    #row[7]<-as.numeric(weather_vals[[1]])
    row[1]<-timeperiod
    timeperiod<-timeperiod+1
    df[nrow(df)+1,]<-row
    #df<-rbind(df,newRow)
    
    if(timeperiod>24){
      timeperiod<-1
    }
  }
  return(df[-1,])
}
