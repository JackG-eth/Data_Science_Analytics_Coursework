# ************************************************
# RubixOnRails Coursework
# ************************************************
# PRATICAL BUSINESS ANALYTICS 2019
# COM3018

rm(list=ls())              

HOLDOUT           <- 70                   # % split to create TRAIN dataset

SCALE_DATASET     <- TRUE                 # Set to true to scale dataset before ML stage
OUTLIER_CONF      <- 0.95                 # Confidence p-value for outlier detection

TYPE_DISCREET     <- "DISCREET"           # field is discreet (numeric)
TYPE_ORDINAL      <- "ORDINAL"            # field is continuous numeric
TYPE_SYMBOLIC     <- "SYMBOLIC"           # field is a string
TYPE_NUMERIC      <- "NUMERIC"            # field is initially a numeric
TYPE_IGNORE       <- "IGNORE"             # field is ignored

DISCREET_BINS     <- 4                    # Number of empty bins to determine discreet
OUTPUT_VALUE      <- "cnt"                # The number of bikes

TIMESERIES_FREQUENCY<-8                   # Breaking the time down into sections of 3 hours

BASICNN_EPOCHS    <- 500                  # Number of epochs
DEEP_HIDDEN       <- c(40,40)             # Number of neurons in each layer
DEEP_STOPPING     <- 50                   # Number of times no improvement before stop
DEEP_TOLERANCE    <- 1e-3                 # Error threshold
DEEP_ACTIVATION   <- "Maxout"             # Linear activation function
DEEP_RHO          <- 0.95                 # adaptive learning rate time decay factor, reducing overfitting
DEEP_EPSILON      <- 1e-9                 # adaptive learning rate time smoothing factor, reducing overfitting
DEEP_REPRODUCABLE <- FALSE                # Set to TRUE to test training is same for each run

MAX_LITERALS      <- 55

# Field is not encoded
manualTypes <- data.frame()

# File locations
DATASET_FILENAME  <- "london-bike-sharing-dataset/London_Bike_Dataset.csv"          # Name of input dataset file for NN 
YEARLONG_TRAINING <- "london-bike-sharing-dataset/London_Bike_Dataset_Year.csv"     # Name of input dataset file for Regression & Time Series

# List of libaries used for models
MYLIBRARIES<-c("outliers",
               "corrplot",
               "MASS",
               "pROC",
               "formattable",
               "quantmod",
               "timeSeries",
               "rnn",
               "h2o",
               "stats",
               "PerformanceAnalytics",
               "scatterplot3d",
               "caret",
               "DMwR",
               "dplyr")

main<-function(){
  
  ### Regression Functions ###
  
  #read in bike_data
  initial_dataframe<-read.csv(YEARLONG_TRAINING)
  
  #retrieve the most valuable column
  
  bike_data<-initial_dataframe[,c(2,3,4,5,6,7,8,9,11)]
  #uncomment code if outliers are wanted
  #bike_data<-NPREPROCESSING_outlier(ordinals =  bike_data, confidence=OUTLIER_CONF,'cnt')
  
  #convert time to numeric values 1-24 to help standardization
  bike_data<-pre_process_24hrs(bike_data)
  #standardize data
  processed_bike_data <- bike_data %>% mutate_each_(funs(scale(.) %>% as.vector), 
                                                    vars=c("timestamp","wind_speed","t1", "t2", "cnt"))
  
  
  #if required to split data by holoday and weekend uncomment these lines
  #processed_bike_data<-processed_bike_data[(processed_bike_data$is_holiday == 1) || (processed_bike_data$is_weekend == 1),]
  processed_bike_data<-processed_bike_data[processed_bike_data$is_weekend == 1,]
  #processed_bike_data<-processed_bike_data[!processed_bike_data$is_holiday == 1,]

  #split data up
  training_records<-round(nrow(processed_bike_data)*(HOLDOUT/100))
  training_data<-processed_bike_data[1:training_records,]
  testing_data<-processed_bike_data[-(1:training_records),]
  
  #used to visualise relationship between two variables
  # results is a list with values for RMSE, MAE and R^2
  pairs(training_data[,c("t1","t2")])
  
  #plot a non linear regression
  results<-NscatterPlotNonLinearRegression(datasetTrain=training_data,
                                  datasetTest=testing_data,
                                  outputName = "cnt",
                                  predictorName ="t1",
                                  polyOrder=4)
  
  
  print(results)
  # multipleLinearRegression()
  ###
  #create a linear model between two inputs to predict count
  linearModelTransform2Inputs<-lm(cnt~t1+t2,data=training_data)
  
  # ceate a model and plot again testing data
  # results is a list with values for RMSE, MAE and R^2
  results<-NscatterPlotMultiLinearRegression(datasetTrain = training_data,
                                             datasetTest = testing_data,
                                             fieldNameOutput = "cnt",
                                             xname = "wind_speed",
                                             zname = "t1",
                                             linearModel=linearModelTransform2Inputs)
  ###timeseries###
  
  # re processing binning time values bin time values 
  dataframe<-pre_process_bins(initial_dataframe)
  
  # retrieve certain entries including all days- uncomment line if this required
  # example below is retrieving first month
  # dataframe<-dataframe[1:(744/3),]
  
  # exclude weekends and holidays, comment out if this is unwanted
  #dataframe<-dataframe[!dataframe$is_weekend == 1,]
  #dataframe<-dataframe[!dataframe$is_holiday == 1,]
  
  # split dataframe by only two columns timestamp and count
  processed_dataframe<-dataframe[,c(2,11)]
  
  # retrieve certin entries after filtering by weekends / holidays
  # comment out if required
  # processed_dataframe<-processed_dataframe[(((3697+1)/3)):(3701/3),]
  
  plot(as.timeSeries(processed_dataframe), at="chic", plot.type="s", cex.axis=0.75)
  
  #create a time series data plot
  title(main = "Full dataset - Bike Series")
  tsBikeCount = ts(processed_dataframe$cnt, frequency = nrow(processed_dataframe)/8)  #broken into time periods
  decomposeBikeData = decompose(tsBikeCount, "multiplicative")
  
  #plot the time series data
  plot(decomposeBikeData)
  
  ###neural networks

  # Read in the dataset
  dataset<-NreadDataset(DATASET_FILENAME)
  
  # Extract time column from dataset
  timestamps<-dataset[,colnames(dataset)=="timestamp"]
  # Encode time column between 0 & 1 and split into x & y 
  timestamps<-encodeTime(timestamps)
  
  # Remove Date and time from original datset and store in new one.
  timeless_data<-dataset[,!(colnames(dataset)=="datestamp" | colnames(dataset)=="timestamp")]
  
  # Determine the Field Types
  field_types<-NPREPROCESSING_initialFieldType(timeless_data)

  # Determine which fields are discrete
  discrete_fields<-NPREPROCESSING_discreetNumeric(dataset = timeless_data,
                                                  field_types = field_types,
                                                  cutoff = DISCREET_BINS)
  
  # Store all fields that are ordinals in seperate dataset
  ordinals<-timeless_data[,which(discrete_fields == TYPE_ORDINAL)]
  # Join time dataset to ordinals
  ordinals<-cbind(ordinals,timestamps)
  
  # First Z-scale values and then rescale them between 0 & 1
  z_scaled_matrix<-scale(ordinals,center=TRUE, scale=TRUE)
  z_scaled_df<-as.data.frame(z_scaled_matrix)
  ordinalReadyForML<-Nrescaleentireframe(z_scaled_df)
  
  # Edited function to not remove columns with low occurance
  catagoricalReadyForML<-NPREPROCESSING_categorical(dataset=timeless_data,field_types = discrete_fields)
  
  # Now all data is between 0 & 1 join them into one dataset for training
  combinedML<-cbind(catagoricalReadyForML,ordinalReadyForML)
  
  # Splitting the data
  # 70 % for training, 30% for testing
  # Remove the "cnt" column from the testing data and store it in expected dataset
  training_records<-round(nrow(combinedML)*(HOLDOUT/100))
  training_data <- combinedML[1:training_records,]
  testing_data <- combinedML[-(1:training_records),-which(names(combinedML)== OUTPUT_VALUE)]
  expected_data <- combinedML[-(1:training_records),which(names(combinedML)== OUTPUT_VALUE)]
  #expected_data_df <- as.data.frame(expected_data)
  
  # Shuffle training data for machine learning
  training_data<-training_data[order(runif(nrow(training_data))),]
  
  # Determine the Mean, Max and Min of the original dataset
  mean<-(sum(dataset[colnames(dataset)=="cnt"])/nrow(dataset[colnames(dataset)=="cnt"]))/nrow(dataset[colnames(dataset)=="cnt"])
  colMax <- function(data) sapply(data, max, na.rm = TRUE)
  colMin <- function(data) sapply(data, min, na.rm = TRUE)
  max<-colMax(dataset[colnames(dataset)=="cnt"])
  min<-colMin(dataset[colnames(dataset)=="cnt"])

  # Initialise NN
  N_DEEP_Initialise(DEEP_REPRODUCABLE)

  # Train the neurel network with training dataset
  deep<-N_DEEP_Train(train = training_data,
                     fieldNameOutput = OUTPUT_VALUE,
                     hidden = DEEP_HIDDEN,
                     stopping_rounds= DEEP_STOPPING,
                     stopping_tolerance = DEEP_TOLERANCE,
                     rho = DEEP_RHO,
                     epsilon = DEEP_EPSILON,
                     activation = DEEP_ACTIVATION,
                     reproducible = DEEP_REPRODUCABLE,
                     regression = TRUE)
  summary(deep)
  plot(deep)
  
  # Produce a set of expectations
  predictions<-N_EVALUATE_DeepNeural(test= testing_data,
                                     fieldNameOutput= OUTPUT_VALUE,
                                     deep = deep,
                                     regression = TRUE)
  
  predictionsAsDF<-as.data.frame(predictions)
  
  # Compare Expectations to Predictions
  plot(expected_data,predictions,main="RNN Training Dataset",ylab=c("Expected","Predicted"))
  

  #Extract date columns from the training dataset range
  dates<-dataset[,(colnames(dataset)=="datestamp" | colnames(dataset)=="timestamp")]
  training_dates<-dates[-(1:training_records),]
  timestamp<-paste(training_dates$datestamp,training_dates$timestamp)
  
  real_training_data <- dataset[1:training_records,]
  real_expected_data <- as.data.frame(dataset[-(1:training_records),which(names(dataset)== OUTPUT_VALUE)])
  scaledMax<-colMax(z_scaled_df[colnames(z_scaled_df)=="cnt"])
  scaledMin<-colMin(z_scaled_df[colnames(z_scaled_df)=="cnt"])

  real_exp_pred<-N_visualiseResults(expected_data,
                                    predictions,
                                    z_scaled_matrix,
                                    timestamp,
                                    scaledMin,
                                    scaledMax,
                                    title="Deep NN Windowed Test Dataset",
                                    yaxis_title=c("Expected","Predicted"),
                                    measuresFLag=TRUE)
  
  print("end of main")
}


cat("\014")

# Loads the libraries
library(pacman)
pacman::p_load(char=MYLIBRARIES,install=TRUE,character.only=TRUE)

#Load additional R script files provide for this lab
source("functions.R")

set.seed(123)


main()

print("End")