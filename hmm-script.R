library(DescTools)
library(ggplot2)
library(factoextra)
#install.packages("remotes")
Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS="true")
remotes::install_github("vqv/ggbiplot", force = TRUE)
library(ggbiplot)
library(caTools)
library(dplyr)
library(lubridate)
library(depmixS4)

getwd()
setwd ("C:/Users/navra/OneDrive/Documents/SFU Fall 2023/CMPT318/Group Project")
data <- read.table("TermProjectData.txt", header=TRUE, sep=",")

#Test for null values:
any(is.na(data$Date))
any(is.na(data$Time))
any(is.na(data$Global_active_power))
any(is.na(data$Global_reactive_power))
any(is.na(data$Voltage))
any(is.na(data$Global_intensity))
any(is.na(data$Sub_metering_1))
any(is.na(data$Sub_metering_2))
any(is.na(data$Sub_metering_3))

# These variables have null values, we thought about replacement methods such as replacing with last observation carried forward,
# however, by considering that it is a measurement and not sequential data, we decided to use something more consistent: mean
# this will keep the data points for our model, but not have them affect our distribution

# Replace NA value with means
data$Global_active_power[is.na(data$Global_active_power)] <- mean(data$Global_active_power, na.rm = TRUE)
data$Global_reactive_power[is.na(data$Global_reactive_power)] <- mean(data$Global_reactive_power, na.rm = TRUE)
data$Voltage[is.na(data$Voltage)] <- mean(data$Voltage, na.rm = TRUE)
data$Global_intensity[is.na(data$Global_intensity)] <- mean(data$Global_intensity, na.rm = TRUE)
data$Sub_metering_1[is.na(data$Sub_metering_1)] <- mean(data$Sub_metering_1, na.rm = TRUE)
data$Sub_metering_2[is.na(data$Sub_metering_2)] <- mean(data$Sub_metering_2, na.rm = TRUE)
data$Sub_metering_3[is.na(data$Sub_metering_3)] <- mean(data$Sub_metering_3, na.rm = TRUE)


# Test again
any(is.na(data))

#function to standardize the data
standardize <- function(data){
  return ((data - mean(data))/sd(data))
}

# Create day column for data
data$Day = c(weekdays(as.POSIXlt(data$Date, format = "%d/%m/%Y")))

# All columns expect for time and date being passed through and standardized
stand_data <- as.data.frame(apply(data[3:9],2,standardize)) 

# makes a table with the date and time with the scaled data
scaled_data = data.frame(data[1:2], data[10],stand_data)
remove(stand_data)
#splits the data into train and test
split = sample.split(scaled_data, SplitRatio = .8)
Training_set = subset(scaled_data, split == TRUE)
Test_set = subset(scaled_data, split == FALSE)

# PCA. 
pca <- prcomp(Training_set[4:10],center = TRUE)

#-----------------------------------------------------------------------------

# Makes a plot of the eigenvalues 
# (Shows what percentage each principal component is contributing)
fviz_eig(pca)

# shows what percentage each column delegates to PC1 
PC1 <- fviz_contrib(pca, choice="var", axes = 1)
PC1

PC2 <- fviz_contrib(pca, choice="var", axes = 2)
PC2

PC3 <- fviz_contrib(pca, choice="var", axes = 3)
PC3

PC4 <- fviz_contrib(pca, choice="var", axes = 4)
PC4

PC5 <- fviz_contrib(pca, choice="var", axes = 5)
PC5

PC6 <- fviz_contrib(pca, choice="var", axes = 6)
PC6

PC7 <- fviz_contrib(pca, choice="var", axes = 7)
PC7

summary(pca);
# We extract our proportion of variance values for each PC using the following formula

variance <- pca$sdev^2 / sum(pca$sdev^2)

# Now we extract the loading magnitude values from each PC for all variables
loading_pc1 <- pca$rotation[,1];
loading_pc2 <- pca$rotation[,2];
loading_pc3 <- pca$rotation[,3];
loading_pc4 <- pca$rotation[,4];
loading_pc5 <- pca$rotation[,5];
loading_pc6 <- pca$rotation[,6];
loading_pc7 <- pca$rotation[,7];

loading_pc1 <- pca$rotation[, 1]

# Calculate the absolute values of loadings for PC1
abs_loading_pc1 <- abs(loading_pc1)

# Identify the indices of the top-n variables based on absolute loadings
top_n_indices <- order(abs_loading_pc1, decreasing = TRUE)[1:3]

# Extract the names of the top-n variables
top_n_variables <- names(abs_loading_pc1)[top_n_indices]

# Display the selected top-n variables
top_n_variables

# The importance metric we will use is the absolute value of loading (shows relationship strength of each variable to its PC) multiplied with
# the proportion of variance (which shows us which PC captures more patterns and information in the data) for that particular PC, and then
# add the ones for each variable all together.

# loadings are all in the same order of variables so we add the absolute values of the magnitude loadings from each PC and multiply them with their 
# PC's proportion of variance to see average importance to the 4 principal components


# We value PC1 the most as it has the highest proportion of variance. The Global_active power and Global_intensity have significant
# loading values in PC1 which shows us it contributes considerably to the variance in PC1, and therefore to the dataset.

# Sub-metering 3 has a high loading on PC_1, as well as a significant loading on PC2, which solidifies it's importance to our data.
# The contribution that Sub_metering_3 has to our most valued PC as well as our second most valued (in terms of proportion of variance)
# shows us that it is a good choice to include in our model

# We finalize Global_active_power, Global_intensity and Sub_metering_3 as our 3 response variables

# ------------------------------------------------------------------------------------------------------------------------------------
# Q2:

# the day is chosen randomly so as to not introduce any bias into our model by making a selection
# randomly chosen day of the week:
round(runif(1, min = 1, max = 7))
# returned 3, therefore we will use Tuesday

#plot:
ggplot()+
  layer(data=Training_set[Training_set$Day=="Tuesday", ], 
        mapping= aes(x=Time, y=Global_active_power), 
        geom="point", 
        stat = "identity",
        position=position_identity())


# We can see high activity in the later afternoons/evenings from the plot. We will choose a 4 hour window in this time frame


# gets the data in the day of the time window, on our determined weekday
filtered_data <- filter(Training_set, Training_set$Day == "Tuesday")
start_time <- as.POSIXct("16:00:00", format = "%H:%M:%S")
end_time <- as.POSIXct("20:00:00", format = "%H:%M:%S")
Train_data <- filtered_data[(as.POSIXct(filtered_data$Time, format = "%H:%M:%S")) >= start_time & 
                              (as.POSIXct(filtered_data$Time, format = "%H:%M:%S")) <= end_time, ]


# determine ntimes
times <- aggregate(Date~Time, Train_data, FUN=length)$Date

# Keep only the data with our 3 response variables for the model
Train_data <- Train_data[c("Global_active_power", "Global_intensity", "Sub_metering_3")];


set.seed(3)
# Test model here: tests done for models with various nstates; found that it will not converge for nstates > 10
# Gaussian is chosen as we have continuous data in all 3 variables
modTest <- depmix(response = list(Global_active_power~1,Global_intensity~1,Sub_metering_3 ~ 1), family=list(gaussian(),gaussian(),gaussian()),
               data = Train_data, nstates = 10, ntimes = times);
fmTest <- fit(modTest)
logTest <- logLik(fmTest)
bicTest <- BIC(fmTest)


# We will test with nstates = 8,9,10, as prior testing with states 4-7 show lower logLike scores
set.seed(1)
BIC_values <- c()
logLik_values <- c()
mod8 <- depmix(response = list(Global_active_power~1,Global_intensity~1,Sub_metering_3 ~ 1), family=list(gaussian(),gaussian(),gaussian()),
                  data = Train_data, nstates = 8, ntimes = times);
fm8 <- fit(mod8)
BIC_values <- append(BIC_values, BIC(fm8))
logLik_values <- append(logLik_values, logLik(fm8))

set.seed(1)
mod9 <- depmix(response = list(Global_active_power~1,Global_intensity~1,Sub_metering_3 ~ 1), family=list(gaussian(),gaussian(),gaussian()),
               data = Train_data, nstates = 9, ntimes = times);
fm9 <- fit(mod9)
BIC_values <- append(BIC_values, BIC(fm9))
logLik_values <- append(logLik_values, logLik(fm9))


set.seed(3)
mod10 <- depmix(response = list(Global_active_power~1,Global_intensity~1,Sub_metering_3 ~ 1), family=list(gaussian(),gaussian(),gaussian()),
               data = Train_data, nstates = 10, ntimes = times);
fm10 <- fit(mod10)
BIC_values <- append(BIC_values, BIC(fm10))
logLik_values <- append(logLik_values, logLik(fm10))

nStates <- c(8:10)
#graph BIC and LogLik values to determine best model without over fitting (high logLik, low BIC pref)
matplot(nStates, cbind(BIC_values,logLik_values), type = "l", col= c("red","green"), lty=c(1,1))
BIC_values
logLik_values

# We see higher logLik values at nstates = 9 and nstates = 10, but will keep nstates = 8 in case of overfitting
# We see signs of overfitting as logLik decreases from nstates = 9 to nstates = 10

# Test data:


# Get ntimes
newTimes <- aggregate(Date~Time, Test_set, FUN=length)$Date


Test_set <- Test_set[c("Global_active_power", "Global_intensity", "Sub_metering_3")]


# make new models and extract parameters from old:
nmod8 <- depmix(response = list(Global_active_power~1,Global_intensity~1,Sub_metering_3 ~ 1), family=list(gaussian(),gaussian(),gaussian()),
                data = Test_set, nstates = 8, ntimes = newTimes);
new_pars8 <- getpars(fm8)

nmod9 <- depmix(response = list(Global_active_power~1,Global_intensity~1,Sub_metering_3 ~ 1), family=list(gaussian(),gaussian(),gaussian()),
                data = Test_set, nstates = 9, ntimes = newTimes);
new_pars9 <- getpars(fm9)

nmod10 <- depmix(response = list(Global_active_power~1,Global_intensity~1,Sub_metering_3 ~ 1), family=list(gaussian(),gaussian(),gaussian()),
                data = Test_set, nstates = 10, ntimes = newTimes);
new_pars10 <- getpars(fm10)

# transfer parameters from fitted model to new model:
testmod8 <- setpars(nmod8, new_pars8)
testmod9 <- setpars(nmod9, new_pars9)
testmod10 <- setpars(nmod10, new_pars10)



predict8 <- forwardbackward(testmod8, data = Test_set)
predict9 <- forwardbackward(testmod9, data = Test_set)
predict10 <- forwardbackward(testmod10, data = Test_set)


logLikTest <- c(predict8$logLike, predict9$logLike,predict10$logLike);

# Normalized comparison:
nlogLik8 <- logLik(fm8) / nrow(Train_data);
nlogLik9 <- logLik(fm9) / nrow(Train_data);
nlogLik10 <- logLik(fm10) / nrow(Train_data);
nlogLikTest8 <- logLikTest[1] / nrow(Test_set);
nlogLikTest9 <- logLikTest[2] / nrow(Test_set);
nlogLikTest10 <- logLikTest[3] / nrow(Test_set);

# print results
nlogLik8
nlogLik9
nlogLik10
nlogLikTest8
nlogLikTest9
nlogLikTest10

# From the results, we see that nstates = 9 returns the highest normalized log-likelihood values for the train and the test data
# therefore, we will use it as our final model
# -------------------------------------------------------------------------------------------------------------------------------------
# Q3:

# import anomaly datasets
anomaly1 <- read.table("DataWithAnomalies1.txt", header=TRUE, sep=",")
anomaly2 <- read.table("DataWithAnomalies2.txt", header=TRUE, sep=",")
anomaly3 <- read.table("DataWithAnomalies3.txt", header=TRUE, sep=",")

# Calculate log-likelihoods for anomaly data:

# Keep only data in time window:
anomaly1 <- anomaly1[(as.POSIXct(anomaly1$Time, format = "%H:%M:%S")) >= start_time & 
                              (as.POSIXct(anomaly1$Time, format = "%H:%M:%S")) <= end_time, ]
anomaly2 <- anomaly2[(as.POSIXct(anomaly2$Time, format = "%H:%M:%S")) >= start_time & 
                       (as.POSIXct(anomaly2$Time, format = "%H:%M:%S")) <= end_time, ]
anomaly3 <- anomaly3[(as.POSIXct(anomaly3$Time, format = "%H:%M:%S")) >= start_time & 
                       (as.POSIXct(anomaly3$Time, format = "%H:%M:%S")) <= end_time, ]
# Get ntimes
aTimes <- aggregate(Date~Time, anomaly1, FUN=length)$Date # they are all the same length


anomaly1 <- anomaly1[c("Global_active_power", "Global_intensity", "Sub_metering_3")]
anomaly2 <- anomaly2[c("Global_active_power", "Global_intensity", "Sub_metering_3")]
anomaly3 <- anomaly3[c("Global_active_power", "Global_intensity", "Sub_metering_3")]

# build models:
a1mod <- depmix(response = list(Global_active_power~1,Global_intensity~1,Sub_metering_3 ~ 1), family=list(gaussian(),gaussian(),gaussian()),
                data = anomaly1, nstates = 9, ntimes = aTimes);

a2mod <- depmix(response = list(Global_active_power~1,Global_intensity~1,Sub_metering_3 ~ 1), family=list(gaussian(),gaussian(),gaussian()),
                data = anomaly2, nstates = 9, ntimes = aTimes);

a3mod <- depmix(response = list(Global_active_power~1,Global_intensity~1,Sub_metering_3 ~ 1), family=list(gaussian(),gaussian(),gaussian()),
                 data = anomaly3, nstates = 9, ntimes = aTimes);

# transfer parameters from fitted model to new model:
a1modS <- setpars(a1mod, new_pars9)
a2modS <- setpars(a2mod, new_pars9)
a3modS <- setpars(a3mod, new_pars9)


predicta1 <- forwardbackward(a1modS, data = anomaly1)
predicta2 <- forwardbackward(a2modS, data = anomaly2)
predicta3 <- forwardbackward(a3modS, data = anomaly3)

a1LogLik <- predicta1$logLike
a2LogLik <- predicta2$logLike
a3LogLik <- predicta3$logLike

# print results
a1LogLik
a2LogLik
a3LogLik

# We can see from our results here that all 3 anomaly dataset are extremely abnormal, with how low the log-likehood scores are
# as well as anomaly3 returning NaN. The anomaly2 dataset is has slightly more normal behaviour, judging by the lower value.
# These datasets are highly likely to have anomalies in them.




