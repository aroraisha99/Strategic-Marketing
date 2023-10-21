# Install packages
library('dplyr')
library('readxl')
library(corrplot)

# Import data set
spotify_data<-read_xlsx("C18 Spotify Quarterly.xlsx")

# Zero-values identification
summary(spotify_data)

# linear regression model of AD revenue and AD MAUs
regression11<-lm(spotify_data$`Ad Revenue`~ spotify_data$`Ad MAUs`,data=spotify_data)
summary(regression11)

# linear regression model of Premium revenue and Premium MAUs
regression12<-lm(spotify_data$`Premium Revenue`~spotify_data$`Premium MAUs`,data=spotify_data)
summary(regression12)

# Plot correlation matrix between variables
spotify_correlations <- spotify_data %>% 
  select(`Sales and Marketing Cost`, `Research and Development Cost`, `Genreal and Adminstraive Cost`)
cor(spotify_correlations)
corrplot(cor(spotify_correlations), method = "number", type = "lower", bg = "grey")

# linear regression model of Premium MAUs 
regression1<-lm(spotify_data$`Premium MAUs`~spotify_data$`Sales and Marketing Cost`+spotify_data$`Research and Development Cost`+spotify_data$`Genreal and Adminstraive Cost`,data=spotify_data)
summary(regression1)

# log-log regression model of premium MAUs
regression2<-lm(log(spotify_data$`Premium MAUs`)~log(spotify_data$`Sales and Marketing Cost`)+log(spotify_data$`Research and Development Cost`)+log(spotify_data$`Genreal and Adminstraive Cost`)+log(spotify_data$`Sales and Marketing Cost`)*log(spotify_data$`Research and Development Cost`)+log(spotify_data$`Sales and Marketing Cost`)*log(spotify_data$`Genreal and Adminstraive Cost`)+log(spotify_data$`Research and Development Cost`)*log(spotify_data$`Genreal and Adminstraive Cost`),data=spotify_data)
summary(regression2)

# linear regression model of AD MAUs
regression3<-lm(spotify_data$`Ad MAUs`~spotify_data$`Sales and Marketing Cost`+spotify_data$`Research and Development Cost`+spotify_data$`Genreal and Adminstraive Cost`,data=spotify_data)
summary(regression3)

# log-log regression model of AD MAUs
regression4<-lm(log(spotify_data$`Ad MAUs`)~log(spotify_data$`Sales and Marketing Cost`)+log(spotify_data$`Research and Development Cost`)+log(spotify_data$`Genreal and Adminstraive Cost`)+log(spotify_data$`Sales and Marketing Cost`)*log(spotify_data$`Research and Development Cost`)+log(spotify_data$`Sales and Marketing Cost`)*log(spotify_data$`Genreal and Adminstraive Cost`)+log(spotify_data$`Research and Development Cost`)*log(spotify_data$`Genreal and Adminstraive Cost`),data=spotify_data)
summary(regression4)

# Function 'center' to mean-center variables
center <- function(x) {scale(x, scale = F)}

# Mean-center predictor variables
spotify_data<-spotify_data %>%
mutate(SM_centered= center(log(spotify_data$`Sales and Marketing Cost`)),
       RD_centered=center(log(spotify_data$`Research and Development Cost`)),
       GA_cenetred=center(log(spotify_data$`Genreal and Adminstraive Cost`)))

# Run log-log regression model of premium MAUs using mean-centered predictor variables
regression5<-lm(log(spotify_data$`Premium MAUs`)~SM_centered+RD_centered+ GA_cenetred+SM_centered*RD_centered+SM_centered*GA_cenetred+RD_centered*GA_cenetred,data=spotify_data)
summary(regression5)

# Run log-log regression model of AD MAUs using mean-centered predictor variables
regression6<-lm(log(spotify_data$`Ad MAUs`)~SM_centered+RD_centered+ GA_cenetred+SM_centered*RD_centered+SM_centered*GA_cenetred+RD_centered*GA_cenetred,data=spotify_data)
summary(regression6)

# Calculate MAPE

# Function "adstock" to calculate Marketing stock. The below illustrates the approach with adstock assuming a lambda of 0.8
adstock <- function(x, rate = 0.8) {return(as.numeric(stats::filter(x = x, filter = rate, method = "recursive")))}
# Generate adstock predictor variables
spotify_data <- spotify_data %>%
  mutate(`Sales and Marketing Cost Adstock` = adstock(`Sales and Marketing Cost`),`Research and Development Cost Adstock`=adstock(`Research and Development Cost`),`Genreal and Adminstraive Cost Adstock`=adstock(`Genreal and Adminstraive Cost`))

# Run log-log regression model of premium MAUs using adstock variables
regression7<-lm(log(spotify_data$`Premium MAUs`)~log(`Sales and Marketing Cost Adstock`)+log(`Research and Development Cost Adstock`)+log(`Genreal and Adminstraive Cost Adstock`),data=spotify_data)
summary(regression7)
# Predictive accuracy
plot(spotify_data$Date,spotify_data$`Premium MAUs`, type = 'l')
# Total number of rows in the data frame
n<-nrow(spotify_data)
# Number of rows for the training set (80% of the dataset)
n_train<-round(0.80*n)
# Training data
spotify_data.train<-subset(head(spotify_data,20))
# Holdout data
spotify_data.holdout<-spotify_data[20:nrow(spotify_data),]
# Estimation on training data
regression8<-lm(log(spotify_data.train$`Premium MAUs`)~log(`Sales and Marketing Cost Adstock`)+log(`Research and Development Cost Adstock`)+log(`Genreal and Adminstraive Cost Adstock`),data=spotify_data.train)
# Predict Premium MAUs on holdout data 
spotify_data.holdout$predicted_Premuim_MAUs_log <- predict(object = regression8, newdata = spotify_data.holdout) 
# Convert predicted log Premium MAUs to actual Premium MAUs
spotify_data.holdout$predicted_Premium_MAUs <- exp(spotify_data.holdout$predicted_Premuim_MAUs_log )

# Quantify predictive accuracy: Mean Average Percentage Error (MAPE)
MAPE1 <- mean(abs((spotify_data.holdout$`Premium MAUs` - spotify_data.holdout$predicted_Premium_MAUs) / spotify_data.holdout$`Premium MAUs`))

# Plot actual versus predicted Premium MAUs
plot(spotify_data.holdout$Date, spotify_data.holdout$`Premium MAUs`, type="l", col="blue")
lines(spotify_data.holdout$Date,spotify_data.holdout$predicted_Premium_MAUs, type = "l", col = "red")
legend("topleft", legend=c("Actual Premium MAUs", "Predicted Premium MAUs"), col=c("blue", "red"), lty = 1:2, cex=0.8)

# Run log-log regression model of AD MAUs using adstock variables
regression9<-lm(log(spotify_data$`Ad MAUs`)~log(`Sales and Marketing Cost Adstock`)+log(`Research and Development Cost Adstock`)+log(`Genreal and Adminstraive Cost Adstock`),data=spotify_data)
summary(regression9)
# Predictive accuracy (based on a log-log regression using adstock variables setting lambda to 0.8)
plot(spotify_data$Date,spotify_data$`Ad MAUs`, type = 'l')
# Estimation on training data
regression10<-lm(log(spotify_data.train$`Ad MAUs`)~log(`Sales and Marketing Cost Adstock`)+log(`Research and Development Cost Adstock`)+log(`Genreal and Adminstraive Cost Adstock`),data=spotify_data.train)
# Predict AD MAUs on holdout data 
spotify_data.holdout$predicted_AD_MAUs_log <- predict(object = regression10, newdata = spotify_data.holdout) 
# Convert predicted log AD MAUs to actual AD MAUs
spotify_data.holdout$predicted_AD_MAUs <- exp(spotify_data.holdout$predicted_AD_MAUs_log )

# Quantify predictive accuracy: Mean Average Percentage Error (MAPE)
MAPE2 <- mean(abs((spotify_data.holdout$`Ad MAUs` - spotify_data.holdout$predicted_AD_MAUs) / spotify_data.holdout$`Ad MAUs`))

# Plot actual versus predicted AD MAUs
plot(spotify_data.holdout$Date, spotify_data.holdout$`Ad MAUs`, type="l", col="blue")
lines(spotify_data.holdout$Date,spotify_data.holdout$predicted_AD_MAUs, type = "l", col = "red")
legend("topleft", legend=c("Actual AD MAUs", "Predicted AD MAUs"), col=c("blue", "red"), lty = 1:2, cex=0.8)