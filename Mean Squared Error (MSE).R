y_obs<-c(-3.9, 8.3, -1.8, -1.7, -6.0, 0.9, 0.7, -0.5, 1.6, 3.7, -1.1,-3.2) #Observed data
y_pred<-c(-1.1, 2.0, 1.4, -0.8, 6.8, 3.4, 4.0, 0.8, 2.9, 10.0, 6.0, 0.8) #Predicted data
install.packages("Metrics")
library(Metrics)
mse(y_obs, y_pred) #Mean Squared Error regression loss