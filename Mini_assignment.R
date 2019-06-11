# Create a file path from which to read the file
path <- file.path("~", "desktop", "vegas_hotel_reviews.csv")
# Read in the CSV file and store it to a variable
mydata <- read.table(path, header = TRUE, sep = ",",
                     stringsAsFactors = FALSE) 
# Create a dichotomous variable for traveler types
mydata$business <- ifelse(mydata$Traveler.type == "Business", 1,0)
# Create a dichotomous variable for low and high scores
mydata$lowscore <- ifelse(mydata$Score < 5, 1,0)
View(mydata)
# Create a simple logistic model
m <- glm(mydata$business ~ mydata$lowscore, family = binomial)
summary(m)
# Calculate the odds ratio
exp(coef(m))

library(pROC)
mydata$prob <- predict(m, type=c("response"))
g <- roc(mydata$business ~ mydata$prob)
g
plot(g)
plot(1-g$specificities, g$sensitivities, type = "l", xlab = "1-specificity",
     ylab = "Sensitivity", main = "ROC curve")
abline(a=0, b=1)
grid()
