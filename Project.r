######## readr provides a fast and friendly way to read rectangular data
#        (like csv, tsv, and fwf). It is designed to flexibly parse many types
#        of data found in the wild, while still cleanly failing when data
#        unexpectedly changes.
library(readr)
#### read_csv() is a special case of the more general read_delim(). It's useful
#    for reading comma separated values (flat file data).
data = read_csv("./Train.csv")
#ggplot lib
library(tidyverse)
#confusion matrix lib
library(caret)
#stats
print(is.data.frame(data))
print(ncol(data))
print(nrow(data))
str(data)
summary(data)

#### Set seed for the Random Number Generator for reproducibility.
set.seed(845481568)
#### sample() takes a sample of the specified size from the data, with or without
#    replacement.
index = sample(1:nrow(data), 10000)

## Divide data into training data and testing data, which is further divided
#  into explanatory variables (x) and response variables (y).
X_training = data[index,-ncol(data)]
y_training = data[index, ncol(data)]
X_testing = data[-index, -ncol(data)]
y_testing = data[-index, ncol(data)]

######## dplyr provides a grammar of data manipulation, providing a consistent
#        set of verbs that solve the most common data manipulation challenges.
library(dplyr)
#### %>% pipes an object forward into a function or call expression.
#    %>% f(y) is equivalent to f(x, y)
#### mutate() adds new variables and preserves existing ones. New variables
#    overwrite existing variables of the same name.
#### across() makes it easy to apply the same transformation to multiple
#    columns, allowing you to use select() semantics inside in "data-masking"
#    functions like summarise() and mutate().
#### where(fn) selects the variables for which the function returns TRUE.
#### is.character returns TRUE or FALSE depending on whether its argument is of
#    character type or not.
#### as.factor coerces its argument to a factor.
X_training <-
  X_training %>% mutate(across(where(is.character), as.factor))
X_testing <-
  X_testing %>% mutate(across(where(is.character), as.factor))
y_training$Reached.on.Time_Y.N <-
  y_training$Reached.on.Time_Y.N %>% as.factor
y_testing$Reached.on.Time_Y.N <-
  y_testing$Reached.on.Time_Y.N %>% as.factor

library(randomForest)
model <-
  randomForest(y_training$Reached.on.Time_Y.N ~ . - ID - Gender - Warehouse_block,
               X_training)
prediction <- predict(model, X_testing)

library(pROC)


#Display confusion matrix
confusionMatrix(data = prediction, reference = y_testing$Reached.on.Time_Y.N, positive = "1")

print(auc(y_testing$Reached.on.Time_Y.N, as.numeric(prediction)))

#Visualization

#display product count in each warehouse
ware <- data.frame(table(data$Warehouse_block))
pie(ware$Freq,
    labels = ware$Freq,
    main = "Count of products in Warehouse Blocks",
    col = rainbow(length(ware$Var1)))
    legend("left",c("A Block","B Block","C Block", "D Block","F Block"), 
           cex = 1.25,
           fill = rainbow(length(ware$Var1)))
   
#display shipments with customer calls count
ship <- aggregate(data$Customer_care_calls ~ data$Mode_of_Shipment, FUN = sum)
barplot(ship$`data$Customer_care_calls`,names.arg = ship$`data$Mode_of_Shipment`,
        xlab = "Mode", ylab = "Calls", main = "Shipment with Customer Calls",
        col = "blue", border = "green")

#display warehouse with different ratings
ware1<-table(data$Customer_rating,data$Warehouse_block)
ratings <- c(1:5)
colors = c("green","orange","brown","yellow","red")
barplot(ware1, main = "Warehouse Rating", 
        names.arg = ware$Var1, xlab ="Blocks", ylab = "Ratings", col = colors,
        legend.text = c(1,2,3,4,5), args.legend = list(cex=1,x = "topleft"))

#display important product purchase
imp<-table(data$Product_importance,data$Gender)
barplot(imp,beside = TRUE, 
        col = c("Red", "skyblue1","yellow"), 
        main = "Who Purchased Important products", 
        width=c(2,3), xlab = "Gender", ylab = "Products",
        legend.text = rownames(imp),
        args.legend = list(cex=1,x = "topleft"))

#display Which shipment reached on time
rtime<-data[,c(3,12)]
rtime<-table(rtime)
barplot(rtime,beside = TRUE,
        main = "Which shipment reached on time",
        col = c("red","yellow","brown"), xlab = "Reached on Time", ylab = "Products",
        legend.text = rownames(rtime), names.arg = c("No", "Yes"),
        args.legend = list(cex=1.5,x = "topleft"))

data1 <- data %>% mutate(Reached.on.Time_Y.N = as.factor(Reached.on.Time_Y.N))

# Display discount density graph
ggplot(data = data1) + geom_density(aes(fill = Reached.on.Time_Y.N, x = Discount_offered, y = ..density.., alpha=0.5))

# Display weight density graph
ggplot(data = data1) + geom_density(aes(fill = Reached.on.Time_Y.N, x = Weight_in_gms, y = ..density.., alpha=0.2))
