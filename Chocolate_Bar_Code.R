########################
########Packages########
########################

install.packages("randomForest")
library(randomForest)
install.packages("tree")
install.packages("rpart.plot")
library(tree)
library(rpart)				        
library(rpart.plot)	
library(MASS)
library(klaR)
library(ggplot2)
install.packages("gbm")
library(gbm)
library(caret)
install.packages("car")
library(car)
install.packages("ggfortify")
library(ggfortify)
library(reshape2)
install.packages("ggcorrplot")
library(ggcorrplot)



############################################
########### Pre-processing & EDA ###########
############################################


#Load and View data
chocolate_data = read.csv("C:/Users/vince/Downloads/Dataset 4 — Chocolate bar ratings.csv")
View(chocolate_data)


#### Pre-Processing ####


#Change column names for ease of use in the program
colnames(chocolate_data) = c('Company','BeanOriginBarName','REF','ReviewDate','CocoaPercent','CompanyLocation',
                  'Rating','BeanType','BroadBeanOrigin') 

# Remove the '%' sign and convert to numeric
chocolate_data$CocoaPercent = as.numeric(gsub("%", "", chocolate_data$CocoaPercent))

# Check the structure to ensure the conversion was successful
str(chocolate_data)

# Checking the different types of chocolate bean
unique_bean_types = unique(chocolate_data$BeanType)

# Printing the list of unique Bean Types
unique_bean_types

# Listing all the bean types found 
specified_bean_types = c(
  "Criollo", "Trinitario", "Forastero (Arriba)", "Forastero", "Forastero (Nacional)", 
  "Criollo, Trinitario", "Criollo (Porcelana)", "Blend", "Trinitario (85% Criollo)", 
  "Forastero (Catongo)", "Forastero (Parazinho)", "Trinitario, Criollo", "CCN51", 
  "Criollo (Ocumare)", "Nacional", "Criollo (Ocumare 61)", "Criollo (Ocumare 77)", 
  "Criollo (Ocumare 67)", "Criollo (Wild)", "Beniano", "Amazon mix", "Trinitario, Forastero", 
  "Forastero (Arriba) ASS", "Criollo, +", "Amazon", "Amazon, ICS", "EET", 
  "Blend-Forastero,Criollo", "Trinitario (Scavina)", "Criollo, Forastero", "Matina", 
  "Forastero(Arriba, CCN)", "Nacional (Arriba)", "Forastero (Arriba) ASSS", "Forastero, Trinitario", 
  "Forastero (Amelonado)", "Trinitario, Nacional", "Trinitario (Amelonado)", "Trinitario, TCGA", 
  "Criollo (Amarru)"
)


# Removing all the Empty values of bean type (I did it this way since there are special characters Rstudio doesnt read)
chocolate_data = chocolate_data[chocolate_data$BeanType %in% specified_bean_types, ]


# Extracting all unique Bean Origin values
unique_broad_bean_origins = unique(chocolate_data$BroadBeanOrigin)

# Printing the list of unique Bean Origins
unique_broad_bean_origins

# Specified Broad Bean Origins found in dataset
specified_broad_bean_origins = c("Venezuela", "Madagascar", "Peru", "Fiji", "Ecuador", "Mexico", "Indonesia", "Brazil",
"Trinidad", "Vietnam", "Nicaragua", "Tanzania", "Dominican Republic", "Ghana", "Belize",
"Jamaica", "Grenada", "Congo", "Colombia", "Honduras", "Philippines", "Cuba",
"Dominican Rep., Bali", "Carribean", "Haiti", "Sao Tome", "Papua New Guinea",
"Costa Rica", "Domincan Republic", "Bolivia", "Uganda", "Malaysia", "Sao Tome & Principe",
"Vanuatu", "Guatemala", "West Africa", "Hawaii", "St. Lucia", "Ven., Indonesia, Ecuad.",
"Peru, Ecuador, Venezuela", "Peru, Belize", "Peru, Mad., Dom. Rep.",
"PNG, Vanuatu, Mad", "South America", "Trinidad, Ecuador", "Cameroon",
"Venezuela, Java", "Venezuela/ Ghana", "Venezuela, Ghana", "Indonesia, Ghana",
"Principe", "Ven., Trinidad, Mad.", "Carribean(DR/Jam/Tri)", "Ghana & Madagascar",
"Madagascar & Ecuador", "Dom. Rep., Madagascar", "Gre., PNG, Haw., Haiti, Mad",
"Mad., Java, PNG", "DR, Ecuador, Peru", "Sri Lanka", "Suriname",
"Ecuador, Mad., PNG", "Ghana, Panama, Ecuador", "Tobago", "Venezuela, Carribean",
"India"
)

# Filtering the dataset for specified Broad Bean Origins (same case to bean type)
chocolate_data = chocolate_data[chocolate_data$BroadBeanOrigin %in% specified_broad_bean_origins, ]

# Checking the filtered dataset
table(chocolate_data$BroadBeanOrigin)

# Define the mapping to group the bean types into 5 groups
beanTypeMapping = list(
  "Criollo" = c("Criollo", "Criollo (Porcelana)", "Criollo (Ocumare)", "Criollo (Ocumare 61)", 
                "Criollo (Ocumare 77)", "Criollo (Ocumare 67)", "Criollo (Wild)", "Criollo (Amarru)", 
                "Criollo, +"),
  "Trinitario" = c("Trinitario", "Trinitario (85% Criollo)", "Trinitario, Criollo", 
                   "Trinitario (Scavina)", "Trinitario, Forastero", "Trinitario, Nacional", 
                   "Trinitario (Amelonado)", "Trinitario, TCGA"),
  "Forastero" = c("Forastero", "Forastero (Arriba)", "Forastero (Nacional)", "Forastero (Catongo)", 
                  "Forastero (Parazinho)", "Forastero (Arriba) ASS", "Forastero(Arriba, CCN)", 
                  "Forastero (Arriba) ASSS", "Forastero, Trinitario", "Forastero (Amelonado)"),
  "Blend" = c("Blend","Blend-Forastero,Criollo", "Criollo, Trinitario","Criollo, Forastero"),
  "Others" = c("Nacional","Nacional (Arriba)","CCN51", "Beniano", "Amazon mix", "Amazon", "Amazon, ICS", "EET", "Matina")
)

unknown_bean_types = c()

# Map each bean type to its group and record unknown types
mapBeanType = function(beanType) {
  for (group in names(beanTypeMapping)) {
    if (beanType %in% beanTypeMapping[[group]]) {
      return(group)
    }
  }
  unknown_bean_types = c(unknown_bean_types, beanType)
  return("Unknown")
}

# Apply the mapping to the dataset
chocolate_data$BeanType = sapply(chocolate_data$BeanType, mapBeanType)

# Check the unknown bean types
unique(unknown_bean_types)


# Check the changes
table(chocolate_data$BeanType)

##### EDA #####

# Counting the occurrences of each Bean Type
beanTypeCounts = table(chocolate_data$BeanType)

# Converting to dataframe for ggplot
beanTypeCountsDF = as.data.frame(beanTypeCounts)

# Renaming columns for clarity
colnames(beanTypeCountsDF) = c('BeanType', 'Count')

####Histograms and boxplots####

# Plotting the count of each BeanType
ggplot(beanTypeCountsDF, aes(x = BeanType, y = Count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Count of Each Bean Type", x = "Bean Type", y = "Count")


# Histogram for CocoaPercent
ggplot(chocolate_data, aes(x = CocoaPercent)) + 
  geom_histogram(bins = 30) +
  labs(title = "Distribution of Cocoa Percent")

# Boxplot for Rating by BeanType
ggplot(chocolate_data, aes(x = BeanType, y = Rating)) + 
  geom_boxplot() +
  labs(title = "Rating by Bean Type")

####Correlation matrix####

# Taking a numerical columns in the table
cor_matrix = cor(chocolate_data[, sapply(chocolate_data, is.numeric)])

# Visualize the correlation matrix
ggcorrplot(cor_matrix, method = "circle")



###################################
####### Random Forest Model #######
###################################


myforest=randomForest(Rating~Company
                      +BeanOriginBarName
                      +ReviewDate
                      +CocoaPercent
                      +CompanyLocation
                      +BeanType
                      +BroadBeanOrigin
                      , ntree=1000, data=chocolate_data, importance=TRUE,  na.action = na.omit)
myforest

importance(myforest)
varImpPlot(myforest)

myforest1=randomForest(Rating~Company
                      #+BeanOriginBarName
                      #+ReviewDate
                      +CocoaPercent
                      +CompanyLocation
                      +BeanType
                      #+BroadBeanOrigin
                      , ntree=1000, data=chocolate_data, importance=TRUE,  na.action = na.omit)
myforest1

importance(myforest1)
varImpPlot(myforest1)

#least number of variables (for test purposes, we can see that this might not capture the whole data variations)
myforest2=randomForest(Rating~CocoaPercent
                      +BeanType
                      , ntree=1000, data=chocolate_data, importance=TRUE,  na.action = na.omit)
myforest2

importance(myforest2)
varImpPlot(myforest2)



##########################
######## GBM Model #######
##########################

#Setting the categorical variables
chocolate_data$Company = as.factor(chocolate_data$Company)
chocolate_data$BeanType = as.factor(chocolate_data$BeanType)
chocolate_data$CompanyLocation = as.factor(chocolate_data$CompanyLocation)
chocolate_data$BeanOriginBarName = as.factor(chocolate_data$BeanOriginBarName)
chocolate_data$BroadBeanOrigin = as.factor(chocolate_data$BroadBeanOrigin)


set.seed(1)  # For reproducibility

gbm_model = gbm(Rating ~#Company
                 #+BeanOriginBarName
                 #+ReviewDate
                 +CocoaPercent
                 #+CompanyLocation
                 +BeanType
                 #+BroadBeanOrigin
                 ,data = chocolate_data,
                 distribution = "gaussian",
                 n.trees = 10000,
                 interaction.depth = 4)

summary(gbm_model)


#Getting the MSE
predicted_score=predict(gbm_model, newdata=chocolate_data, n.trees=10000)
mean((predicted_score - chocolate_data$Rating)^2)


# Setting the seed for reproducibility
set.seed(1)

# Splitting the data into training and testing sets (e.g., 80% training, 20% testing)
split = createDataPartition(chocolate_data$Rating, p = 0.5, list = FALSE)
training_set = chocolate_data[split, ]
testing_set = chocolate_data[-split, ]

# Training the model on the training set
gbm_model = gbm(Rating ~ CocoaPercent + BeanType,
                 data = training_set,
                 distribution = "gaussian",
                 n.trees = 10000,
                 interaction.depth = 4,
                 shrinkage = 0.01,
                 cv.folds = 5)

# Making predictions on the test set
predicted_score = predict(gbm_model, newdata=testing_set, n.trees=1000)

# Calculating Mean Squared Error (MSE) on the test set
mse = mean((predicted_score - testing_set$Rating)^2)
mse

#########################
####### LDA Model #######
#########################

# Ensure BeanType is a factor
chocolate_data$BeanType = as.factor(chocolate_data$BeanType)

# Run LDA
lda_model = lda(BeanType ~ Rating +Company
                   +BeanOriginBarName
                   +ReviewDate
                   +CocoaPercent
                   +CompanyLocation
                   +BroadBeanOrigin
                 , data = chocolate_data)

# Summary of the model
summary(lda_model)

#predictions and cross-validation
predicted = predict(lda_model, chocolate_data)
conf_matrix_lda = table(predicted$class, chocolate_data$BeanType)  # Confusion matrix
conf_matrix_lda
# Calculate accuracy
accuracy_lda = sum(diag(conf_matrix_lda)) / sum(conf_matrix_lda)
accuracy_lda

#Definitely Overfitting, lets drop company and BeanOriginBarName

lda_model = lda(BeanType ~ Rating +
                +ReviewDate
                +CocoaPercent
                +CompanyLocation
                +BroadBeanOrigin
                , data = chocolate_data)

# Summary of the model
summary(lda_model)

#predictions and cross-validation
predicted = predict(lda_model, chocolate_data)
conf_matrix_lda = table(predicted$class, chocolate_data$BeanType)  # Confusion matrix
conf_matrix_lda
# Calculate accuracy
accuracy_lda = sum(diag(conf_matrix_lda)) / sum(conf_matrix_lda)
accuracy_lda

#Lets drop everything but the bean origin

lda_model = lda(BeanType ~BroadBeanOrigin
                , data = chocolate_data)

# Summary of the model
summary(lda_model)

#predictions and cross-validation
predicted = predict(lda_model, chocolate_data)
conf_matrix_lda = table(predicted$class, chocolate_data$BeanType)  # Confusion matrix
conf_matrix_lda
# Calculate accuracy
accuracy_lda = sum(diag(conf_matrix_lda)) / sum(conf_matrix_lda)
accuracy_lda

############################
########### PCA ############
############################


# Dummifying the 'BeanType' variable
dummies = model.matrix(~ BeanType - 1, data = chocolate_data)
dummies = as.data.frame(dummies)

# Combining with other variables
pca_data = cbind(chocolate_data[c('ReviewDate', 'CocoaPercent', 'Rating')], dummies)

# Perform PCA 
pca_model = prcomp(pca_data, scale. = TRUE)

# Visualize PCA results
autoplot(pca_model, data = pca_data, loadings = TRUE, loadings.label = TRUE, scale = 0)




# Dummifying the 'BeanType' and 'BroadBeanOrigin' variables
dummies_BeanType = model.matrix(~ BeanType - 1, data = chocolate_data)
dummies_BroadBeanOrigin = model.matrix(~ BroadBeanOrigin - 1, data = chocolate_data)

# Convert to data frames
dummies_BeanType = as.data.frame(dummies_BeanType)
dummies_BroadBeanOrigin <- as.data.frame(dummies_BroadBeanOrigin)

# Combining with numerical variables
pca_data2 = cbind(chocolate_data[c('CocoaPercent', 'Rating', 'ReviewDate')], 
                       dummies_BeanType, dummies_BroadBeanOrigin)
pca_model2 = prcomp(pca_data2, scale. = TRUE)

autoplot(pca_model2, data = pca_data2, loadings = TRUE, loadings.label = TRUE, scale = 0)


############################
######### KMeans ###########
############################


# Convert to data frames
dummies_BeanType1 = as.data.frame(dummies_BeanType)

# Combining with numerical variables
kmeans_data = cbind(chocolate_data[c('CocoaPercent', 'Rating', 'ReviewDate')], 
                  dummies_BeanType1)


# Determine the optimal number of clusters (this is data maning course the elbow method)
# Create a vector to store WSS for each k
wss <- numeric(10)

# Compute WSS for k from 1 to 10
for (k in 1:10) {
  set.seed(1)  # For reproducibility
  kmeans_result <- kmeans(kmeans_data, centers = k, nstart = 25)
  wss[k] <- kmeans_result$tot.withinss
}

# Plotting the Elbow Method
plot(1:10, wss, type = "b", xlab = "Number of Clusters", ylab = "Total Within-Cluster Sum of Squares", 
     main = "Elbow Method for Determining Optimal Number of Clusters")


# Run K-means clustering, i chose 5 as number of clusters
set.seed(1)  # For reproducibility
kmeans_result = kmeans(kmeans_data, 5)

# Analyze the results
print(kmeans_result)
table(kmeans_result$cluster)  # Cluster sizes

# Add the cluster assignments to the data

kmeans_data$cluster = as.factor(kmeans_result$cluster)

# Plotting each pair of predictors
ggplot(kmeans_data, aes(x = CocoaPercent, y = Rating, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering: CocoaPercent vs Rating", x = "CocoaPercent", y = "Rating")

# Plot for 'CocoaPercent' vs 'ReviewDate'
ggplot(kmeans_data, aes(x = CocoaPercent, y = ReviewDate, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering: CocoaPercent vs ReviewDate", x = "CocoaPercent", y = "ReviewDate")

# Plot for 'Rating' vs 'ReviewDate'
ggplot(kmeans_data, aes(x = Rating, y = ReviewDate, color = cluster)) +
  geom_point() +
  labs(title = "K-means Clustering: Rating vs ReviewDate", x = "Rating", y = "ReviewDate")


