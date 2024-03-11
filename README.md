# Chocolate Bar Ratings Analysis
## Overview
This project explores an extensive dataset of chocolate bars to identify factors influencing their ratings. Using data analysis techniques such as Gradient Boosting and Linear Discriminant Analysis, we focus on cocoa bean types, cocoa percentages, and geographical origins. Insights from this study can assist manufacturers and retailers in enhancing product quality and aligning with consumer preferences.

## Data Description and Preprocessing
The dataset includes chocolate bar ratings, bean types, cocoa percentages, and origin countries. We performed preprocessing to clean and prepare the data, including renaming columns, converting data types, and grouping bean types into meaningful categories.

## Exploratory Data Analysis (EDA)
We conducted EDA to understand the distribution of cocoa percentages, bean types, and their relation to chocolate ratings. Key findings include:

- Trinitario is the most common bean type.
- Cocoa percentage distribution centers around 70%.
- Bean type does not significantly affect the rating, suggesting that factors like cocoa percentage play a more critical role.
## Model Selection & Methodology
### Rating Prediction
Gradient Boosting Model emerged as the best approach for predicting chocolate ratings, emphasizing the importance of cocoa percent and bean type.

### Bean Type Prediction
Linear Discriminant Analysis predicted bean types based on the chocolate's geographical origin, highlighting the significance of origin in determining bean characteristics.

### Clustering Analysis
- PCA and KMeans Clustering revealed consumer preference patterns, notably a trend towards lower ratings for chocolates with higher cocoa percentages.
## Results & Business Applications
The analysis suggests focusing on specific cocoa percentages and bean types to enhance chocolate quality. Understanding the geographical influence on bean characteristics can inform sourcing strategies. Consumer preferences toward moderate cocoa levels can guide marketing strategies.
