#import libraries
library(ggplot2)
options(repr.plot.width = 12, repr.plot.height = 8)


#importing data
data=googleplaystore
str(data)
head(data)
T1=table(data$Category)
T1

#data exploration
tail(data)

print(paste("Number of records: ", nrow(data)))
print(paste("Number of features: ", ncol(data)))

summary(data)

colnames(data) # columns names

unique(data$Category) # Unique cities

#feauture selection
maindf <- data[,c("App","Category","Rating","Reviews",
                  "Size","Installs","Type","Price","Content Rating","Genres",
                  "Last Updated","Current Ver","Android Ver")]
head(maindf)


#checking null values
sum(is.na(maindf))


#App with large number of reviews

# Convert 'Reviews' column to numeric with handling of errors
data$Reviews <- as.numeric(data$Reviews)

# Sort the data by 'Reviews' column in descending order
sorted_data <- data[order(-data$Reviews),]

# Get the value of 'App' from the first row
top_app <- sorted_data[1, 'App']
print(top_app)


#Count of Paid vs Free apps

# Print the number of Free Apps
cat("Free Apps ", sum(data$Type == "Free"), "\n")

# Print the number of Paid Apps
cat("Paid Apps ", sum(data$Type == "Paid"), "\n")




#findind apps with largest number of installs

# Remove '+' and ',' from 'Installs' column and convert to integer
data$Installs <- as.numeric(gsub("[+,]", "", data$Installs))

# Remove rows with Category '1.9'
i <- which(data$Category == '1.9')
data <- data[-i,]

# Group by 'App' and sum the 'Installs'
res <- aggregate(data$Installs, by = list(data$App), FUN = sum)

# Rename the columns
colnames(res) <- c("App", "Installs")

# Sort by 'Installs' in descending order and take the top 10
final_result <- head(res[order(-res$Installs),], 10)

# Create a bar plot
barplot(final_result$Installs, names.arg = final_result$App, col = "blue",
        main = "Top 10 Apps having Highest Installs", xlab = "Apps", 
        ylab = "Install Counts", 
        ylim = c(0, max(final_result$Installs) + 500000), las = 2, 
        cex.names = 0.7)

# Rotate x-axis labels for better visibility
axis(1, at = 1:10, labels = final_result$App, las = 2, cex.axis = 0.7)





#apps with largest size 

# Assuming you've already loaded the data into 'data'

# Remove rows where Size is 'Varies with device'
unwanted <- data[data$Size == "Varies with device",]
data <- data[!data$Size == "Varies with device",]

# Remove 'M', 'k', and '+' from 'Size' column and convert to numeric
data$Size <- as.numeric(gsub("[Mk+]", "", as.character(data$Size)))

# Sort the data by 'Size' in descending order
data <- data[order(-data$Size),]

# Get the app with the largest size
top_app <- data[1, 'App']

# Print the top app
print(top_app)
 

#Most Popular Category

# Remove rows where Size is 'Varies with device'
unwanted <- data[data$Size == "Varies with device",]
data <- data[!data$Size == "Varies with device",]

# Remove 'M', 'k', and '+' from 'Size' column and convert to numeric
data$Size <- as.numeric(gsub("[Mk+]", "", as.character(data$Size)))

# Group by 'Category' and sum the 'Size'
res <- aggregate(data$Size, by = list(data$Category), FUN = sum)

# Rename the columns
colnames(res) <- c("Category", "Size")

# Sort by 'Size' in descending order and reset the index
finaldata <- res[order(-res$Size),]
rownames(finaldata) <- NULL

# Create the bar plot
bar <- barplot(finaldata$Size, names.arg = finaldata$Category, col = "blue",
               main = "Total Size by Category", xlab = "Category", 
               ylab = "Size",
               ylim = c(0, max(finaldata$Size) + 500), las = 2, 
               cex.names = 0.7)

# Adjust x-axis labels for better visibility
axis(1, at = bar, labels = finaldata$Category, las = 2, cex.axis = 0.7)



#Data Visualization

# Define a function to compute the number of free and paid apps
compute_app_types <- function(df) {
  num_free <- sum(df$Type == "Free")
  num_paid <- sum(df$Type == "Paid")
  return(c(num_free, num_paid))
}

# Define a function to plot app type distributions across categories
plot_app_types <- function(df) {
  vc_rating <- table(df$Category)
  cat_free_apps <- numeric(length(vc_rating))
  cat_paid_apps <- numeric(length(vc_rating))
  
  for (i in 1:length(vc_rating)) {
    cat <- names(vc_rating)[i]
    sub_df <- subset(df, Category == cat)
    app_types <- compute_app_types(sub_df)
    cat_free_apps[i] <- app_types[1]
    cat_paid_apps[i] <- app_types[2]
  }
  
  par(mfrow = c(2, 1))
  barplot(cat_free_apps, main = "Number of Free Apps by Category", 
          names.arg = names(vc_rating), col = "blue")
  barplot(cat_paid_apps, main = "Number of Paid Apps by Category", 
          names.arg = names(vc_rating), col = "blue")
}

# Define a function to plot the mean of a target column grouped by a group column
plot_target_by_group <- function(df, target_col, group_col, figsize=c(6,4), 
                                 title="") {
  order <- sort(unique(df[[group_col]]))
  stats <- tapply(df[[target_col]], df[[group_col]], mean)
  
  barplot(stats, names.arg = order, col = "blue", main = title, 
          ylim = c(3.8, 4.5),
          xlab = group_col, ylab = target_col)
}

# Assuming 'df' is your dataframe, you can call these functions like this:
#num_apps <- compute_app_types(df)
#plot_app_types(df)
#plot_target_by_group(df, "TargetColumn", "GroupingColumn", c(6,4), "Title")






# Assuming you've already loaded the data into 'data'

compute_app_types <- function(df) {
  num_free <- sum(df$Type == "Free")
  num_paid <- sum(df$Type == "Paid")
  return(c(num_free, num_paid))
}

plot_app_types <- function(df) {
  vc_rating <- table(df$Category)
  cat_free_apps <- numeric(length(vc_rating))
  cat_paid_apps <- numeric(length(vc_rating))
  
  for (i in 1:length(vc_rating)) {
    cat <- names(vc_rating)[i]
    sub_df <- subset(df, Category == cat)
    app_types <- compute_app_types(sub_df)
    cat_free_apps[i] <- app_types[1]
    cat_paid_apps[i] <- app_types[2]
  }
  
  par(mfrow = c(2, 1))
  barplot(cat_free_apps, main = "Number of Free Apps by Category", names.arg = names(vc_rating), col = "blue")
  barplot(cat_paid_apps, main = "Number of Paid Apps by Category", names.arg = names(vc_rating), col = "blue")
}



# Create a bar plot for the difference in ratings
barplot(rating_diff, names.arg = sorted_idx, col = "blue",
        main = "Difference of Ratings between Paid and Free Apps Across App Categories",
        xlab = "Category", ylab = "Rating Difference", 
        ylim = c(0, max(rating_diff) + 0.1),
        las = 2, cex.names = 0.7)



install.packages("dplyr")
library(dplyr)



#Hypothesis 1 
#Hypothesis: The average user ratings of paid apps are higher than the average user ratings of free apps.

#H0: μ_paid = μ_free
#v/s
#H1: μ_paid > μ_free

#significance level - 0.05


# Load necessary libraries
library(ggplot2)
library(dplyr)

# Assuming you have a dataframe 'data' with columns 'Rating' and 'Type' (Free/Paid)

# Separate the data into two groups: Free and Paid apps
free_apps <- data %>%
  filter(Type == "Free") %>%
  select(Rating)

paid_apps <- data %>%
  filter(Type == "Paid") %>%
  select(Rating)

# Check for missing values and remove them
free_apps <- na.omit(free_apps)
paid_apps <- na.omit(paid_apps)

# Perform t-test
t_test_result <- t.test(free_apps$Rating, paid_apps$Rating, 
                        alternative = "less", na.rm = TRUE)

# Print the t-test result
print(t_test_result)

# Visualize the data using a box plot
combined_data <- bind_rows(
  mutate(free_apps, Type = "Free"),
  mutate(paid_apps, Type = "Paid")
)

ggplot(combined_data, aes(x = Type, y = Rating, fill = Type)) +
  geom_boxplot() +
  labs(title = "Comparison of User Ratings between Free and Paid Apps") +
  theme_minimal()


# Assuming you have conducted the t-test and have the result in 't_test_result'

# Create a bar plot to support the hypothesis
library(ggplot2)

# Extract the means and standard errors from the t-test result
means <- c(mean(free_apps$Rating), mean(paid_apps$Rating))
std_errors <- c(sd(free_apps$Rating)/sqrt(length(free_apps$Rating)), 
                sd(paid_apps$Rating)/sqrt(length(paid_apps$Rating)))

# Create a data frame for plotting
plot_data <- data.frame(Type = c("Free", "Paid"), Mean_Rating = means, 
                        Std_Error = std_errors)

# Plot the bar graph
ggplot(plot_data, aes(x = Type, y = Mean_Rating, fill = Type)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.7) +
  geom_errorbar(aes(ymin = Mean_Rating - Std_Error, ymax = Mean_Rating + Std_Error),
                position = position_dodge(width = 0.9), width = 0.25, linewidth = 0.7) +
  labs(title = "Comparison of Average User Ratings between Free and Paid Apps",
       x = "App Type", y = "Average Rating") +
  theme_minimal()







# Separate the data into two groups: Free and Paid apps
free_apps <- data[data$Type == "Free", "Rating"]
paid_apps <- data[data$Type == "Paid", "Rating"]

# Perform Mann-Whitney U test
mwu_test_result <- wilcox.test(free_apps, paid_apps, alternative = "less")

# Print the Mann-Whitney U test result
print(mwu_test_result)




# Separate the data into two groups: Free and Paid apps
free_apps <- data[data$Type == "Free", "Rating"]
paid_apps <- data[data$Type == "Paid", "Rating"]

# Perform Welch's t-test
welch_test_result <- t.test(free_apps, paid_apps, alternative = "less", 
                            var.equal = FALSE)

# Print the Welch's t-test result
print(welch_test_result)

#conclusion 
#Reject null hypothesis: The average user ratings of paid apps are higher than free apps.










#Hypothesis 2
#Hypothesis: The size of an app (in MB) is correlated with the number of installations.

#H0: ρ = 0 (where ρ is the correlation coefficient)
#v/s 
#H1: H1: ρ ≠ 0

#significance level - 0.05

#Pearson's Correlation Coefficient Test

# Calculate Pearson correlation coefficient
correlation <- cor(data$Size, data$Installs, method = "pearson")

# Print the correlation coefficient
print(paste("Pearson Correlation Coefficient:", round(correlation, 4)))

# Perform a hypothesis test for correlation
cor_test_result <- cor.test(data$Size, data$Installs, method = "pearson")

# Print the correlation test result
print(cor_test_result)



#Spearman's Rank Correlation Test

# Calculate Spearman's rank correlation coefficient
correlation <- cor(data$Size, data$Installs, method = "spearman")

# Print the correlation coefficient
print(paste("Spearman's Rank Correlation Coefficient:", round(correlation, 4)))

# Perform a hypothesis test for correlation
cor_test_result <- cor.test(data$Size, data$Installs, method = "spearman")

# Print the correlation test result
print(cor_test_result)


#Scatter Plot for Visualization
# Assuming you have a dataframe 'data' with columns 'Size' and 'Installs'

# Create a scatter plot to visualize the relationship
plot(data$Size, data$Installs, main = "Scatter Plot: Size vs. Installs",
     xlab = "Size (MB)", ylab = "Installs", pch = 16)




#Hypothesis 3
#Hypothesis: Paid apps have higher average ratings compared to free apps.

#H0: μ_paid = μ_free
#v/s
#H1: μ_paid > μ_free


#significance level - 0.05


#Two-Sample T-Test

# Separate the data into two groups: Free and Paid apps
free_apps <- data[data$Type == "Free", "Rating"]
paid_apps <- data[data$Type == "Paid", "Rating"]

# Perform a two-sample t-test
t_test_result <- t.test(free_apps, paid_apps, alternative = "less")

# Print the t-test result
print(t_test_result)



#Mann-Whitney U Test

# Perform Mann-Whitney U test
mwu_test_result <- wilcox.test(free_apps, paid_apps, alternative = "less")

# Print the Mann-Whitney U test result
print(mwu_test_result)


#Box Plot for Visualization

# Create a box plot to visualize the distribution of ratings by app type
boxplot(Rating ~ Type, data = data, col = c("blue", "orange"), 
        main = "Box Plot: Ratings by App Type", xlab = "App Type", 
        ylab = "Rating")










# Assuming 'data' is your data frame containing the Google Play Store dataset

# Frequency table for 'Category'
category_freq <- table(data$Category)
cat_freq_df <- as.data.frame(category_freq)
cat_freq_df

# Frequency table for 'Rating'
rating_freq <- table(data$Rating)
rating_freq_df <- as.data.frame(rating_freq)
rating_freq_df

# Frequency table for 'Reviews'
reviews_freq <- table(data$Reviews)
reviews_freq_df <- as.data.frame(reviews_freq)
reviews_freq_df

# Frequency table for 'Size'
size_freq <- table(data$Size)
size_freq_df <- as.data.frame(size_freq)
size_freq_df

# Frequency table for 'Installs'
installs_freq <- table(data$Installs)
installs_freq_df <- as.data.frame(installs_freq)
installs_freq_df

# Frequency table for 'Type'
type_freq <- table(data$Type)
type_freq_df <- as.data.frame(type_freq)
type_freq_df

# Frequency table for 'Price'
price_freq <- table(data$Price)
price_freq_df <- as.data





# Assuming 'data' is your data frame containing the Google Play Store dataset

# Find the frequency of each Android version
android_version_freq <- table(data$`Android Ver`)

# Find the Android version with the highest frequency
most_common_android_version <- names(android_version_freq)[which.max(android_version_freq)]

# Print the most common Android version
cat("The most common Android version is:", most_common_android_version)

install.packages("BSDA")
library("BSDA")

s1<-subset(data,Category=='PRODUCTIVITY')
sigma1=sd(s1$Installs)
s2<-subset(data,Category=='LIFESTYLE')
sigma2=sd(s2$Installs)
z.test(s1$Installs,s2$Installs,alternative='greater',sigma.x=sigma1,sigma.y=sigma2,conf.level=0.95)
