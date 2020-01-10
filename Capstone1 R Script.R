################################
# 2019/12/16
# Matthew Manasterski, MBA
# PH125.9x - Capstone Project - MovieLens Project
#

################################
# Create edx set, validation set -  next few lines provided by edX
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#### End of the code provided by edX


#####################################
## Data Exploration and Visualization
#####################################

# We first look at the structure of the datasets.
# Train edx data:
head(edx)

# Test validation data:
head(validation)

# Before exploring the data check for missing values in edx, strip if any found
any(is.na(edx))

any(is.na(validation))
# No missing values

# Structure of the dataset
str(edx)

# Summary of the structure
summary(edx)

# Number of rows and columns
nrow(edx)
ncol(edx)
dim(edx)

# Number of distinc movies
n_distinct(edx$movieId)

# Number of distinct members
n_distinct(edx$userId)


## What are the five most given ratings in order from most to least?
edx %>% group_by(rating) %>% summarize(count = n()) %>% top_n(5) %>%
  arrange(desc(count))  

ggplot(edx,aes(x=rating)) + 
  geom_histogram(binwidth=0.5,color='darkblue',fill='lighblue') + 
  xlab('Movie Ratings')+ scale_x_discrete(limits = c(seq(0.5,5,0.5))) + 
  ylab('Occurences') + scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) + 
  ggtitle('Movie Ratings')

# From the plot and the tibble we can see rating of 4 has most occurances followed by 3 and then 5. Half ratings are not used as often

## Explore Movie rating by occurance.

# Show movies with the greatest number of ratings
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Show movies with the least number of ratings
edx %>% group_by(movieId, title) %>%
  summarize(count = n()) %>%
  arrange(count)

edx %>% count(movieId) %>%
ggplot(aes(x=n)) + 
  geom_histogram(bins=30,color='darkblue',fill='lightblue') + scale_x_log10() +
  ggtitle('Movie Ratings per Movie')


##Explore number of ratings per User
# Show Users with the greatest number of ratings
edx %>% group_by(userId) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# Show users with the least number of ratings
edx %>% group_by(userId) %>%
  summarize(count = n()) %>%
  arrange(count)

edx %>% count(userId) %>% 
  ggplot(aes(x=n)) + geom_histogram(bins=30,color='darkblue',fill='lightblue') + scale_x_log10()  +
  ggtitle('User Ratings per User')

##From the above we can see some users are more active than others 

##Explore age at the time of rating, release date, and rating date of the movie
# Timestamp is not really readable for us. Let's transform it into a year of the review
# We will require lubridate library for that.
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# Get year of the rating
edx <- mutate(edx, ratingYear = year(as_datetime(timestamp)))

# As we see from the structure of the title in the dataset, 
#each title ends with the release date in brackets, this let us easily seperate the release date into a another column
edx <- edx %>% mutate(releaseYear = as.numeric(str_sub(title, -5, -2)))

#Function to calculate age of the movie at the time of the rating
calculateAgeAtRating <- function(x,y){
  return(x-y)
}

# Calculate the age of the movie at the time of the rating
edx$ageAtRating <- mapply(calculateAgeAtRating, edx$ratingYear, edx$releaseYear)
#Show new structure with additional three columns
head(edx)

#Plot average rating by year movie was rated
edx %>% group_by(ratingYear) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(ratingYear,rating)) +
  geom_point() +
  geom_smooth() +
  theme_bw()
#Since the ratings only started in 1995 this does not seem like it will be very usful

#Plot average rating by year movie was released
edx %>% group_by(releaseYear) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(releaseYear,rating)) +
  geom_point() +
  geom_smooth() +
  theme_bw()
# It does seem like movies between 1930 to 1960 were reated the highest we might use this later

#Plot average rating by the age of the movie when it was rated
edx %>% group_by(ageAtRating) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(ageAtRating,rating)) +
  geom_point() +
  geom_smooth() +
  theme_bw()


#Data Analysis and Modeling

# Before Analysing the data and coming up with the best model we will split edx dataset
# to create a train and test sets.
#set.seed(755)
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train <- edx[-test_index,]
test_set <- edx[test_index,]

test <- test_set %>% 
  semi_join(train, by = "movieId") %>%
  semi_join(train, by = "userId")

# Add rows removed from validation set back into edx set

removed_set <- anti_join(test_set, test)
train <- rbind(train, removed_set)

rm(test_set, removed_set)



# RSME function
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Simple model is just average movie rating across all movies.
#We depict mean movie rating with mu.

mu <- mean(train$rating)
mu

# Calulate RSME for mean prediction
# We will use the following formula

mean_rmse <- RMSE(test$rating, mu)
mean_rmse

# Change in % between original mean_rmse and current model.
change=0

# We will store the results in the data frame so we can keepn track of the progress and improvements we make.
rmse_results <- data_frame(method = "Mean movie rating model", 
                           RMSE = mean_rmse, 
                           Change = change )
rmse_results %>% knitr::kable()

# Improve model by including movie effect
# estimate movie bias 'b_i' for all movies

mu <- mean(train$rating)
movie_avgs <- train %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

movie_avgs
# plot with above bias
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 30, 
                     data = ., color = I("darkblue"), 
                     fill = I("lightblue"),
                     main = "Movie Averages with the computed b_i")

# new predictions with above movie bias
predicted_ratings <- mu + test %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

# calculate rmse after modeling movie bias
moviebias_rmse <- RMSE(predicted_ratings, test$rating)

moviebias_rmse

#re-calculate change
change = (((mean_rmse - moviebias_rmse)/mean_rmse) * 100)

rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie Bias model",  
                                     RMSE = moviebias_rmse, Change = change))
rmse_results %>% knitr::kable()

## Next we add User bias effect to Movie bias model 
# We will show the bias for user with more then 100 ratings.

# Plot user bias effect for users with at least 100 ratings
mu <- mean(train$rating)
user_100avg<- train %>% 
  na.omit() %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_100avg %>% qplot(b_u, geom ="histogram", bins = 30, 
                      data = ., color = I("darkblue"), 
                      fill = I("lightblue"), 
                      main = "User Averages with the computed b_u")


user_avgs <- train %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

# rmse results with User bias effect and Movie bias model
predicted_ratings <- test %>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  na.omit() %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)
  
user_movie_bias_rmse <- RMSE(predicted_ratings, test$rating)
user_movie_bias_rmse

#re-calculate change
change = (((mean_rmse - user_movie_bias_rmse)/mean_rmse) * 100)
# Update rmse_results table 
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="User and Movie Bias model",  
                                     RMSE = user_movie_bias_rmse, 
                                     Change = change ))

rmse_results %>% knitr::kable()

# Next we will use regularization to that will shrink b_i and b_u
# Use lamda from 0 to 10, incements of .25
lambdas <- seq(0, 10, 0.25)

reg_rmse <- sapply(lambdas, function(l){
  
  mu <- mean(train$rating)
  
  b_i <- train %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$rating))
})

#lowest regularized rmse

reg_optimal_rmse <- min(reg_rmse)
reg_optimal_rmse

# Plot rmses vs lambdas to select the optimal lambda                                                             
qplot(lambdas, reg_rmse)  

# Find optimal lambda                                                             
lambda <- lambdas[which.min(reg_rmse)]
lambda

#re-calculate change
change = (((mean_rmse - reg_optimal_rmse)/mean_rmse) * 100)
# Update rmse_results table                                                            
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized movie/user bias model",  
                                     RMSE = reg_optimal_rmse, 
                                     Change = change))

# print rmse_ results
rmse_results %>% knitr::kable()

# We could go further and use Release Year or Age at the raing but since we achieved our RSME goal we will stop here
# Before our final test we should make sure edx and validation have the same # varialbles
# So we should add the three date columns we added
head(edx)
edx <- select(edx, -ratingYear, -releaseYear, -ageAtRating)
head(edx)



# Run a final Test with Our original Validation data
l <- lambda

mu <- mean(edx$rating)

# re-calculate initial mean RSME against edx set
mean_rmse <- RMSE(validation$rating, mu)
mean_rmse

b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+l))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+l))

predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

final_rmse <- RMSE(predicted_ratings, validation$rating)

final_rmse

#re-calculate change
change = (((mean_rmse - final_rmse)/mean_rmse) * 100)


# Add Validation set to the results table
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularized movie/user model- Validation",  
                                     RMSE = final_rmse, 
                                     Change = change))

# print rmse_ results
rmse_results %>% knitr::kable()



