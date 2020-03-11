################################
# Create edx set, validation set
################################
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 +                  col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           +             title = as.character(title),
                                           +                                            genres = as.character(genres))
movielens <- left_join(ratings, movies, by = "movieId")
# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]
# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  +     semi_join(edx, by = "movieId") %>%
  +     semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)
rm(dl, ratings, movies, test_index, temp, movielens, removed)
#Data Analysis &  Visualisation
str(edx)
summary(edx$rating)
#Distribution of Movie Ratings
edx %>% group_by(movieId) %>% summarize(n = n()) %>%
  +     ggplot(aes(n)) + geom_histogram(fill = "yellow4", color = "red3", bins = 10) +
  +     scale_x_log10() + ggtitle("Movies Ratings Distribution")
#Distribution of Users
edx %>% group_by(userId) %>% summarize(n = n()) %>%
  +     ggplot(aes(n)) + geom_histogram(fill = "red3", color = "black", bins = 10) +
  +     scale_x_log10() + ggtitle("Distribution of Users Ratings")
#Method
# Optimal Least Squares
RMSE <- function(true_ratings, predicted_ratings){
  +     sqrt(mean((true_ratings - predicted_ratings)^2))
}
lambdas <- seq(0, 5, 0.25)
rmses <- sapply(lambdas,function(l){
  +     
    +     #Calculate the mean of ratings from the edx training set
    +     mu <- mean(edx$rating)
    +     
      +     #Adjust mean by movie effect and penalize low number on ratings
      +     b_i <- edx %>% 
        +         group_by(movieId) %>%
        +         summarize(b_i = sum(rating - mu)/(n()+l))
      +     
        +     # adjust mean by user and movie effect and penalize low number of ratings
        +     b_u <- edx %>% 
          +         left_join(b_i, by="movieId") %>%
          +         group_by(userId) %>%
          +         summarize(b_u = sum(rating - b_i - mu)/(n()+l))
        +     
          +     #predict ratings in the training set to derive optimal penalty value 'lambda'
          +     predicted_ratings <- 
            +         edx %>% 
            +         left_join(b_i, by = "movieId") %>%
            +         left_join(b_u, by = "userId") %>%
            +         mutate(pred = mu + b_i + b_u) %>%
            +         .$pred
          +     
            +     return(RMSE(predicted_ratings, edx$rating))
          })
plot(lambdas, rmses)
# RMSE 0.8566952
lambda
#lambda 0.5
# This shows that RMSE is minimal at a Lambda of 0.5
# Also, the minimum RMSE is better by 10% of 0.86490
# Now, predicting on the validation set based on the Lambda obtained
pred_y_lse <- sapply(lambda,function(l){
  +     
    +     #Derive the mearn from the training set
    +     mu <- mean(edx$rating)
    +     
      +     #Calculate movie effect with optimal lambda
      +     b_i <- edx %>% 
        +         group_by(movieId) %>%
        +         summarize(b_i = sum(rating - mu)/(n()+l))
      +     
        +     #Calculate user effect with optimal lambda
        +     b_u <- edx %>% 
          +         left_join(b_i, by="movieId") %>%
          +         group_by(userId) %>%
          +         summarize(b_u = sum(rating - b_i - mu)/(n()+l))
        +     
          +     #Predict ratings on validation set
          +     predicted_ratings <- 
            +         validation %>% 
            +         left_join(b_i, by = "movieId") %>%
            +         left_join(b_u, by = "userId") %>%
            +         mutate(pred = mu + b_i + b_u) %>%
            +         .$pred #validation
          +     
            +     return(predicted_ratings)
})
#Conclusion
# The optimal least square method turns out to be better for this algorithm

