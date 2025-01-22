

library(ggplot2)
library(dplyr)
library(wordcloud)
library(stringr)
library(lubridate)
library(tm)

# Load the dataset
movies_df <- read.csv("C:\\Users\\rajas\\OneDrive\\Desktop\\Masters\\sem-2\\imdb_movies.csv", stringsAsFactors = FALSE)

# Clean the dataset: Remove rows with missing values for simplicity
movies_df <- na.omit(movies_df)

# Extract the year from the date_x column
movies_df$date_x <- dmy(movies_df$date_x)
movies_df$year <- year(movies_df$date_x)

# 1. Find Maximum & Minimum scores, budgets, and revenues
print(paste("Max Score:", max(movies_df$score, na.rm = TRUE)))
print(paste("Min Score:", min(movies_df$score, na.rm = TRUE)))
print(paste("Max Budget:", max(movies_df$budget_x, na.rm = TRUE)))
print(paste("Min Budget:", min(movies_df$budget_x, na.rm = TRUE)))
print(paste("Max Revenue:", max(movies_df$revenue, na.rm = TRUE)))
print(paste("Min Revenue:", min(movies_df$revenue, na.rm = TRUE)))


# 2. Calculate Standard Deviation & Average values for scores, budgets, and revenues
print(paste("Standard Deviation of Score:", sd(movies_df$score, na.rm = TRUE)))
print(paste("Average Score:", mean(movies_df$score, na.rm = TRUE)))
print(paste("Standard Deviation of Budget:", sd(movies_df$budget_x, na.rm = TRUE)))
print(paste("Average Budget:", mean(movies_df$budget_x, na.rm = TRUE)))
print(paste("Standard Deviation of Revenue:", sd(movies_df$revenue, na.rm = TRUE)))
print(paste("Average Revenue:", mean(movies_df$revenue, na.rm = TRUE)))


# 3. Plot the Distribution of Movies by Genre
movies_df$primary_genre <- sapply(strsplit(as.character(movies_df$genre), ","), `[`, 1)
ggplot(movies_df, aes(x = primary_genre)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of Movies by Primary Genre", x = "Genre", y = "Count")

# 4. Check for Erroneous or Missing Values
print("Missing Values per Column:")
print(colSums(is.na(movies_df)))

# 5. Correlation Analysis between Budget and Revenue
print(paste("Correlation between Budget and Revenue:", cor(movies_df$budget_x, movies_df$revenue, use = "complete.obs")))

# 6. Time Series Analysis: Average Movie Score by Year
avg_score_by_year <- movies_df %>% group_by(year) %>% summarize(avg_score = mean(score, na.rm = TRUE))
ggplot(avg_score_by_year, aes(x = year, y = avg_score)) +
  geom_line() +
  labs(title = "Average Movie Score by Year", x = "Year", y = "Average Score")

 
movies_df$primary_genre <- sapply(strsplit(as.character(movies_df$genre), ","), `[`, 1) # Extracting primary genre
genre_revenue <- movies_df %>% group_by(primary_genre) %>% summarize(average_revenue = mean(revenue, na.rm = TRUE))

#7  Printing the average revenue by genre
print("Average Revenue by Genre:")
print(genre_revenue)
# To visualize the average revenue by genre
ggplot(genre_revenue, aes(x = reorder(primary_genre, -average_revenue), y = average_revenue)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Revenue by Genre", x = "Genre", y = "Average Revenue (USD)")



# 8. Predictive Modeling: Linear Regression of Revenue on Budget
model <- lm(revenue ~ budget_x, data = movies_df)
print(summary(model))

# 9. Text Analysis: Common Words in Movie Overviews
corpus <- Corpus(VectorSource(movies_df$overview))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
tdm <- TermDocumentMatrix(corpus)
m <- as.matrix(tdm)
word_freqs <- sort(rowSums(m), decreasing = TRUE)
wordcloud(names(word_freqs), word_freqs, max.words = 100)

# 10. Comparative Analysis: Compare Average Scores by Genre
avg_score_by_genre <- movies_df %>% group_by(primary_genre) %>% summarize(avg_score = mean(score, na.rm = TRUE))
ggplot(avg_score_by_genre, aes(x = reorder(primary_genre, -avg_score), y = avg_score)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Average Movie Score by Genre", x = "Genre", y = "Average Score")
