library(wordcloud)
library(RColorBrewer)
library(tm)
library(dplyr)
library(tidytext)
library(ggplot2)
library(flextable)
library(webshot2)
library(magick)

# Load the cleaned dataset
load("cleaned_data.rdata")

# Combine all the hashtag columns into a single column
merged_data <- merged_data %>%
  mutate(hashtags = paste(hashtag_1, hashtag_2, hashtag_3, hashtag_4, hashtag_5, hashtag_6, sep = " "))

# Create a corpus from the hashtags
corpus <- Corpus(VectorSource(merged_data$hashtags))

# Perform text preprocessing
corpus <- tm_map(corpus, content_transformer(tolower))  # Convert to lowercase
corpus <- tm_map(corpus, removePunctuation)  # Remove punctuation
corpus <- tm_map(corpus, removeNumbers)  # Remove numbers
corpus <- tm_map(corpus, removeWords, c(stopwords("english"), "#"))  # Remove stopwords and '#' symbol
corpus <- tm_map(corpus, stripWhitespace)  # Remove extra whitespace

# Create a term-document matrix
tdm <- TermDocumentMatrix(corpus)

# Convert the term-document matrix to a matrix
m <- as.matrix(tdm)

# Calculate word frequencies
word_freqs <- sort(rowSums(m), decreasing = TRUE)

# Create a data frame with words and their frequencies
word_freqs_df <- data.frame(word = names(word_freqs), freq = word_freqs)

# Calculate the average likes for each hashtag
hashtag_likes <- merged_data %>%
  select(hashtags, Likes) %>%
  unnest_tokens(hashtag, hashtags, token = "regex", pattern = "\\S+") %>%
  group_by(hashtag) %>%
  summarize(avg_likes = mean(Likes, na.rm = TRUE))

# Merge the hashtag frequencies and average likes
word_freqs_df <- merge(word_freqs_df, hashtag_likes, by.x = "word", by.y = "hashtag", all.x = TRUE)
word_freqs_df$avg_likes[is.na(word_freqs_df$avg_likes)] <- 0  # Replace NA with 0

# Identify the most popular hashtags
popular_hashtags <- word_freqs_df$word[1:10]  # Adjust the number of popular hashtags as needed

# Create a new variable indicating the presence of popular hashtags
merged_data$has_popular_hashtag <- sapply(merged_data$hashtags, function(x) any(popular_hashtags %in% unlist(strsplit(gsub("#", "", x), " "))))

# Convert has_popular_hashtag to a binary factor
merged_data$has_popular_hashtag <- factor(ifelse(merged_data$has_popular_hashtag, "Yes", "No"))

# Perform the t-test
t.test(Likes ~ has_popular_hashtag, data = merged_data)

table(merged_data$has_popular_hashtag)

# Step 4 (Optional): Build a regression model
model <- lm(Likes ~ has_popular_hashtag + Total_Likes + Comments + Shares + Saves + Following + Followers + Length_seconds, data = merged_data)
summary(model)

# Assuming you have the word_freqs_df data frame with columns: word, freq, avg_likes

word_freqs_df$popularity <- cut(rank(word_freqs_df$avg_likes, ties.method = "random"),
                                breaks = quantile(rank(word_freqs_df$avg_likes, ties.method = "random"), probs = seq(0, 1, 0.33)),
                                labels = c("Low", "Medium", "High"), include.lowest = TRUE)

# Define a color palette for each popularity tier
color_palette <- c("Low" = "#488C3F", "Medium" = "#375A80", "High" = "#B14044")

# Create a legend data frame
legend_data <- data.frame(
  Popularity = factor(names(color_palette), levels = c("Low", "Medium", "High")),
  Color = color_palette
)

# Create the word cloud with color-coded hashtags
wordcloud(words = word_freqs_df$word, freq = word_freqs_df$freq,
          colors = color_palette[word_freqs_df$popularity],
          min.freq = 1, max.words = 100, random.order = FALSE,
          scale = c(10,0.85))

# Add a legend to the word cloud
legend(
  x = "topright",
  legend = levels(legend_data$Popularity),
  fill = legend_data$Color,
  title = "Popularity",
  cex = 0.8,  # Adjust the size of the legend
  pt.cex = 1.5  # Adjust the size of the legend symbols
)

# Add a caption to the word cloud
caption_text <- "Figure 3. Color-coded word cloud of popular hashtags based on their average likes."
mtext(caption_text, side = 1, line = 3, adj = 0.5, cex = 0.8)  # Adjust cex to change the caption size

# Save the color-coded word cloud with legend as a high-resolution TIFF image
png("color_coded_wordcloud_with_legend.png", width = 3500, height = 1800, res = 300)
wordcloud(words = word_freqs_df$word, freq = word_freqs_df$freq,
          colors = color_palette[word_freqs_df$popularity],
          min.freq = 1, max.words = 100, random.order = FALSE,
          scale = c(10,0.85))
legend(
  x = "topright",
  legend = levels(legend_data$Popularity),
  fill = legend_data$Color,
  title = "Popularity",
  cex = 0.8,
  pt.cex = 1.5
)
mtext(caption_text, side = 1, line = 3, adj = 0.5, cex = 0.8)
dev.off()
img <- image_read("color_coded_wordcloud_with_legend.png")
image_write(img, path = "color_coded_wordcloud_with_legend.tiff", format = "tiff", density = 300)

