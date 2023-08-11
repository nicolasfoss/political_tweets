
# Load the libraries
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)

# Read in the data
tweets <- read_csv("datasets/trump_tweets.csv", guess_max = 36000) %>%
  filter(created_at >= "2015-06-01" , created_at <= "2016-11-08")

# Inspect the first six rows
head(tweets)

# These packages need to be loaded in the first @tests cell. 
library(testthat) 
library(IRkernel.testthat)

soln_tweets  <- read_csv("datasets/trump_tweets.csv", guess_max = 36000) %>%
  filter(created_at >= "2015-06-01", created_at <= "2016-11-08")

run_tests({
    test_that("packages are loaded", {
        expect_true("lubridate" %in% .packages(), info = "Did you load the lubridate package?")
    })
    test_that("The .csv was read in correctly", {
        expect_is(tweets, "tbl_df", info = "Did you read in bottom_line with read_csv?")
        expect_equal(tweets, soln_tweets, info = "tweets contains the wrong values. Did you load the correct .csv file?")
        expect_equal(nrow(tweets),nrow(soln_tweets), info = "tweets has an incorrect nubmer of rows. 
                                                    Did you filter for the correct time frame?" )
    })
})

# Count the nubmer of tweets by source
tweets %>% count(source)

# Clean the tweets
cleaned_tweets <- tweets %>%
  select(id_str, source, text, created_at) %>% 
  filter(source %in% c("Twitter for Android", "Twitter for iPhone")) %>%
  extract(source, "source", "(\\w+)$")

# Inspect the first six rows
head(cleaned_tweets)

soln_cleaned_tweets <- soln_tweets %>%
  select(id_str, source, text, created_at) %>%
  filter(source %in% c("Twitter for iPhone", "Twitter for Android")) %>%
  extract(source, "source", "(\\w+)$")

run_tests({
    #Thinking about order of columns and how to test for them out of order. So far, only figured out with sort.
    # expect_equivalent does not work.
    test_that("cleaned_tweets is not correct", {
        expect_true(identical(cleaned_tweets[order(colnames(cleaned_tweets))], 
                              soln_cleaned_tweets[order(colnames(soln_cleaned_tweets))]), 
        info = "The column names are not correct. Either the correct columns were not selected or the wrong argument was given to 'extract()'?")
    })
    test_that("filter() was used correctly", {
        expect_equal(cleaned_tweets$source, soln_cleaned_tweets$source, 
                     info = "`source` is not correct. Did you use the correct column names? Make sure they are surrounded by quotation marks.")
    })
}) 

# Load the packages
library(ggplot2)

# Plot the percentage of tweets by hour of the day for each device
cleaned_tweets %>%
  count(source, hour = hour(with_tz(created_at, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%  
  ggplot(aes(hour, percent, color = source)) +
  geom_line() + 
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "")

stud_p <- last_plot()

soln_plot_time  <- soln_cleaned_tweets  %>% 
  count(source, hour = hour(with_tz(created_at, "EST"))) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(hour, percent, color = source)) +
  geom_line() +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Hour of day (EST)", y = "% of tweets", color = "")

run_tests({
    test_that("Data used in the plot are correct", {
        expect_identical(stud_p$data, soln_plot_time$data, info = "The data used to create the plot are not correct. Did you count the sources and use `mutate()` to add a column of percents? 
                                Refer to the hint if you are unsure about how to proceed.")
        })
    test_that("Plot aesthetics are correct.", {
        expect_identical(deparse(stud_p$mapping$x),deparse(soln_plot_time$mapping$x),
                         info = 'The `x` aesthetic is incorrect. Did you map it to `hour`?')
        expect_identical(deparse(stud_p$mapping$y),deparse(soln_plot_time$mapping$y),
                         info = "The `y` aesthetic is incorrect. Did you map it to `percent`?")
    })
    test_that("Plot labels and legend are correct.", {
        expect_identical(stud_p$labels$x, soln_plot_time$labels$x, info="The x label is not correct.")
        expect_identical(stud_p$labels$y, soln_plot_time$labels$y, info="The y label is not correct.")
        expect_identical(stud_p$labels$colour, soln_plot_time$labels$colour, info="The empty legend string is not correct. Did you use empty quotation marks?")
    })
})

# Load stringr
library(stringr)

# Plot the number of tweets with and without quotes by device
cleaned_tweets %>%
  count(source,
        quoted = ifelse(str_detect(text, '^"'), "Quoted", "Not quoted")) %>%
  ggplot(aes(source, n, fill = quoted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "") +
  ggtitle('Whether tweets start with a quotation mark (")')

stud_p <- last_plot()

soln_plot_quote <- soln_cleaned_tweets %>%
  count(source,
        quoted = ifelse(str_detect(text, '^"'), "Quoted", "Not quoted")) %>%
  ggplot(aes(source, n, fill = quoted)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "") +
  ggtitle('Whether tweets start with a quotation mark (")')


run_tests({
    test_that("Data used in the plot are correct", {
        expect_identical(stud_p$data, soln_plot_quote$data, info = "The data used to create the plot are not correct. Did you count the sources and create \"Quoted\" and \"Not quoted\" data? (Capitalization matters) 
                                Refer to the hint if you are unsure about how to proceed.")
    })
    test_that("Plot aesthetics are correct.", {
        expect_identical(deparse(stud_p$mapping$x),deparse(soln_plot_quote$mapping$x),
                         info = 'The `x` aesthetic is incorrect. Did you map it to `source`?')      
        expect_identical(deparse(stud_p$mapping$y),deparse(soln_plot_quote$mapping$y),
                         info = "The `y` aesthetic is incorrect. Did you map it to `n`?")
        expect_identical(deparse(stud_p$mapping$fill),deparse(soln_plot_quote$mapping$fill),
                         info = "The `fill` aestheitc is incorrect. Did you map it to `quoted`?")
    })
    test_that("geom_bar() is correct", {
        expect_equal(stud_p$layers[[1]], soln_plot_quote$layers[[1]],
        info = "The parameters in `geom_bar()` are not correct. Check the values for `identity=`, and `position=`?")
        })
})

# Count the number of tweets with and without picture/links by device
tweet_picture_counts <- cleaned_tweets %>%
  filter(!str_detect(text, '^"')) %>%
  count(source,
        picture = ifelse(str_detect(text, "t.co"),
                         "Picture/link", "No picture/link"))

# Make a bar plot 
ggplot(tweet_picture_counts, aes(source, n, fill = picture)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "")

stud_p  <- last_plot()

soln_tweet_picture_counts <- soln_cleaned_tweets %>%
  filter(!str_detect(text, '^"')) %>%
  count(source,
        picture = ifelse(str_detect(text, "t.co"),
                         "Picture/link", "No picture/link"))

soln_plot_pic  <- ggplot(soln_tweet_picture_counts, aes(source, n, fill = picture)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "", y = "Number of tweets", fill = "")

run_tests({
    test_that("tweet_picture_counts is correct", {
        expect_identical(tweet_picture_counts, soln_tweet_picture_counts, 
        info = "`tweet_picutre_counts` is not correct. Did you cound the sources correctly? Did you use str_detect()?")
    })
    test_that("Plot aesthetics are correct.", {
        expect_identical(deparse(stud_p$mapping$x),deparse(soln_plot_pic$mapping$x),
                         info = 'The `x` aesthetic is incorrect. Did you map it to `source`?')      
        expect_identical(deparse(stud_p$mapping$y),deparse(soln_plot_pic$mapping$y),
                         info = "The `y` aesthetic is incorrect. Did you map it to `n`?")
        expect_identical(deparse(stud_p$mapping$fill),deparse(soln_plot_pic$mapping$fill),
                         info = "The `fill` aestheitc is incorrect. Did you map it to `quoted`?")
    })
    test_that("geom_bar() is correct", {
        expect_equal(stud_p$layers[[1]], soln_plot_pic$layers[[1]],
        info = "The parameters in `geom_bar()` are not correct. Check the values for `identity=`, and `position=`?")
    })
})

# Load the tidytext package
library(tidytext)

# Create a regex pattern
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

# Unnest the text strings into a data frame of words
tweet_words <- cleaned_tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

# Inspect the first six rows of tweet_words
head(tweet_words)

soln_tweet_words <- soln_cleaned_tweets %>%
  filter(!str_detect(text, '^"')) %>%
  mutate(text = str_replace_all(text, "https://t.co/[A-Za-z\\d]+|&amp;", "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))

run_tests({
    test_that("packages are loaded", {
        expect_true("tidytext" %in% .packages(), info = "Did you load the tidyverse package?")
    })
    test_that("tweet_words is correct", {
        expect_equal(nrow(tweet_words), nrow(soln_tweet_words), 
        info = "`tweet_words` does not have the correct number of rows. Check the calls to `filter()` ahd `unnest_tokens()` Did you use stop_words$word correctly?.") 
    })
})

# Plot the most common words from @realDonaldTrump tweets
tweet_words %>%
  count(word, sort = TRUE) %>%
  head(n = 20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()

stud_p <- last_plot()

soln_plot_tweet_words  <- soln_tweet_words %>%
  count(word, sort = TRUE) %>%
  head(20) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_bar(stat = "identity") +
  ylab("Occurrences") +
  coord_flip()

run_tests({
    test_that("Data used in the plot are correct", {
        expect_identical(stud_p$data, soln_plot_tweet_words$data, info = "The data used to create the plot are not correct. Did you count `words` and reorder them correctly? 
                                Refer to the hint if you are unsure about how to proceed.")
    })
   test_that("Plot aesthetics are correct.", {
        expect_identical(deparse(stud_p$mapping$x),deparse(soln_plot_tweet_words$mapping$x),
                         info = 'The `x` aesthetic is incorrect. Did you map it to `source`?')      
        expect_identical(deparse(stud_p$mapping$y),deparse(soln_plot_tweet_words$mapping$y),
                         info = "The `y` aesthetic is incorrect. Did you map it to `n`?")
   })
    test_that("geom_bar() is correct", {
        expect_equal(stud_p$layers[[1]], soln_plot_tweet_words$layers[[1]],
        info = "The parameters in `geom_bar()` are not correct. Check the values for `identity=`, and `position=`?")
        })
    test_that("coordinates were flipped", {
        expect_equal(stud_p$coordinates$default, soln_plot_tweet_words$coordinates$default, info="The coordinates were not flipped.")
    })
})

# Create the log odds ratio of each word
android_iphone_ratios <- tweet_words %>%
  count(word, source) %>%
  group_by(word)  %>% 
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log2(Android / iPhone)) %>%
  arrange(desc(logratio))

# Inspect the first six rows
head(android_iphone_ratios)

soln_android_iphone_ratios <- soln_tweet_words %>%
  count(word, source) %>%
  group_by(word)  %>% 
  filter(sum(n) >= 5) %>%
  spread(source, n, fill = 0) %>%
  ungroup() %>%
  mutate_if(is.numeric, ~((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log2(Android / iPhone)) %>%
  arrange(desc(logratio))

run_tests({
    test_that("the answer is correct", {
    expect_identical(soln_android_iphone_ratios, android_iphone_ratios, 
        info = "android_iphone_ratios is not correct. Did you divide Android by iPhone? Call sum(n) in `filter()`? 
                How about arrange in descending value according to `logratio`?")
    })
    
})

# Plot the log odds ratio for each word by device
android_iphone_ratios %>%
  group_by(logratio > 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))

stud_p <- last_plot()

soln_plot_LOR   <- soln_android_iphone_ratios %>%
  group_by(logratio > 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  ylab("Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))


run_tests({
    test_that("Data used in the plot are correct", {
        expect_identical(stud_p$data, soln_plot_LOR$data, info = "The data used to create the plot are not correct.
        Did you group by the logratio greatet than 0 and take the top 15 of abs(logratio)?")
    })
   test_that("Plot aesthetics are correct.", {
        expect_identical(deparse(stud_p$mapping$x),deparse(soln_plot_LOR$mapping$x),
                         info = 'The `x` aesthetic is incorrect. Did you map it to `word`?')      
        expect_identical(deparse(stud_p$mapping$y),deparse(soln_plot_LOR$mapping$y),
                         info = "The `y` aesthetic is incorrect. Did you map it to `logratio`?")
   })
    test_that("geom_bar() is correct", {
        expect_equal(stud_p$layers[[1]], soln_plot_LOR$layers[[1]],
        info = "The parameters in `geom_bar()` are not correct. Check the values for `identity=`.")
        })
    })

# Create a sentiment data frame from the NRC lexicon
nrc <- read_rds("datasets/nrc.rds")

# Join the NRC lexicon to log odds ratio data frame
android_iphone_sentiment <- android_iphone_ratios %>%
  inner_join(nrc, by = "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate(sentiment = reorder(sentiment, -logratio),
         word = reorder(word, -logratio)) %>%
  group_by(sentiment) %>%
  top_n(10, abs(logratio)) %>%
  ungroup()

# Inspect the first six rows
head(android_iphone_sentiment)

soln_nrc <- read_rds("datasets/nrc.rds")

soln_android_iphone_sentiment <- soln_android_iphone_ratios %>%
  inner_join(soln_nrc, by = "word") %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate(sentiment = reorder(sentiment, -logratio),
         word = reorder(word, -logratio)) %>%
  group_by(sentiment) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() 


run_tests({
    test_that("the answer is correct", {
    expect_equal(soln_android_iphone_sentiment, android_iphone_sentiment, 
        info = "android_iphone_sentiment is not correct. Check the type of join.")
    })
})

# Plot the log odds ratio of words by device in groups sentiments
ggplot(android_iphone_sentiment, aes(word, logratio, fill = logratio < 0)) +
  facet_wrap(~ sentiment, scales = "free", nrow = 2) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))

stud_p  <-  last_plot()

soln_plot_sentiment  <- ggplot(soln_android_iphone_sentiment, aes(word, logratio, fill = logratio < 0)) +
  facet_wrap(~ sentiment, scales = "free", nrow = 2) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "", y = "Android / iPhone log ratio") +
  scale_fill_manual(name = "", labels = c("Android", "iPhone"),
                    values = c("red", "lightblue"))
run_tests({
    test_that("Data used in the plot are correct", {
        expect_identical(stud_p$data, soln_plot_sentiment$data, info = "The data used to create the plot are not correct. Check `android_iphone_sentiment`.")
    })
    test_that("Plot aesthetics are correct.", {
        expect_identical(deparse(stud_p$mapping$x),deparse(soln_plot_sentiment$mapping$x),
                         info = 'The `x` aesthetic is incorrect. Did you map it to `word`?')
        expect_identical(deparse(stud_p$mapping$y),deparse(soln_plot_sentiment$mapping$y),
                         info = "The `y` aesthetic is incorrect. Did you map it to `logratio`?")
    })
    test_that("geom_bar() is correct", {
        expect_equal(stud_p$layers[[1]], soln_plot_sentiment$layers[[1]],
                     info = "The parameters in `geom_bar()` are not correct. Check the values for `identity=`.")
    })
    test_that("the use of `facet()` is correct", {
        expect_identical(deparse(stud_p$facet$params$facets) , deparse(soln_plot_sentiment$facet$params$facet), 
                         info = "The variable used the facet the plot is incorrect.")
        expect_identical(stud_p$facet$params$nrow, soln_plot_sentiment$facet$params$nrow,
                         info = "The number of rows in the call to `facet()` is incorrect.")
    })
})

anonymous_iPhone_tweeter <- "Cog"

run_tests({
    test_that("an answer was put in", {
        expect_true(is.character(anonymous_iPhone_tweeter), info = "Did you make a choice? What do you think (in characters)?")
    })
})
