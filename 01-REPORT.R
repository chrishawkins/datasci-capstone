
#download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "Coursera-SwiftKey.zip")
#unzip("Coursera-SwiftKey.zip")

lang <- "en_US"
directory = paste("final/", lang, "/", sep = "")
rootfilename <- paste("final/", lang, "/", lang, sep = "")

# load tweets
twitter <- read.delim(paste(rootfilename, ".twitter.txt", sep = ""), header = F, sep = "\t", colClasses = c(character()))
# load blogs
blogs <- read.delim(paste(rootfilename, ".blogs.txt", sep = ""), header = F, sep = "\t", colClasses = c(character()))
# load news
news <- read.delim(paste(rootfilename, ".news.txt", sep = ""), header = F, sep = "\t", colClasses = c(character()))

#----------------------------

cleanse_normalize <- function(sentence) {
	lower_with_spaces <- tolower(gsub("\\W+", " ", sentence))
	trim_left <- gsub("^\\W+", "", lower_with_spaces)
	trim_right <- gsub("\\W+$", "", trim_left)
	return (trim_right)
}

cleansed_twitter <- data.frame(sentence = apply(twitter, 1, FUN=cleanse_normalize))
cleansed_blogs <- data.frame(sentence = apply(blogs, 1, FUN=cleanse_normalize))
cleansed_news <- data.frame(sentence = apply(news, 1, FUN=cleanse_normalize))

#----------------------------

word_count <- function(sentence) {
	no_spaces <- gsub(" ", "", sentence)
    return (nchar(sentence) - nchar(no_spaces))
}

word_freq <- function(sentence) {
	df <- as.data.frame(table(strsplit(sentence, " ")))
	colnames(df) <- c("word", "freq")
	return(df)
}

summary_stats <- function(name, text) {
	word_counts <- apply(text, 1, FUN=word_count)
	char_counts <- apply(text, 1, FUN=nchar)
	word_freqs <- apply(text, 1, FUN=word_freq)

	rows <- nrow(text)

	all_freqs <- do.call("rbind", word_freqs)
	total_freqs <- aggregate(all_freqs$freq, by=list(word=all_freqs$word), FUN=sum)

	top3 <- total_freqs[order(-total_freqs$x),][1:3, c("word")]
	bottom3 <- total_freqs[order(total_freqs$x),][1:3, c("word")]

	top3_str <- paste(top3, collapse=", ")
	bottom3_str <- paste(bottom3, collapse=", ")

	total_words = sum(word_counts)
	total_chars = sum(char_counts)
	longest_line_length = max(char_counts)
	distinct_words = nrow(total_freqs)

	return(data.frame(	name = name,
						num_rows = rows,
					  	word_count = total_words,
					  	distinct_words = distinct_words,
					  	length_in_chars = total_chars,
					  	longest_line_length = longest_line_length,
					  	top_three_words = top3_str,
					  	bottom_three_words = bottom3_str))
}

twitter_stats <- summary_stats("twitter", cleansed_twitter)
blogs_stats <- summary_stats("blogs", cleansed_blogs)
news_stats <- summary_stats("news", cleansed_news)

all_stats = rbind(twitter_stats, blogs_stats, news_stats)

#----------------------------
## Produce N-grams
#----------------------------

N <- 3

all_text <- rbind(cleansed_twitter, cleansed_blogs, cleansed_news)

ngrams <- data.frame(ngram = character())

for (row in 1:nrow(text))
{
	words <- cleanse_split(text[row,1])
	new_ngrams <- data.frame(ngram = character())

	# all words are n-grams
	#new_ngrams <- rbind(new_ngrams, data.frame(ngram = words))

	if (row %% 1000 == 0) print(paste(row, "/", nrow(text)))

	# bigrams and up
	for (n in 2:(N+1))
	{
		if (n > length(words)) break

		for (i in 1:(length(words) - (n - 1)))
		{
			ngram <- words[i:(i + n - 1)]
			new_ngrams = rbind(new_ngrams, data.frame(ngram = paste(ngram, collapse=" ")))
		}
	}

	ngrams <- rbind(ngrams, new_ngrams)
}

write.table(ngrams, file = "ngrams.txt", col.names = F, row.names = F, quote = F)

