
lang <- "en_US"
directory = paste("final/", lang, "/", sep = "")
rootfilename <- paste("final/", lang, "/", lang, sep = "")

twitter <- read.delim(paste(rootfilename, ".twitter.txt", sep = ""), header = F, sep = "\t", colClasses = c(character()))
print("Read tweets")
blogs <- read.delim(paste(rootfilename, ".blogs.txt", sep = ""), header = F, sep = "\t", colClasses = c(character()))
print("Read blogs")
news <- read.delim(paste(rootfilename, ".news.txt", sep = ""), header = F, sep = "\t", colClasses = c(character()))
print("Read news")

print("Loaded files")

text <- rbind(blogs, news, twitter)
#text <- data.frame(text = c("I love cats a lot", "Cats love me not so much"), stringsAsFactors = F) # sample text

print("Concatenated inputs")

cleanse_split <- function(sentence) {
	sentence <- tolower(gsub("\\W", " ", sentence))
	words <- strsplit(sentence, " ")
	return(unlist(words))
}

cleanse_split(text)

N <- 3

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
