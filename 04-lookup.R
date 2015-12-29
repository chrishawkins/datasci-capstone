
lookup <- read.table("lookup.txt", header = F, sep = "\t")
names(lookup) <- c("seed", "prediction", "probability")

predict <- function(str, N = 3) {

	cleansed_words <- unlist(strsplit(tolower(gsub("\\W+", " ", str)), " "))
	word_count = length(cleansed_words)

	if (word_count > N)
	{
		cleansed_words <- cleansed_words[(word_count - N + 1):word_count]
	}

	rows <- lookup[lookup$seed == paste(cleansed_words, collapse = " "), 2:3]

	if (nrow(rows) == 0) {
		if (N == 1) {
			return(c())
		} else {
			return(predict(str, N - 1))
		}
	}

	return(as.vector(rows))
}
