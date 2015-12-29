
library(plyr)

lang <- "en_US"
directory = paste("final/", lang, "/", sep = "")
rootfilename <- paste(directory, lang, sep = "")

read_df <- function(file) {
  df <- read.delim(paste(rootfilename, ".", file, ".txt", sep = ""), sep = "\t")
  names(df) = c("line")
  df
}

blogs <- read_df("blogs")
news <- read_df("news")
twitter <- read_df("twitter")

# longest line

add_length <- function(frame) {
  adply(frame, 1, fun = function(row) nchar(row$line))
}

blogs_length = add_length(blogs)
head(blogs_length)
