
library(tm)

lang <- "en_US"
directory = paste("final/", lang, "/", sep = "")
rootfilename <- paste("final/", lang, "/", lang, sep = "")

#blogs <- read.delim(paste(rootfilename, ".blogs.txt", sep = ""), sep = "\t")
#news <- read.delim(paste(rootfilename, ".news.txt", sep = ""), sep = "\t")
#twitter <- read.delim(paste(rootfilename, ".twitter.txt", sep = ""), sep = "\t")

corpus <- Corpus(DirSource(directory), readerControl = list(language = lang))
