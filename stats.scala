
val rootDir = "/home/ubuntu/"

val blogs = sc.textFile("file://" + rootDir + "final/en_US/en_US.blogs.txt")
val news = sc.textFile("file://" + rootDir + "final/en_US/en_US.news.txt")
val twitter = sc.textFile("file://" + rootDir + "final/en_US/en_US.twitter.txt")

val text = blogs.union(news).union(twitter)

// lowercase only and replace all non-word characters with space
val cleansed = text.map(sentence => sentence.replaceAll("\\W+", " ").toLowerCase)


val cleansedBlogs = blogs.map(sentence => sentence.replaceAll("\\W+", " ").toLowerCase)
val cleansedNews = news.map(sentence => sentence.replaceAll("\\W+", " ").toLowerCase)
val cleansedTweets = twitter.map(sentence => sentence.replaceAll("\\W+", " ").toLowerCase)

// -----------
// -----------
// -----------

def stats(rdd: org.apache.spark.rdd.RDD[String]) = {

/*
name = c("twitter", "blogs", "news")
num_rows = ,
word_count = ,
length_in_chars =,
longest_line_length =,
top_three_words,
bottom_three_words
*/

val numrows = rdd.count

val words = rdd.flatMap(sentence => sentence.split(" "))
val distinctWords = words.distinct.count
val totalWordCount = words.count
val totalChars = rdd.map(sentence => sentence.length).sum
val longestLineLen = rdd.map(sentence => sentence.length).max

val frequentWords = words.countByValue.toArray

val top = frequentWords.sortBy(-_._2).take(3).mkString(", ")
val bottom = frequentWords.sortBy(_._2).take(3).mkString(", ")

(s"num_rows = $numrows,\n" +
s"word_count = $totalWordCount,\n" +
s"distinct_words = $distinctWords,\n" +
s"length_in_chars = $totalChars,\n" +
s"longest_line_length = $longestLineLen,\n" +
s"top_three_words = $top,\n" +
s"bottom_three_words = $bottom")
}

// -----------
// -----------
// -----------

val twitterStats = stats(cleansedTweets)
val blogStats = stats(cleansedBlogs)
val newsStats = stats(cleansedNews)

println("Twitter:\n\n" + twitterStats + "\n\n======\n\n");
println("Blogs:\n\n" + blogStats + "\n\n======\n\n");
println("News:\n\n" + newsStats + "\n\n======\n\n");

// -----------
// -----------
// -----------

import java.io.PrintWriter

def word_freqs(rdd: org.apache.spark.rdd.RDD[String], filename: String) = {

    val words = rdd.flatMap(sentence => sentence.split(" "))
    val frequentWords = words.countByValue

    val writer = new PrintWriter(filename)

    frequentWords.foreach(entry => {
        writer.println(entry._1 + "\t" + entry._2)
    })

    writer.flush
    writer.close
}

word_freqs(cleansedTweets, "frequent_words_twitter")
word_freqs(cleansedBlogs, "frequent_words_blogs")
word_freqs(cleansedNews, "frequent_words_news")


