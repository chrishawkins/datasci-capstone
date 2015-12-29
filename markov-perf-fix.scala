
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

val blogFrequentWords = cleansedBlogs.flatMap(sentence => sentence.split(" ")).countByValue
val newsFrequentWords = cleansedNews.flatMap(sentence => sentence.split(" ")).countByValue
val tweetFrequentWords = cleansedTweets.flatMap(sentence => sentence.split(" ")).countByValue

// -------------------
// EXTRACTS ALL NGRAMS
// -------------------

val n = 3

val allNgrams = cleansed.flatMap(sentence => {
	(2 to n + 1).flatMap(length => {
		sentence.split(" ").sliding(length).map(ngram => ngram.mkString(" "))
	})
})

allNgrams.saveAsTextFile("file://" + rootDir + "ngram_cache")


// -------------------------------
// REMOVE ITEMS OCCURING ONLY ONCE
// -------------------------------

import scala.sys.process._

"cat " + rootDir + "ngram_cache/part-* > " + rootDir + "ngram_cache.txt".!
"sort --parallel=2 " + rootDir + "ngram_cache.txt > " + rootDir + "sorted_ngram_cache.txt".!

import scala.io.Source
import java.io.PrintWriter

// note that a final line is added to trigger the final process (this line is automatically culled)
val lines = Source.fromFile(rootDir + "sorted_ngram_cache.txt").getLines() ++ " "
//val lines = Source.fromFile("test.txt").getLines() ++ " "

var previousLine = lines.next
var currentCount = 1
var removedCount = 0
val threshold = 2

var i = 0

val writer = new PrintWriter("frequent_ngrams.txt")

lines.foreach(line => {

	if (line == previousLine) {
		currentCount = currentCount + 1
	} else {

		if (currentCount >= threshold) {
			for (i <- 0 until currentCount) {
				writer.println(previousLine)
			}
		} else {
			removedCount = removedCount + 1
		}

		currentCount = 1
	}

	if (i % 1000 == 0) {
		println("Up to line " + i)
	}

	i = i + 1
	previousLine = line
})

println("Total N-Grams Pruned = " + removedCount)

writer.flush
writer.close


// -------------------------------
// DEVELOPS PREDICTIONS FOR n >= 1
// -------------------------------

val allNgrams = sc.textFile("file://" + rootDir + "frequent_ngrams.txt")

val frequencies = allNgrams.map(ngram => {

	val words = ngram.split(" ")
	((words.take(words.length - 1).mkString(" "), words(words.length - 1)), 1)

}).reduceByKey(_ + _)

val denominators = allNgrams.map(ngram => {

	ngram.split(" ")
	val words = ngram.split(" ")
	words.take(words.length - 1).mkString(" ")

}).countByValue

val denominatorsBc = sc.broadcast(denominators)

val topFollowingNGrams = frequencies.groupBy(tuple => tuple._1._1).map(tuple => {

	val possibilities = tuple._2.toArray
	val denominator = denominatorsBc.value(tuple._1)

	val sourceNgram = tuple._1
	//val highestTuple = tuple._2.maxBy(x => x._2) // [((String, String), int)]
	val highest3 = possibilities.sortBy(x => x._2).takeRight(3).map(x => (x._1._2, x._2.toDouble / denominator))

	(sourceNgram, highest3)

}).collectAsMap


// -----------
// COMPRESSION (note that this was totally ineffective :( )
// -----------

val prune = topFollowingNGrams.map(tuple => tuple._1).map(key => {

	val words = key.split(" ")

	if (words.length == 1) (key, false) // do not prune
	else {
		val comparisonTerm = words.takeRight(words.length - 1).mkString(" ")
		if (topFollowingNGrams.getOrElse(comparisonTerm, "") == topFollowingNGrams(key)) (key, true) // PRUNE
		else (key, false) // do not prune
	}

}).filter(x => x._2).toSet

val topFollowingNGrams.filter(x => !prune.contains(x))


// ---------
// WRITE OUT
// ---------

val writer = new PrintWriter("lookup.txt")

topFollowingNGrams.foreach(entry => {
	entry._2.foreach(x => {
		writer.println(entry._1 + "\t" + x._1 + "\t" + x._2)
	})
})

writer.flush
writer.close

