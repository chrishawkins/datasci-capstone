
val blogs = sc.textFile("file:///home/ubuntu/final/en_US/en_US.blogs.txt")
val news = sc.textFile("file:///home/ubuntu/final/en_US/en_US.news.txt")
val twitter = sc.textFile("file:///home/ubuntu/final/en_US/en_US.twitter.txt")

val text = blogs.union(news).union(twitter)
//val text = twitter

 // lowercase only and replace all non-word characters with space
val cleansed = text.map(sentence => sentence.replaceAll("\\W+", " ").toLowerCase)

// ------------------------------
// DEVELOPS PREDICTIONS FOR n = 1
// ------------------------------

val uniqueWords = cleansed.flatMap(sentence => sentence.split(" ")).distinct.collect
val wordsWithIndices = uniqueWords.zipWithIndex.toMap

val occurrences = cleansed.flatMap(sentence => sentence.split(" ")).map(word => (word, 1)).reduceByKey(_ + _).collect

val frequencies = cleansed.flatMap(sentence => {

	val words = sentence.split(" ").toArray

	for (i <- 0 to words.length - 2) yield {
		val currentWord = words(i)
		val nextWord = words(i + 1)
		((currentWord, nextWord), 1)
	}

}).reduceByKey(_ + _)

val topFollowingWords = frequencies.groupBy(tuple => tuple._1._1).map(tuple => {

	val sourceWord = tuple._1
	val highestTuple = tuple._2.maxBy(x => x._2) // [((String, String), int)]

	(sourceWord, highestTuple._1._2)
}).collectAsMap

// -------------------------------
// DEVELOPS PREDICTIONS FOR n >= 1
// -------------------------------

val n = 3

val frequencies = cleansed.flatMap(sentence => {

	val words = sentence.split(" ")
	val ngrams = words.sliding(n + 1).toArray

	(1 to n).flatMap(length => {
		val ngrams = words.sliding(length + 1).toArray

		for (i <- 0 until ngrams.length) yield {
			val currentGram = ngrams(i)
			((currentGram.take(length).mkString(" "), currentGram.takeRight(1)(0)), 1)
		}
	})

}).reduceByKey(_ + _, numPartitions = 100)

val topFollowingNGrams = frequencies.groupBy(tuple => tuple._1._1).map(tuple => {

	val sourceWord = tuple._1
	//val highestTuple = tuple._2.maxBy(x => x._2) // [((String, String), int)]
	val highest3 = tuple._2.toArray.sortBy(x => x._2).takeRight(3).map(x => x._1._2)

	(sourceWord, highest3)
}).collectAsMap


// -----------
// COMPRESSION
// -----------

val prune = topFollowingNGrams.map(tuple => tuple._1).map(key => {

	val words = key.split(" ")

	if (words.length == 1) (key, false) // do not prune
	else {
		println(words.mkString(" "))
		val comparisonTerm = words.takeRight(words.length - 1).mkString(" ")
		if (topFollowingNGrams(comparisonTerm) == topFollowingNGrams(key)) (key, true) // PRUNE
		else (key, false) // do not prune
	}

}).filter(x => x._2).toSet

val topFollowingNGrams.filter(x => !prune.contains(x))


