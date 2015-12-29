val blogs = sc.textFile("file:///home/ubuntu/final/en_US/en_US.blogs.txt").cache
val news = sc.textFile("file:///home/ubuntu/final/en_US/en_US.news.txt").cache
val twitter = sc.textFile("file:///home/ubuntu/final/en_US/en_US.twitter.txt").cache

// Question 3

val longestBlog = blogs.max()(new Ordering[String]() {
  override def compare(x: String, y: String): Int = 
      Ordering[Int].compare(x.length, y.length)
})

val longestNews = news.max()(new Ordering[String]() {
  override def compare(x: String, y: String): Int = 
      Ordering[Int].compare(x.length, y.length)
})

val longestBlogLength = longestBlog.length
val longestNewsLength = longestNews.length

if (longestBlogLength > longestNewsLength) {
	println(s"Question 3: Blog, $longestBlogLength")
} else { 
	println(s"Question 3: News, $longestNewsLength")
}

// Question 4

val loveTwitter = twitter.filter(line => line.contains("love")).count
val hateTwitter = twitter.filter(line => line.contains("hate")).count
val division = loveTwitter.toDouble / hateTwitter.toDouble

println(s"Question 4: $loveTwitter / $hateTwitter = $division")

// Question 5

twitter.filter(line => line.contains("biostats")).collect.foreach(tweet => println("Question 5: " + tweet))

// Question 6

val count = twitter.filter(line => line.contains("A computer once beat me at chess, but it was no match for me at kickboxing")).count
println("Question 6: " + count)