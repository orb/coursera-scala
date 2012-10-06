package objsets

import common._
import TweetReader._

class Tweet(val user: String, val text: String, val retweets: Int) {

  override def toString: String =
    "User: " + user + "\n" +
    "Text: " + text + " [" + retweets + "]"
}


abstract class TweetSet
{
  def better_filter(p: Tweet => Boolean): TweetSet
  
  def filter(p: Tweet => Boolean): TweetSet = 
    filter0(p, new Empty)
    
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet

  def union(that: TweetSet): TweetSet = 
    if (that.isEmpty) 
      this
    else
      this.incl(that.head).union(that.tail)       

  // Hint: the method "remove" on TweetSet will be very useful.
  def ascendingByRetweet: Trending

  def incl(x: Tweet): TweetSet
  def contains(x: Tweet): Boolean
  def isEmpty: Boolean
  def head: Tweet
  def tail: TweetSet

  /** This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }

  def remove(tw: Tweet): TweetSet

  def findMin0(curr: Tweet): Tweet =
    if (this.isEmpty) curr
    else if (this.head.retweets < curr.retweets) this.tail.findMin0(this.head)
    else this.tail.findMin0(curr)

  def findMin: Tweet =
    this.tail.findMin0(this.head)
}

class Empty extends TweetSet 
{  
  def better_filter(p: Tweet => Boolean) = this
     
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = accu

  def ascendingByRetweet: Trending = new EmptyTrending
  
  
  def contains(x: Tweet): Boolean = false
  def incl(x: Tweet): TweetSet = new NonEmpty(x, new Empty, new Empty)
  def isEmpty = true
  def head = throw new Exception("Empty.head")
  def tail = throw new Exception("Empty.tail")
  def remove(tw: Tweet): TweetSet = this
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {
  def better_filter(p: Tweet => Boolean) = 
    if (p(elem))
        new NonEmpty(elem, left.better_filter(p), right.better_filter(p))
    else
        left.better_filter(p).union(right.better_filter(p))
    
  def filter0(p: Tweet => Boolean, accu: TweetSet): TweetSet = {
    val newaccu = if (p(elem)) accu.incl(elem) else accu    
    right.filter0(p, left.filter0(p, newaccu))
  }

  def ascendingByRetweet: Trending = {
    val min = findMin
    new NonEmptyTrending(min, remove(min).ascendingByRetweet)
  }
  
  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) 
      left.contains(x)
    else if (elem.text < x.text) 
      right.contains(x)
    else 
      true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) 
      new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) 
      new NonEmpty(elem, left, right.incl(x))
    else 
      this
  }

  def isEmpty = false
  def head = if (left.isEmpty) elem else left.head
  def tail = if (left.isEmpty) right else new NonEmpty(elem, left.tail, right)

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) 
      new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) 
      new NonEmpty(elem, left, right.remove(tw))
    else 
      left.union(right)
}


/** This class provides a linear sequence of tweets.
 */
abstract class Trending {
  def + (tw: Tweet): Trending
  def head: Tweet
  def tail: Trending
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit = {
    if (!this.isEmpty) {
      f(this.head)
      this.tail.foreach(f)
    }
  }
}

class EmptyTrending extends Trending {
  def + (tw: Tweet) = new NonEmptyTrending(tw, new EmptyTrending)
  def head: Tweet = throw new Exception
  def tail: Trending = throw new Exception
  def isEmpty: Boolean = true
  override def toString = "EmptyTrending"
}

class NonEmptyTrending(elem: Tweet, next: Trending) extends Trending {
  /** Appends tw to the end of this sequence.
   */
  def + (tw: Tweet): Trending =
    new NonEmptyTrending(elem, next + tw)
  def head: Tweet = elem
  def tail: Trending = next
  def isEmpty: Boolean = false
  override def toString =
    "NonEmptyTrending(" + elem.retweets + ", " + next + ")"
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")

  def matchesKeyword(text:String)(keyword:String) = text.contains(keyword)
  def tweetInList(keywords: List[String])(tweet: Tweet) : Boolean =    
    !keywords.filter(matchesKeyword(tweet.text)).isEmpty
  
  val googleTweets: TweetSet = TweetReader.allTweets.filter(tweetInList(google))
  val appleTweets: TweetSet  = TweetReader.allTweets.filter(tweetInList(apple))
  val combined = googleTweets.union(appleTweets)
  
  val trending: Trending = combined.ascendingByRetweet  
}

object Main extends App {
  // Some help printing the results:
  println("RANKED:")
  GoogleVsApple.trending foreach println
  println("--done")
}
