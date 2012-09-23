package au.com.langdale.publisher

import java.util.Date
import java.io.File
import RSS.{Item, Feed}
import scala.xml.{XML, NodeSeq, Node}
import Util._

/**
 * Add RSS and HTML for a basic blog to the site.
 */
trait Blog extends Publisher {
  
  val distrib: File
  val nonLocalPrefix: String
  val blogTitle: String
  val blogDescription: String
  def history: Seq[Item]
  def fixLink( url: String): (String, String)
  
  val blogPage = "blog.html"
  val blogRSS = "blog.rss"
  
  def expand(title: String, content: NodeSeq, feeds: Seq[Feed]): Node
  
  override def scan {
    super.scan
    pageNames += blogPage
  }
  
  override def publish {
    super.publish 
    
    val feeds = List(Feed(blogTitle, nonLocalPrefix + blogRSS))
    saveXHTML( expand(blogTitle, content(history), feeds), distrib / blogPage)

    object channel extends Item {
      val url = nonLocalPrefix + blogPage;
      val title = blogTitle;
      val description = blogDescription;
      val date = new Date
    }
    
    XML.save((distrib / blogRSS).getPath, RSS(channel, history), "utf-8")
  }
  
  private def content(items: Seq[Item]) = 
    for( i <- items; (style, url) = fixLink(i.url)) yield
      <div class="blog-item">
        <h2>{i.title}</h2>
        <p>{i.description}</p>
        <p><span class="datetime">{i.date}</span> <a class={style} href={url}>more...</a></p>
      </div>
}
