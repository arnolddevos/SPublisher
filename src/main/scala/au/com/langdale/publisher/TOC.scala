package au.com.langdale.publisher

import java.io.File
import scala.xml.{Node, NodeSeq, XML}
import scala.xml.NodeSeq.Empty
import Util._

/**
 * Adds an eclipse help table of contents document to the site.
 *
 * The TOC is generated from a list of anchors on a given html page.
 * It is sufficient to make the site deployable as eclipse help content.
 */
trait TOC extends Publisher {
  
  val distrib: File
  val toc: File
  val tocWikiWord: String
  val localPrefix: String
  
  override def publish {
    super.publish
    val Some((title, content)) = getContent(tocWikiWord)
    publishTOC( title, localPrefix + tocWikiWord + ".html", content )
  }
  
  def publishTOC( title: String, link: String, x: NodeSeq ) {
    val l = findChild(x, "ul")
    val t = <toc label={title} topic={link}>{l flatMap tocRule}</toc>
    XML.save(toc.getPath, t)
  }
  
  private val tocRule: Rule = {
    case <li>{child@_*}</li> =>
      var href: Option[String] = None
      val text = new StringBuilder
      var subs: NodeSeq = Empty
      
      child foreach {
        case a @ <a>{child@_*}</a> if a has "href" => 
          href = Some(a.attributes("href").text)
          text append child.text
          
        case <ul>{child@_*}</ul> => subs = child 
          
        case other =>  text append other.text
      }
      
      href match {
        case Some(addr) =>
          <topic label={text.toString.trim} href={addr}>{subs flatMap tocRule}</topic>
            
        case None =>
          <topic label={text.toString.trim}>{subs flatMap tocRule}</topic>
      }      
          
    case _ => Empty
  }
}
