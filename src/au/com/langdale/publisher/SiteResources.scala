package au.com.langdale.publisher

import java.io.File
import scala.xml.{XML, Node, NodeSeq}
import scala.collection.mutable.HashSet

import RSS.Feed
import Util._

trait SiteResources extends Publisher {

  val sources: Seq[File]
  val distrib: File

  def wikiParse(f: File): (String, NodeSeq)
  def extract(x: Node): (String, NodeSeq)
  def expand(title: String, content: NodeSeq, feeds: Seq[Feed]): Node
  
  override def scan {
    super.scan
    for( s <- sources) {
      require(s.isDirectory)
      for(f <- s.listFiles ) {
        if( isPage(f))
          pageNames += stripext(f.getName)
        else if( isImage(f))
          imageNames += f.getName
      }
    }
  }
  
  def isExcluded(f: File) = f.getName.endsWith("~")
  
  def isImage(f: File) = f.isFile && 
    { val n = f.getName; n.endsWith(".png") || n.endsWith(".jpg") || n.endsWith(".gif") }
  
  def isOtherResource(f: File) = f.isFile && 
    { val n = f.getName; n.endsWith(".ico") || n.endsWith(".css") } 
  
  def isCopied(f: File) = f.isDirectory && ! isExcluded(f) || isImage(f) || isOtherResource(f)  
  
  def isPage(f: File) = f.isFile && ! isCopied(f) && ! isExcluded(f)
  
  def isWikiPage(f: File) = f.isFile && f.getName.endsWith(".txt")
  
  override def publish {
    super.publish
    ensureDir(distrib)

    val topNames = new HashSet[String]

    for( s <- sources if s.isDirectory; f <- s.listFiles) {
      if( isPage(f)) {
        val rname = stripext(f.getName) + ".html"
        if(! (topNames contains rname)) {
          if( isWikiPage(f))
            publishWikiPage(f, distrib / rname)
          else
            publishMarkup(f, distrib / rname)
          topNames += rname
        }
      }
      else if(isCopied(f)) { 
        copy( f, distrib / f.getName)
        topNames += f.getName
      }
    }
  }
  
  override def getContent( wikiWord: String ): Option[(String, NodeSeq)] = {
    for( s <- sources if s.isDirectory; f <- s.listFiles; if stripext(f.getName) == wikiWord ) 
      return Some(extract(XML.loadFile(f)))
    super.getContent( wikiWord )
  }
  
  private def publishWikiPage(f: File, g: File) {
    println(f +" ~> " + g)
    try {
      val (title, content) = wikiParse(f)
      val y = expand(title, content, Nil)
      saveXHTML(y, g)
    } 
    catch {
      case e => println(e);
    }  
  }
  
  private def publishMarkup(f: File, g: File) {
    println(f +" ~> " + g)
    try {
      val x = XML.loadFile(f)
      val (title, content) = extract(x)
      val y = expand(title, content, Nil)
      saveXHTML(y, g)
    } 
    catch {
      case e => println(e);
    }  
  }
}
