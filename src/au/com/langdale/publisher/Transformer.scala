package au.com.langdale.publisher

import java.io.{File}
import scala.xml.{XML, Node, Elem, TopScope, NodeSeq}
import scala.xml.NodeSeq.Empty
import Util._

trait Transformer {

  def fixLink(href: String): (String, String)  

  def extract(x: Node) = (extractTitle(x), extractContent(x))
  
  private def extractTitle(x: Node) = 
    findChild(x, "h1").text.trim
  
  private def extractContent(x: Node): NodeSeq = 
    findChild(x, "div", "id" -> "content") flatMap transform

  private val transform: Rule = {
    case e @ <div>{child@_*}</div> if e has "id" -> "searchable" =>
      child flatMap transform
    
    case e @ <a>{child@_*}</a> if e has "href" =>
      val (c, h) = fixLink(e.attributes("href").text)
      <a class={c} href={h}>{child flatMap transform}</a>
      
    case <span/> => Empty
      
    case <tt>{child@_*}</tt> => <code>{child flatMap transform}</code>  
    
    case <h1>{_*}</h1> => Empty
    
    case e @ <img>{child@_*}</img> if e has "src" =>
      val (c, h) = fixLink(e.attributes("src").text)      
      Elem( null, "img", e.attributes append "src" -> h, TopScope, (child flatMap transform): _*)
    
    case Elem( _, label, atts, _, child@_*) =>
      Elem( null, label, atts, TopScope, (child flatMap transform): _*)
    
    case n => n  
  }
}
