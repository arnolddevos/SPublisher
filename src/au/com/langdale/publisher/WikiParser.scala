package au.com.langdale.publisher

import java.io.{File}
import scala.xml.{NodeSeq, NodeBuffer, Node, Text}
import Util._

object WikiParser {
  val Header = """^ *(=+) *([^=]+) *=+ *$""".r
  val Bullet = """^( +)[*] *([^ ].*) *$""".r
  val Line = """^( *)([^ ].*) *$""".r
}
import WikiParser._

trait WikiParser {
  def linkWordsAndURIs(text: String): NodeSeq  
  
  def wikiParse( f: File ): (String, NodeSeq) = {
    val lines = f.lines.advance
    
    def slew {
      lines.next
      while( lines.hasNext ) lines.peek match {
        case Line(_, _) => return
        case _ => lines.next
      }  
    }
    
    def collect( lead: String) = {
      val sb = new StringBuilder
      sb append lead
      var more = true
      while( more && lines.hasNext ) {
        lines.peek match {
          case Header(_, _) | Bullet( _, _) => more = false
          case Line(_, text) => lines.next; sb append '\n' append text 
          case _ => slew; more = false
        }
      }
      sb.toString
    }
    
    def para( lead: String) = {
      <p>{linkWordsAndURIs(collect(lead))}</p>
    }
    
    def item( indent: String, lead: String): Node = {
      val item = new NodeBuffer
      item += para(lead)
      
      var more = true  
      while( more && lines.hasNext ) {
        lines.peek match {
          case Bullet( newindent, newlead ) if newindent.length > indent.length =>
            lines.next
            item += bullets(newindent, newlead)
            
          case Header(_, _) | Bullet( _, _) => 
            more = false
            
          case Line( newindent, newlead) if newindent.length >= indent.length =>
            lines.next
            item += para(newlead)
            
          case _ => 
            more = false
        }
      } 
      
      <li>{item}</li>
    }
    
    def bullets( indent: String, lead: String): Node = {
      val items = new NodeBuffer
      items += item(indent, lead)
        
      var more = true  
      while( more && lines.hasNext ) {
        lines.peek match {
          case Bullet( newindent, newlead ) if newindent.length == indent.length =>
            lines.next
            items += item(indent, newlead)

          case _ => 
            more = false
        }
      }   
      <ul>{items}</ul>  
    }
    
    var title: String = null
    val buf = new NodeBuffer

    while( lines.hasNext) {
      lines.next match {
        
        case Header( "=", head) => 
          if( title == null )
            title = head
          else 
            buf += <h1>{head}</h1> 
        
        case Header( "==", head) => buf += <h2>{head}</h2> 
        case Header( "===", head) => buf += <h3>{head}</h3>
        case Bullet( indent, lead) => buf += bullets( indent, lead)
        case Line(_, lead) => buf += para(lead)
        case _ =>                              
      }
    }
    
    if( title == null )
      title = stripext(f.getName)
    (title, buf)
  }
}
