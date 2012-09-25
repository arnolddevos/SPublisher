package au.com.langdale.publisher

import java.io.File
import java.util.Date
import scala.xml.{Node, Text, Elem}

import Util._

/**
 * Converts the HTML extracted from Tomboy notes to Markdown.
 */

import Tomboy._

trait Tomboy2Markdown extends Publisher { this: Tomboy =>
  
  val markdown: File
  val codeLanguage = "scala"

  override def publish {
    super.publish
    
    ensureDir(markdown)
    
    for(note <- notesByKey.values if note.valid) {
      val destin = markdown / note.page
      println(note.source + " ~> " + destin)
      
      val text = mdH(1, Seq(Segment(note.title))) ++ (note.content flatMap blockRule)
      
      try {
        save(text.mkString, destin)
      } 
      catch {
        case e => println(e); e.printStackTrace
      }  
    }
  }
  
  case class Segment(text: String="", indent: Int = 0) {
    def wrap(a: String, b: String="") = Segment(a + text + b, indent)
    override def toString = "\n" + " " * indent + text
  }
  
  val br = Segment()
  
  def segments(text: String, level: Int) = 
    (for( l <- text.lines) yield Segment(l.trim, level)).toSeq
    
  //def wrap(ts: Seq[Segment], a: String, b: String) =
 
  private def mdPRE(t: String): Seq[Segment] = br +: segments(t, 8)
  private def mdUL(ts: Seq[Segment]): Seq[Segment]  = br +: ts
  private def mdP(ts: Seq[Segment]): Seq[Segment]  = br +: ts
  private def mdH(l: Int, ts: Seq[Segment]): Seq[Segment]  = br +: (ts map (_.wrap("#" * l + " ")))
  private def mdLI(ts: Seq[Segment]): Seq[Segment]  = if(ts.isEmpty) Seq() else ts.head.wrap("* ") +: (ts.tail map (_.wrap("  ")))
  private def mdBR: Seq[Segment] = Seq(br)
  private def mdA(hr: Option[String], ts: Seq[Segment]): Seq[Segment] = hr map { l => ts map (_.wrap("[", "](" + l + ")"))} getOrElse ts
  private def mdEM(ts: Seq[Segment]): Seq[Segment]  = ts map (_.wrap("_", "_"))
  private def mdSTRONG(ts: Seq[Segment]): Seq[Segment]  = ts map (_.wrap("__", "__"))
  private def mdBIG(ts: Seq[Segment]): Seq[Segment]  = mdSTRONG(ts)
  private def mdSMALL(ts: Seq[Segment]): Seq[Segment]  = ts
  private def mdDEL(ts: Seq[Segment]): Seq[Segment]  = ts
  private def mdCODE(ts: Seq[Segment]): Seq[Segment]  = ts map (_.wrap("`", "`"))
  private def mdSPAN(c: Option[String], ts: Seq[Segment]): Seq[Segment]  = ts

  type MdRule = PartialFunction[Node, Seq[Segment]]
  
  private val blockRule: MdRule = {
    case <pre>{child@_*}</pre> => mdPRE(child.text)
    case <br/> => mdBR 
    case <ul>{child@_*}</ul> => mdUL(child flatMap blockRule)
    case <li>{child@_*}</li> => mdLI(child flatMap blockRule)  
    case <h1>{child@_*}</h1> => mdH(1, child flatMap textRule)  
    case <h2>{child@_*}</h2> => mdH(2, child flatMap textRule)  
    case <h3>{child@_*}</h3> => mdH(3, child flatMap textRule)  
    case <p>{child@_*}</p> => mdP(child flatMap textRule)  
    case n => textRule(n)
  }
  
  private val textRule: MdRule = {
    case E("a", atts, child) => mdA(atts.get("href").map(_.text), child flatMap textRule)
    case <strong>{child@_*}</strong> => mdSTRONG(child flatMap textRule)  
    case <em>{child@_*}</em> => mdEM(child flatMap textRule)
    case <del>{child@_*}</del> => mdDEL(child flatMap textRule)  
    case E("span", atts, child) => mdSPAN(atts.get("class").map(_.text), child flatMap textRule)  
    case <small>{child@_*}</small> => mdSMALL(child flatMap textRule)  
    case <big>{child@_*}</big> => mdBIG(child flatMap textRule)  
    case <code>{child@_*}</code> => mdCODE(child flatMap textRule)  
    case E(_, _, child) => child flatMap textRule
    case Text(t) => segments(t, 0) filter (! _.text.isEmpty)
    case _ => Seq()
  }
}
