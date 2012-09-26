package au.com.langdale.publisher

import scala.xml.{Node, NodeSeq, Text, Elem}

import Util._

trait Html2Markdown {

  val codeLanguage = "scala"
  
  case class Segment(text: String) {
    override def toString = "\n" + text
  }
  
  val br = Segment("")
  
  private def wrap(a: String, b: String="")(s: Segment) = Segment(a + s.text + b)
  
  def segments(text: String) = (text.lines map (Segment(_))).toSeq 
 
  def mdContent(html: NodeSeq)  = html flatMap blockRule
  def mdText(t: String)         = segments(t) filter (! _.text.trim.isEmpty)
  def mdP(ts: Seq[Segment])     = br +: ts
  def mdPRE(t: String)          = br +: Segment("```" + codeLanguage) +: segments(t) :+ Segment("```")  
  def mdBR                      = Seq(br)
  def mdUL(ts: Seq[Segment])    = br +: ts
  def mdLI(ts: Seq[Segment])    = if(ts.isEmpty) Seq() else wrap("* ")(ts.head) +: (ts.tail map wrap("  "))
  def mdH(level: Int, ts: Seq[Segment]) = br +: (ts map wrap("#" * level + " "))
  def mdA(href: Option[String], ts: Seq[Segment]) = href map { l => ts map wrap("[", "](" + l + ")")} getOrElse ts
  def mdEM(ts: Seq[Segment])    = ts map wrap("_", "_")
  def mdSTRONG(ts: Seq[Segment]) = ts map wrap("__", "__")
  def mdBIG(ts: Seq[Segment])   = mdSTRONG(ts)
  def mdSMALL(ts: Seq[Segment]) = ts
  def mdDEL(ts: Seq[Segment])   = ts
  def mdCODE(ts: Seq[Segment])  = ts map wrap("`", "`")
  def mdSPAN(c: Option[String], ts: Seq[Segment]) = ts

  private type MdRule = PartialFunction[Node, Seq[Segment]]
  
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
    case Text(t) => mdText(t)
    case _ => Seq()
  }
}
