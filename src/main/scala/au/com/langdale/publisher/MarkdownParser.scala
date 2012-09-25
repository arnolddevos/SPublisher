package au.com.langdale.publisher

import java.io.{File}
import scala.xml.Node
import scala.xml.NodeSeq
import org.pegdown.{PegDownProcessor,Extensions}
import Extensions._
import scala.io.Source
import scala.xml.XML
import Util._

trait MarkdownParser {
  private val pegdown = new PegDownProcessor(ALL - HARDWRAPS)
  def markdownParse(f: File): (String, NodeSeq) = {
    val mdText = Source.fromFile(f).mkString("")
    val xmlText = pegdown.markdownToHtml(mdText)
    val xml = XML.loadString("<div>" + xmlText + "</div>").child // dropWhile { _.text.trim.isEmpty}
    xml.headOption match {
      case Some(E("h1", _, t)) => (t.text.trim, xml.tail)
      case _ => (stripext(f.getName), xml)
    }
  }
}
