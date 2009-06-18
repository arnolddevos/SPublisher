package au.com.langdale.publisher

import scala.collection.mutable.HashSet
import scala.xml.NodeSeq

trait Publisher {
  def scan {}
  def publish {}

  val pageNames, imageNames = new HashSet[String]
  def getContent( wikiWord: String ): Option[(String, NodeSeq)] = None
  def build { scan;  publish }
}
