package au.com.langdale.publisher

import scala.collection.mutable.HashSet
import scala.xml.NodeSeq

/**
 * Defines the publishing phases, scan and publish, that are implemented 
 * by the various traits that generate parts of the site.  
 *
 * Also provides a sitemap.
 */ 
trait Publisher {
  def scan {}
  def publish {}

  val pageNames, imageNames = new HashSet[String]
  def getContent( wikiWord: String ): Option[(String, NodeSeq)] = None
  def build { scan;  publish }
}
