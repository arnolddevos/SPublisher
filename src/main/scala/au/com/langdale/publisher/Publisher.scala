package au.com.langdale.publisher

import scala.collection.mutable.{HashSet,HashMap}
import scala.xml.NodeSeq

/**
 * Defines the publishing phases, scan and publish, that are implemented 
 * by the various traits that generate parts of the site.  
 *
 * Also provides a sitemap.
 */ 
trait Publisher {
  
  // stackable methods to be overriden in mixins
  def scan {}
  def publish {}
  def getContent( pageName: String ): Option[(String, NodeSeq)] = None ///  get the content of an HTML page by pageName

  // global resources available to mixins
  val pageNames = new HashSet[String]  /// base filenames for all HTML pages (names end in '.html') 
  val imageNames = new HashSet[String] /// base filenames for all images
  val linkedTerms = new HashMap[String,String] /// mapping from phrases to page names for wiki hyperlinking
  
  // start here
  def build { scan;  publish } /// create the static web site ready for upload
}
