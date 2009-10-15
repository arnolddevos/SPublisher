package au.com.langdale.publisher

import Util._
import java.io.File

/**
 * Demonstrates the composition of a site generator from traits.
 */
class ExampleSite(path: String) 
  extends SiteResources 
  with Tomboy 
  with Blog 
  with Template 
  with Transformer 
  with WikiParser
  with TextExpander
  with Linker 
  with GoogleAnalytics
{
  val project = new File(path)
  val sources = List( project / "sitestyle", project / "resources" )
  val distrib = project / "distrib"
  val tomboy = project / "tomboy"
  val includedTags = Set[String]()

  val localPrefix = ""
  val nonLocalPrefix = "http://www.example.com/"
  val familyDomain = ".example.com"
  val siteDomains = Set("myoldsite.org", "www.myoldsite.org", "wiki.myoldsite.org")
  val trackerID = "UA-9999999-1"
  val blogTitle = "Example.com News"
  val blogDescription = "All thats new in the examples world"
  
  val menu = new Menu("Main",
    List(
      "Home" -> "About_This_Web_Site.html", 
      "News Feed" -> "blog.html"
    )
  )
    
  val footer =
    <div>
      Web site created by <a href="http://github.com/arnolddevos/SPublisher">Static Site Publisher (in Scala)</a>. 
    </div>      
}

object ExampleSite {
  def main( args: Array[String]) {
    new ExampleSite(args(0)).build
  }
}
