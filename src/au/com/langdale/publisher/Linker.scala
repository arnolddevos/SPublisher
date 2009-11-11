package au.com.langdale.publisher
import Util._


/**
 * Generates internal and external URIs and links for the site.
 */
trait Linker extends Publisher {

  val siteDomains: Set[String] /// all the domains at which this site will be deployed. 
                               /// URLs in these domains will be converted to relative.
  val familyDomain: String     /// any link to a domain ending in this is styled as 'internal' 
  val localPrefix: String      /// prefix added to bare file names to form a relative URL
  val nonLocalPrefix: String   /// prefix added to bare file names to form an absolute URL
  
  /**
   * Take a URI appearing in a raw resource and clean it up and classify it.
   */
  def fixLink(href: String) = href match {
    case u @ URI(s, a, p, q, f) if s == null || s == "http" || s == "https" =>
      if( a == null || (siteDomains contains a))
        if( p.length > 0) {
          val r = defaultext(if(p.startsWith("/")) p.substring(1) else p, "html") 
          if( r.endsWith(".html") && ! (pageNames contains r))
            ("internal", nonLocalPrefix + URI(null, null, r, null, f))
          else
            ("internal", localPrefix + URI(null, null, r, null, f))
        }
        else if( f != null )
          ("contents", URI(null, null, null, null, f))
        else
          ("internal", nonLocalPrefix + "index.html" )
      else if( a endsWith familyDomain)
          ("internal", u)
      else
          ("external", u)  
    case u if u startsWith "mailto:" =>  
      ("mail", u)
    case u =>
      ("nonavigate", u)
  }
}
