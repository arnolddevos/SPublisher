package au.com.langdale.publisher
import scala.xml.{NodeSeq,Comment}

/**
 * Adds google analytics to the site.
 */
trait GoogleAnalytics {
  
  val familyDomain: String
  val trackerID: String
  
  private val script1 = """
    var gaJsHost = (("https:" == document.location.protocol) ? "https://ssl." : "http://www.");
    document.write(unescape("%3Cscript src='" + gaJsHost + "google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E"));
  """
  
  private val script2 = """
    try{
      var pageTracker = _gat._getTracker("TRACKER");
      pageTracker._setDomainName("DOMAIN");
      pageTracker._trackPageview();
    } catch(err) {}
  """
  
  lazy val analytics: NodeSeq = 
    <script type="text/javascript">{Comment(script1)}</script> ++
  	<script type="text/javascript">{Comment(script2.replace("TRACKER",trackerID).replace("DOMAIN",familyDomain))}</script>
}
