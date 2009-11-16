package au.com.langdale.publisher
import scala.xml.NodeSeq

trait Disqus {
  val disqusID: String
      
  def disqusScript = "http://disqus.com/forums/"+ disqusID +"/embed.js"
  def disqusRef = "http://disqus.com/forums/"+ disqusID +"/?url=ref"
  val disqusDebug = false
  
  lazy val comments =
    <div id="comments">
      <div id="disqus_thread"></div>
      { if(disqusDebug) <script type="text/javascript">  var disqus_developer = 1; </script> else NodeSeq.Empty }
      <script type="text/javascript" src={disqusScript}></script>
      <noscript><a href={disqusRef}>View the discussion thread.</a></noscript>
    </div>  
}
