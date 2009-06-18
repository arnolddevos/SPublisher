package au.com.langdale.publisher

import RSS.Feed
import Util._
import scala.xml.Comment

/**
 * Template for an HTML page containing a google slideshow widget.
 */
object GalleryTemplate {

  def apply(feed: Feed) =
    <html xmlns={XHTML}>
      <head>
        <meta http-equiv="Content-Type" content="text/html; charset=utf-8"/>
        <title>{feed.title}</title>
        <script 
          src="http://www.google.com/jsapi" 
          type="text/javascript"/>
        <script 
          src="http://www.google.com/uds/solutions/slideshow/gfslideshow.js"
          type="text/javascript"/>
        
        <style type="text/css">
           <!--  
           body { background-color: #000000; }  
          .gss a img {border : none;}
          .gss {
            width: 288px;
            height: 288px;
            color: #dddddd;
            background-color: #000000;
            padding: 8px;
          }
          -->
        </style>
  
        <script type="text/javascript">
          {Comment("""
		    function load() {
		      var samples = """ + '"' + feed.url + '"' + """;
		      var options = {
                fullControlPanel : true,
		        displayTime: 2000,
		        transistionTime: 600,
		        linkTarget : google.feeds.LINK_TARGET_BLANK
		      };
		      new GFslideShow(samples, "slideshow", options);
		    }
		    google.load("feeds", "1");
		    google.setOnLoadCallback(load);
          """)}
        </script>
      </head>

      <body>
        <div id="body">
          <div id="slideshow" class="gss">Loading...</div>
        </div>
      </body>
    </html>    
}
