package au.com.langdale.publisher

import RSS.Item
import Util._
import scala.xml.NodeSeq.Empty

/**
 * A template for an RSS2 document.
 */
object RSS2 {
    
  trait ImageItem extends Item {
    val image: String
    val thumbnail: String
    val caption: String
  } 
  
  def apply(channel: Item, items: Seq[Item]) = 
    <rss version="2.0" xmlns:dc="http://purl.org/dc/elements/1.1/" xmlns:media="http://search.yahoo.com/mrss/">
    <channel>
      <title>{channel.title}</title>
      <description>{channel.description}</description>
      <link>{channel.url}</link>
      <lastBuildDate>{dateFormat.format(channel.date)}</lastBuildDate>
      {
        for( i <- items ) yield
          <item>
            <title>{i.title}</title>
            <link>{i.url}</link>
            <description>{i.description}</description>
            <guid>{i.url}</guid>
            <pubDate>{dateFormat.format(i.date)}</pubDate>
            {
              i match {
                case m: ImageItem =>
                  <media:group>
                    <media:content type="image/jpeg" medium="image" url={m.image}/>
                    <media:title>{m.caption}</media:title>
                    <media:thumbnail url={m.thumbnail} />
                  </media:group>
                    
                case _ => Empty
              }    
            }
            
          </item>
      }  
    </channel>  
    </rss>

}
