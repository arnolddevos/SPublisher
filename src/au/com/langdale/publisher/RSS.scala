package au.com.langdale.publisher
import java.util.Date
import Util.formatW3CDate

object RSS {
  
  trait Item {
    val url: String
    val title: String
    val description: String
    val date: Date
  }
  
  case class Feed(title: String, url: String)
  
  def apply( channel: Item, items: Seq[Item]) =
    <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns="http://purl.org/rss/1.0/" xmlns:dc="http://purl.org/dc/elements/1.1/">
      <channel rdf:about="">
        <title>{channel.title}</title>
        <link>{channel.url}</link>
        <description>{channel.description}</description>
        <items>
          <rdf:Seq>
          {
            for( i <- items ) yield
              <rdf:li resource={i.url}/>
          }  
          </rdf:Seq>  
        </items>
      </channel>
      {
        for( i <- items ) yield
          <item rdf:about={i.url}>
            <title>{i.title}</title>
            <link>{i.url}</link>  
            <description>{i.description}</description>
            <dc:date>{formatW3CDate(i.date)}</dc:date>
          </item>
      }  
    </rdf:RDF>
}
