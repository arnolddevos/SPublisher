package au.com.langdale.publisher

import java.io.File
import java.util.Date
import Util._
import ImageUtil._
import RSS2.ImageItem
import RSS.{Item, Feed}
import scala.collection.mutable.ListBuffer
import scala.xml.XML

trait Gallery extends Publisher {
  
  val images: File
  val distrib: File
  val nonLocalPrefix: String
  val galleryTitle: String
  val galleryDescription: String
  val galleryName: String
  
  val THUMBSIZE = 280
  
  val items = new ListBuffer[GalleryItem]
  
  class GalleryItem(val source: File) extends ImageItem {
    def name = source.getName
    def thumbName = "th-" + name

    val url = nonLocalPrefix + name
    val title = name
    val description = ""
    val date = new Date(source.lastModified)
    val image = nonLocalPrefix + name
    val thumbnail = nonLocalPrefix + thumbName
    val caption = ""
  }
  
  override def scan {
    super.scan
    require(images.isDirectory)
    
    for( f <- images.listFiles; if f.isFile && isImage(f.getName))
      items += new GalleryItem(f)
  }
  
  def isImage(n: String) = n.endsWith(".jpg")
  
  override def publish {
    super.publish
    ensureDir(distrib)
    
    for( i <- items ) {
      copy( i.source, distrib / i.name )
      readImage( i.source ).fit( THUMBSIZE ).writeJPEG( distrib / i.thumbName )
    }

    object channel extends Item {
      val url = nonLocalPrefix + galleryName + ".html"
      val title = galleryTitle;
      val description = galleryDescription;
      val date = new Date
    }
    
    XML.saveFull((distrib / (galleryName + ".rss")).getPath, RSS2(channel, items), "utf-8", true, null)
    val feed = Feed( galleryTitle, nonLocalPrefix + galleryName + ".rss")
    saveXHTML(GalleryTemplate( feed ), distrib / ( galleryName + ".html"))
  }  

}
