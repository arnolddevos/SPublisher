package au.com.langdale.publisher
import Util._
import java.io.File

/**
 * Demontrates use of the Gallery trait.
 */
object ExampleGallery {
  class GallerySite( path: String ) extends Gallery {
    val project = new File(path)
    
    val galleryName = "gallery"
    val galleryTitle = "Family Photos"
    val galleryDescription = "Family Photos"
    val distrib = project / "distrib"
    val images = project / "gallery"
    val nonLocalPrefix = "http://photos.example.com/"
  }
  
  def main( args: Array[String]) {
    val gallery = new GallerySite( args(0))
    gallery.build
  }
}
