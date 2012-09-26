package au.com.langdale.publisher

import java.io.File
import java.util.Date

import Util._

/**
 * Converts the HTML extracted from Tomboy notes to Markdown.
 */

import Tomboy._

trait Tomboy2Markdown extends Publisher { this: Tomboy with Html2Markdown =>
  
  val markdown: File

  override def publish {
    super.publish
    
    ensureDir(markdown)
    
    for(note <- notesByKey.values if note.valid) {
      
      val destin = markdown / note.page
      println(note.source + " ~> " + destin)
      
      val text = mdH(1, segments(note.title)) ++ mdContent(note.content)
      
      try {
        save(text.mkString, destin)
      } 
      catch {
        case e => println(e); e.printStackTrace
      }  
    }
  }
}
