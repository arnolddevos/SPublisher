package au.com.langdale.publisher
import Util._

import scala.xml.{NodeSeq, Text}
import scala.util.matching.Regex.Match

/**
 * Generates internal and external URIs and links for the site.
 */
trait Linker extends Publisher {
  
  def fixLink(href: String) = href match {
    case u @ URI(s, a, p, q, f) if s == null || s == "http" || s == "https" =>
      if( a == null || (oldDomains contains a))
        if( p.length > 0) {
          val r = defaultext(if(p.startsWith("/")) p.substring(1) else p, "html") 
          if( r.endsWith(".html") && ! (pageNames contains stripext(r)))
            ("internal", nonLocalPrefix + URI(null, null, r, null, f))
          else
            ("internal", localPrefix + URI(null, null, r, null, f))
        }
        else
          ("contents", URI(null, null, null, null, f))
      else if( a endsWith familyDomain)
          ("internal", u)
      else
          ("external", u)  
    case u if u startsWith "mailto:" =>  
      ("mail", u)
    case u =>
      ("nonavigate", u)
  }
  
  def expandWord( word: String) = {
    if( word contains "_" )
      word.replace("_",  " ")
    else
      tokenize(CamelHump, word).mkString( " " )  
  }
  
  def linkWords(text: String): NodeSeq = mapMatches( Word, text) {
    case Match(word) if pageNames contains word => 
      <a href={localPrefix + word + ".html"} class="internal">{expandWord(word)}</a>
  }
  
  val URIPattern = """(https?://[^ ]+)""".r
  val BigWord = """[^ \n\r]+""".r
  val ImagePattern = """\[(\w+)\.(png|jpg|gif)]""".r
  val Word = """\w+""".r
  val CamelHump = """\p{Lu}+[^\p{Lu}]*""".r
  
  object Link {
    def unapply(uri: String): Option[(String, String)] = fixLink(uri) match {
      case ("nonavigate", _) => None
      case x => Some(x)
    }
  }
  
  object WikiWords {
    def unapply( text: String): Option[NodeSeq] = linkWords(text) match {
      case Text(x) => None
      case x => Some(x)
    }
  }
  
  def linkWordsAndURIs(text: String): NodeSeq = mapMatches( BigWord, text) {
    case URIPattern(Link(style, href)) =>  <a href={href} class={style}>{href}</a>
    case ImagePattern(name, ext) if imageNames contains name + "." + ext => <img src={name + "." + ext}/>  
    case Match(WikiWords(x)) => x  
  }

  val oldDomains: Set[String]
  val familyDomain: String
  val localPrefix: String
  val nonLocalPrefix: String
}
