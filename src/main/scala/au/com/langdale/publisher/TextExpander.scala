package au.com.langdale.publisher
  
import scala.xml.{NodeSeq, Text, Node}
import scala.util.matching.Regex.Match
import scala.util.parsing.combinator.RegexParsers
import Util._

trait TextExpander extends Publisher {

  def fixLink(href: String): (String, String)  
  val localPrefix: String
  
  private object Expander extends RegexParsers {
    override def skipWhitespace = false
    
    def ws = regex(whiteSpace) ^^ { x => Text(x) }
    
    def other = regex("""\S+""".r) ^^ { x => Text(x) }
    
    def word = regex("""\w+""".r)

    def reference = regex("""(\w|-)+(\.(\w|-)+)+(/[^]\[}{>< ]+)?""".r) 
    
    def uri = regex("""https?://[^]\[}{>< ]+""".r)
    
    val imageNameRE = """\w+\.(jpg|png|gif)""".r
    
    def imageName = regex(imageNameRE)
    
    def imageLink = "[" ~ imageName ~ "]" ^? {
      case _ ~ n ~ _ if imageNames contains n => <img src={n}/>  
    }
    
    def uriLink = uri ^^ {
      u =>  fixLink(u) match {
        case (style, href) => <a href={href} class={style}>{u}</a>
      }
    }
    
    def refLink = word ~ opt(ws) ~ "[" ~ reference ~ "]" ^? {
      case word ~ _ ~ _ ~ r ~ _ if imageNameRE.unapplySeq(r).isEmpty => 
        fixLink("http://" + r) match {
          case (style, href) => <a href={href} class={style}>{word}</a>
        }
    }
    
    def wordLink = word ^? {
      case w  if linkedTerms contains w => 
        <a href={localPrefix + linkedTerms(w)} class="internal">{expandWord(w)}</a>
      case w if pageNames contains w + ".html" => 
        <a href={localPrefix + w + ".html"} class="internal">{expandWord(w)}</a>
    }
    
    def part: Parser[NodeSeq] = imageLink | refLink | uriLink | wordLink | ws | other
    
    def whole: Parser[NodeSeq] = rep(part) ^^ { _.reduceLeft( _ ++ _ ) }
    
    def parse(text: String): NodeSeq = parse(whole, text).get
  }
  
  /**
   * Convert a string to a sequence of nodes with hyperlinks.
   */
  def linkWordsAndURIs(text: String) = Expander.parse(text)
  
  /**
   * Prettify a wikiWord for presentation by replacing underscores.
   */
  private def expandWord( word: String) = {
    if( word contains "_" )
      word.replace("_",  " ")
    else
      tokenize(CamelHump, word).mkString( " " )  
  }

  private val CamelHump = """\p{Lu}+[^\p{Lu}]*""".r

}
