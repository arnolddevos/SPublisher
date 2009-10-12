package au.com.langdale.publisher

import java.io.File
import java.util.Date

import scala.xml.{XML, Node, NodeSeq, Text, Elem, NodeBuffer}
import scala.xml.NodeSeq.Empty
import scala.collection.mutable.{HashMap, ListBuffer, HashSet}
import scala.collection.Set

import Util._
import RSS.Feed

/**
 * Adds all tagged notes from a Tomboy repository to the site as html pages.
 */
object Tomboy {
  val TOMBOY="http://beatniksoftware.com/tomboy"
  val SIZE="http://beatniksoftware.com/tomboy/size"
  val LINK="http://beatniksoftware.com/tomboy/link"
  val suffix = ".note"
  val templateTag = "system:template"
}

import Tomboy._

trait Tomboy extends Publisher {

  val tomboy: File
  val distrib: File
  val includedTags: Set[String]
  val localPrefix: String
  val nonLocalPrefix: String
  val homePage = Some("Home")
  def expand( title: String, content: NodeSeq, feeds: Seq[Feed]): Node
  def fixLink(href: String): (String, String)
  def linkWordsAndURIs(t: String): NodeSeq
  
  val notesByKey = new HashMap[String, Note]
  val notesByTag = new HashMap[String, ListBuffer[Note]]
  
  def history = notesByKey.values.filter(_.valid).toList.sort( _.date after _.date ).take(12)

  def formWikiWord(name: String): String =  """\w+""".r.findAllIn(name).mkString("_") 

  class Note( val source: File) extends RSS.Item {
    private val x = XML.loadFile(source)

    val title = findChild(x, "title").text.trim
    val wikiWord = formWikiWord(title)
    val key = wikiWord.toLowerCase
    val page = if( linkedTerms contains wikiWord ) linkedTerms(wikiWord) else wikiWord + ".html"
    val link = localPrefix + page
    val url = nonLocalPrefix + page

    val tags = findChild(x, "tags") flatMap { 
      case <tag>{Text(tn)}</tag> => List(tn.trim) 
      case _ => Nil 
    }

    val date = dateFormat.parse(findChild(x, "create-date").text.trim)

    lazy val content = findChild(x, "text")  flatMap blockRule
    lazy val description = findChild(content, "p").text.trim 
    
    def valid =  ! inValid
    def inValid = title.isEmpty || tags.isEmpty || tags.contains(templateTag) ||
                  ! includedTags.isEmpty  && ! tags.exists( includedTags contains _ ) 
  
  }
  
  override def getContent( pageName: String ): Option[(String, NodeSeq)] = {
    notesByKey.get(pageName.toLowerCase) match {
      case Some(note) => Some(note.title, note.content)
      case None => super.getContent( pageName )
    }
  }
  
  override def scan {
    super.scan
    
    require(tomboy.isDirectory)

    for( f <- tomboy.listFiles; if f.isFile; name = f.getName; if name.endsWith(suffix)) {
      val note = new Note(f)
      notesByKey(note.key) = note
      if(note.valid) {
        pageNames += note.page
        for( tag <- note.tags )
          notesByTag.getOrElseUpdate(tag, new ListBuffer[Note]) += note
      }
    }
  }

  override def publish {
    super.publish
    
    ensureDir(distrib)
    
    for(note <- notesByKey.values if note.valid) {
      val destin = distrib / note.page
      println(note.source + " ~> " + destin)
      
      try {
        saveXHTML(expand( note.title, note.content, Nil), destin)
      } 
      catch {
        case e => println(e); e.printStackTrace
      }  
    }
  }
  
  def mkMenu(tag: String) = new Menu("tag", 
    notesByTag.getOrElse("system:notebook:" + tag, Nil).toList
    sort(_.title < _.title)
    map( note => (note.title, note.page)))

  private def internalLink(t: String): NodeSeq = {
    val k = formWikiWord(t).toLowerCase
    notesByKey.get(k) match {
      case Some(note) if note.valid =>
        <a href={note.link} class="internal">{note.title}</a>
      case _ =>
        println("broken: " + t)
        linkWordsAndURIs(t)
    }
  }
    
  private def breakup(t: String): NodeSeq = {
    val ns = new NodeBuffer
    var delim = false
    for( line <- t.lines) {
      if( line.trim.isEmpty ) {
        ns += <br gap="big"/>
        delim = false  
      }
      else {
        if( delim )
          ns += <br/>
        ns ++= linkWordsAndURIs(line)
        delim = true
      }
    }
    ns
  }
    
  private def paras(ns: NodeSeq): NodeSeq = {
    val buf = new NodeBuffer
    for( (block, g) <- group(ns, blockElem(_: Node))) {
      val f = if( block )
        g
      else 
        for( h <- split(g, breakElem(_:Node))) yield 
          if( h.length == 1 )
            h.first match {
              case <code>{child@_*}</code> => <pre>{child}</pre>
              case <strong>{child@_*}</strong> => <h3>{child}</h3>  
              case <big>{child@_*}</big> => <h2>{child}</h2>
              case _ => <p>{h}</p>
            }
          else
            <p>{h}</p> 
      buf ++= f
    }
    buf
  }
  
  private def blockElem( n: Node) = n match {
    case <h1>{_*}</h1> => true
    case <h2>{_*}</h2> => true
    case <ul>{_*}</ul> => true
    case _ => false
  }
  
  private def breakElem(n: Node) = n == <br gap="big"/>
  
  private val blockRule: Rule = {
    case <note-content>{Text(t), rest@_*}</note-content> => paras(breakup(t.substring(t.indexOf('\n') + 1)) ++ (rest flatMap blockRule))
    case <list>{child@_*}</list> => <ul>{child flatMap blockRule}</ul>
    case <list-item>{child@_*}</list-item> => <li>{child flatMap blockRule}</li>  
    case n => textRule(n)
  }
  
  private val textRule: Rule = {
    case <url>{url@_*}</url> =>
      val (style, href) = fixLink(url.text)
      <a href={href} class={style}>{href}</a>
    case <internal>{t@_*}</internal> => internalLink(t.text)  
    case <bold>{child@_*}</bold> => <strong>{child flatMap textRule}</strong>  
    case <italic>{child@_*}</italic> => <em>{child flatMap textRule}</em>  
    case <strikethrough>{child@_*}</strikethrough> => <del>{child flatMap textRule}</del>  
    case <highlight>{child@_*}</highlight> => <span class="highlight">{child flatMap textRule}</span>  
    case <small>{child@_*}</small> => <small class="small">{child flatMap textRule}</small>  
    case <large>{child@_*}</large> => <big>{child flatMap textRule}</big>  
    case <huge>{child@_*}</huge> => <span class="huge">{child flatMap textRule}</span>  
    case <monospace>{child@_*}</monospace> => <code>{child flatMap textRule}</code>  
    case <datetime>{child@_*}</datetime> => <span class="datetime">{child flatMap textRule}</span>  
    case e: Elem => e.child flatMap textRule
    case Text(t) => breakup(t)
    case _ => Empty
  }
}
