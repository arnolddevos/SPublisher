package au.com.langdale.publisher

import java.io.{File, FileInputStream, FileOutputStream, OutputStreamWriter, InputStreamReader, Reader}
import java.text.SimpleDateFormat
import java.util.Date
import java.net.{URI, URISyntaxException}
import scala.xml.{XML, Node, NodeSeq, NodeBuffer, Text, Elem, MetaData, TopScope, Null, UnprefixedAttribute, Xhtml}
import scala.collection.mutable.ListBuffer
import scala.util.matching.Regex
import scala.util.matching.Regex.Match

/**
 * Various supporting utilities some of which might deserve to go into a library.
 */
object Util {
  type Rule = PartialFunction[Node, Seq[Node]]
  class Menu(val title: String, val entries: Seq[(String, String)])
  val emptyMenu = new Menu("", Nil)
  
  val XHTML = "http://www.w3.org/1999/xhtml"

  
  object E {
    def apply(label: String, atts: MetaData, child: Seq[Node]) = Elem( null, label, atts, TopScope, child: _*)
    def unapply(n: Node): Option[(String, MetaData, Seq[Node])] = n match {
      case e: Elem => Some(e.label, e.attributes, e.child)
      case _ => None
    }
  }
  
  object URI {
    def apply(s: String, a: String, p: String, q: String, f: String) = new URI(s,a,p,q,f).toASCIIString
    def unapply(u: String) = {
      try {
        val v = new URI(u)
        if( v.isOpaque)
          None
        else
          Some(v.getScheme, v.getAuthority, v.getPath, v.getQuery, v.getFragment)
      }
      catch {
        case _:URISyntaxException => None
      }
    }
  }
  
  def basename(path: String) = path.substring(path.lastIndexOf('/')+1)
  def stripext(path: String) = if(basename(path).contains('.')) path.substring(0, path.lastIndexOf('.')) else path
  def defaultext(path: String, ext: String) = if( basename(path) contains "." ) path else path + "." + ext
  
  def ensureDir(e: File) {
    if( e.exists ) 
      require( e.isDirectory )
    else
      e.mkdirs
  }
  
  def copyDir(d: File, e: File) {
    ensureDir(e)
    for( f <- d.listFiles; g = new File(e, f.getName))
      copy(f, g)
  }
  
  def copyFile( a:File, b:File) {
    println(a +" -> " + b)
    val ac = new FileInputStream(a).getChannel
    val bc = new FileOutputStream(b).getChannel
    ac.transferTo(0, ac.size, bc)
  }
  
  def copy(a:File, b:File) {
    if( a.isDirectory)
      copyDir(a,b)
    else
      copyFile(a,b)
  }
  
  def saveXHTML(x: Node, f: File) {
    val sb = new StringBuilder
    sb append """<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.1//EN" >
"""
    Xhtml.toXhtml(x, TopScope, sb, false, false)
    val s = new OutputStreamWriter(new FileOutputStream(f), "utf-8")
    s.write(sb.toString)
    s.close
  }
  
  class AdvancedIterator[A]( it: Iterator[A]) extends Iterator[A] {
    private var la = step
    private def step = if( it.hasNext ) Some(it.next) else None
    def peek = la.get
    def hasNext = la.isDefined
    def next = {
      val a = la.get
      la = step
      a
    }
  }

  implicit def AdvanceableIterator[A]( it: Iterator[A]) = new {
    def advance = new AdvancedIterator(it)
  }
  
  implicit def RichFile(f: File) = new {
    def /(n: String) = new File(f, n)
    def copyTo( g: File) {copy(f,g)}
    def lines = new LineIterator(new InputStreamReader( new FileInputStream(f), "utf-8"))
  }
  
  class CharIterator(fr: Reader) extends Iterator[Char] {
    private var ch: Int = _
    
    next
    
    def next = {
      val a = ch
      ch = fr.read
      if( ch == -1)
        fr.close
      a.toChar
    }
    
    def peek = ch.toChar
    
    def hasNext = ch != -1
  }
  
  class LineIterator( fr: Reader) extends Iterator[String] {
    private var it = new CharIterator(fr)

    def hasNext = it.hasNext
      
    def next = {
      val sb = new StringBuilder
      while( it.hasNext && it.peek != '\n' && it.peek != '\r') {
        sb += it.next
      }
      if( it.hasNext && it.peek == '\r') {
        it.next
        if( it.hasNext && it.peek == '\n')
          it.next
      }
      else if( it.hasNext )
        it.next
      sb.toString
    }
  }

  class M(m: MetaData) {
    def has(p: (String, String)): Boolean = m(p._1) == Text(p._2) 
    def has(k: String): Boolean = m(k) != null
  }
          
  implicit def M(m: MetaData) = new M(m) 
  implicit def M(p: Pair[String, String]): MetaData = new UnprefixedAttribute(p._1, Text(p._2), Null)
  implicit def M(e: Node) = new M(e.attributes)
  
  def find(ns: NodeSeq)(rule: Rule): NodeSeq = {
    val i = ns.iterator
    while( i.hasNext) {
      val n = i.next
      if( rule.isDefinedAt(n)) 
        return rule(n)
      else {
        val cs = find(n.child)(rule)
        if( ! cs.isEmpty )
          return cs
      }
    }
    NodeSeq.Empty
  }
 
  def findChild(ns: NodeSeq, label: String, atts: Pair[String, String]*): NodeSeq = {
    
    // compiler crash on forall so introduce this
    def testall(a: MetaData): Boolean = { 
      for( p <- atts) 
        if( ! (a has p))
          return false
      return true
    }
    
    
    find(ns) {
      case E( l, a, cs) if l == label && testall(a) => cs
    }
  }
  
  def mapMatches(regex: Regex, text: String)(f: PartialFunction[Match, NodeSeq]): NodeSeq = {
    val b = new NodeBuffer
    var offset = 0
    for( m <- regex.findAllIn(text).matchData) {
      if(f.isDefinedAt(m)) {
        if( offset < m.start)
          b += Text(text.substring(offset, m.start))
        b ++= f(m)
        offset = m.end  
      }
    }
 
    if( offset == 0) {
      Text(text)
    }
    else {
      if( offset < text.length)
        b += Text(text.substring(offset))
      b
    }
  }
  
  def tokenize(regex: Regex, text: String) = {
    val b = new ListBuffer[String]
    var offset = 0
    for( m <- regex.findAllIn(text).matchData) {
        if( offset < m.start)
          b += text.substring(offset, m.start)
        b += m.matched
        offset = m.end  
    }
    if( offset == 0)
      List(text)
    else {
      if( offset < text.length)
        b += text.substring(offset)
      b.toList
    }
 }
  
  def mapMatches(regex: Regex, text: String, f: String => String): String = {
    val b = new StringBuilder
    var offset = 0
    for( m <- regex.findAllIn(text).matchData) {
        if( offset < m.start)
          b append text.substring(offset, m.start)
        b append f(m.matched)
        offset = m.end  
    }
    if( offset == 0)
      text
    else {
      if( offset < text.length)
        b append text.substring(offset)
      b.toString
    }
  }
  
  def initialText(ns: NodeSeq, limit: Int): String = {
    val b = new StringBuilder
    for( d <- ns; if b.length < limit  ) {
      b.append( d.text )
    }
    if( b.length > limit)
      b.length = limit
    b.toString
  }
  
  def group[A, B](s: Seq[A], f: A => B): Seq[(B, Seq[A])] = {
    val outer = new ListBuffer[(B, Seq[A])]

    val it = s.iterator
    def advance = if(it.hasNext) Some(it.next) else None
    var next = advance
    
    while( next.isDefined ) {
        val key = f(next.get)

        val inner = new ListBuffer[A]
        inner += next.get
        next = advance
        
        while( next.isDefined && f(next.get) == key ) {
          inner += next.get
          next = advance
        }

        outer += key -> inner.toList
    }
    
    outer.toList
  }
  
  def split[A](s: Seq[A], f: A => Boolean): Seq[Seq[A]] = {
    val outer = new ListBuffer[Seq[A]]

    val it = s.iterator
    def advance = if(it.hasNext) Some(it.next) else None
    var next = advance
    
    while( next.isDefined ) {
        val inner = new ListBuffer[A]
        while( next.isDefined && ! f(next.get)) {
          inner += next.get
          next = advance
        }
        if( next.isDefined )
          next = advance
        if( ! inner.isEmpty)
          outer += inner.toList
    }
    outer.toList
  }
  
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss")
  val tzFormat = new SimpleDateFormat("Z")
  def formatW3CDate(date: Date) =  dateFormat.format(date) + { val tz = tzFormat.format(date); tz.substring(0,3) + ":" + tz.substring(3) }
  
  class DebugPrint[X](val x: X) {
    def debug(m: String): X = {
      println( m + ": " + x)
      x
    }
  }
  
  implicit def DebugPrint[X](x: X) = new DebugPrint(x)
}
