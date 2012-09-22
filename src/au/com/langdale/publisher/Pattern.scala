package au.com.langdale.publisher
import scala.xml.{XML, Node, NodeSeq, Text, Elem, MetaData}
import scala.collection.mutable.ListBuffer

/**
 * Conveniently create extractors for XML which we call Patterns.
 */
object Pattern {
  type Rule = PartialFunction[Node, Seq[Node]]
  
  trait Pattern {
    def unapplySeq(n: Node): Option[Seq[Node]]
  }
  
  def pattern(rule: Rule): Pattern = new Pattern {
    def unapplySeq(n: Node): Option[Seq[Node]] = {
      if( rule.isDefinedAt(n))
        Some(rule(n))
      else
        None  
    }
  }
  
  def rule( ex: Pattern ): Rule = {
    case ex(ns @ _*) => ns
  }
  
  implicit def rule(e: Elem): Rule = rule( pattern(e))
  
  implicit def pattern(e: Elem): Pattern  = new Pattern {
    def unapplySeq(n: Node): Option[Seq[Node]] = n match {
      case n: Elem 
        if e.label == n.label 
          && e.namespace == null || e.namespace == n.namespace 
          && e.attributes.forall( n.attributes.iterator.contains(_)) =>
        Some(n.child)
      case _ =>
        None
    }
  }
  
  def combine(ps: Pattern*) = new Pattern {
    def unapplySeq(n: Node): Option[Seq[Node]] = {
      var b = new ListBuffer[Node]
      for( p <- ps) {
        n match {
          case p(ns @ _) => b ++= ns
          case _ => return None
        }
      }
      Some(b.toList)
    }
  }
  
  def pattern(key: String): Pattern = new Pattern {
    def unapplySeq(n: Node): Option[Seq[Node]] = n.attribute(key)
  }

  def find(input: Node)(rule: Rule): NodeSeq = {
    val f = finder(pattern(rule))
    input match {
      case f(ns @ _*) => ns
      case _ => NodeSeq.Empty
    }
  }
    
  object wild extends Node {
    def child = NodeSeq.Empty
    def label = "*"
  }

  def finder(inner: Pattern): Pattern = new Pattern {
    def unapplySeq(n: Node): Option[Seq[Node]] = {
      val o = inner.unapplySeq(n)
      if( o.isDefined )
        o
      else {
        val els = n.child.iterator
        var o: Option[Seq[Node]] = None
        while( o.isEmpty && els.hasNext) 
          o = unapplySeq(els.next)
        o
      }
    }
  }

}
