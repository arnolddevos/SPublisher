package au.com.langdale.publisher

import scala.xml.{NodeSeq, Node}
import Util._
import RSS.Feed

/**
 * Applies a template to each page. 
 *
 * The template provides a generic structure for the page, 
 * relying on CSS for layout as well as styling.
 */
trait Template {

  val menu: Menu
  val footer: NodeSeq
  val analytics: NodeSeq
  val comments: NodeSeq
  
  def fixLink(href: String): (String, String)
 
  def expandMenu( menu: Menu ): Node =
    <ul>
    {
      for((name, target) <- menu) yield {
        val (style, href) = fixLink(target)
        <li><a href={href} class={style}>{name}</a></li>
      }
    }
    </ul>  
  
  def expandFeeds(feeds: Seq[Feed]) = 
    for( Feed(title, link) <- feeds ) yield {
      link debug "feed"
      <link rel="alternate" type="application/rss+xml" title={title} href={link} />
    }

  def expand(title: String, content: NodeSeq): Node = expand(title, content, Nil, false)
  def expand(title: String, content: NodeSeq, feeds: Seq[Feed]): Node = expand(title, content, feeds, false)
  def expand(title: String, content: NodeSeq, showComments: Boolean): Node = expand(title, content, Nil, showComments)
  
  def expand(title: String, content: NodeSeq, feeds: Seq[Feed], showComments: Boolean) =
    <html xmlns={XHTML}>
      <head>
        <title>{title}</title>
        <link rel="stylesheet" href="graphic.css" type="text/css" />
        <link rel="icon" href="favicon.ico" type="image/x-icon" />
        <link rel="shortcut icon" href="favicon.ico" type="image/x-icon" />  
        { expandFeeds(feeds) }    
      </head>
      <body>
        
        <div id="banner">
          <img id="illust" src="illust-composite.png"/>
          <img id="logo" src="logo-composite.png"/>
          <h1>{title}</h1>
        </div>
        <div id="main">
          <div id="content">{ content }</div>
          { if(showComments) comments else NodeSeq.Empty }
          <div id="menu">{ expandMenu(menu) }</div>
        </div>  
        <div id="footer">{ footer }</div>
        { analytics }  
      </body>
    </html>
}
