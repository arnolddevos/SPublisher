# Static Site Publisher

Creates static web sites from raw html, markdown,
generic wiki markup, tomboy notes, images and other resources.

A generated site can have an RSS feed and a basic blog,
an image feed and slideshow, and analytics support.

## Deprecated in Favour of Soapbox

The most useful features of SPublisher have now been migrated to the sbt environment in [Soapbox](https://github.com/arnolddevos/Soapbox). i
Some things, such as RSS, were dropped. 
Markdown replaces the tomcat notes utility for authoring content.

## Recent Changes

* SPublisher has gained [Markdown] support. The [pegdown] markdown parser is a dependency.
* Now compiles with scala 2.9.x or even recent 2.10 milestones abeit with warnings.  The previous version needed scala 2.7 which seems antique now.
* Builds with [sbt]. (Run `sbt eclipse` to generate an eclipse project.)

## Rationale and Alternatives

There are many, many alternatives.  One could use [sbt] with the [lwm] plugin or similar.  Or [Jekyll], which github pages uses.

SPublisher represents the particular text transformations and tools needed by the author. 

It was a first scala project or at least an early one.  I think the code is pretty clean but it is not really approved scala style.  It is something like python in scala. 

## Examples

A site generated with this software and hosted on S3 can be seen here: 
http://wiki.cimtool.org/index.html

Peruse the examples directory tree and `ExampleSite.scala` for another. 
The output is in example/distrib while the inputs are in example/resource
example/tomboy and example/sitestyle . 

You can edit the inputs and regenerate the output by running 
`sbt run au.com.langdale.publisher.ExampleSite` .

An example slideshow and image feed generator 
can be seen in `ExampleGallery.scala` . 

## Usage

 * Copy the example directory tree and `ExampleSite.scala` .
 * Customise the site generation object.
 * Add content. 

The site is configured by combining `Publisher` traits into
a site generation object. 

 * Each `Publisher` trait is responsible
   for some aspect of site generation. 
   See the trait comment for a synopsis. 

 * A trait will declare abstract members that it
   expects others to implement (dependancies).

 * A trait will implement members 
   used by other traits (dependents).

 * Some abstract members represent configuration
   constants that should be defined in the site object. 

[markdown]: http://daringfireball.net/projects/markdown/
[sbt]: https://github.com/harrah/xsbt/wiki
[lwm]: http://software.clapper.org/sbt-lwm/
[jekyll]: http://jekyllrb.com/
[pegdown]: https://github.com/sirthias/pegdown

