= Static Site Publisher =

Creates static web sites from raw html, wiki markup, 
tomboy notes, images and other resources.

A generated site can have an RSS feed and a basic blog,
an image feed and slideshow, and analytics support.

== Usage ==

Copy ExampleSite.scala and customise.  

The site is configured by combining Publisher traits into
a site generation object.   

Each Publisher trait is responsible
for some aspect of site generation.  

It will declare abstract members
that must be implemented by other traits (dependancies).

It will implement members used by other traits (dependents).

Other abstract members represent configuration
constants and may be defined in the site object. 




