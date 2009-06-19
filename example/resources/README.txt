= Static Site Publisher =

Creates static web sites from raw html, wiki markup, 
tomboy notes, images and other resources.

A generated site can have an RSS feed and a basic blog,
an image feed and slideshow, and analytics support.

== Examples ==

A site generated with this software and hosted on S3 can be seen here: 
http://wiki.cimtool.org/index.html

Peruse the examples directory tree and ExampleSite.scala for another.  
The output is in example/distrib while the inputs are in example/resource
example/tomboy and example/sitestyle . 

You can edit the inputs and regenerate the output by launching the provided
Publisher eclipse run configuration.  If you are not using eclipse, run the 
au.com.langdale.publisher.Main class with the path of the example directory
as the argument.

An example slideshow and image feed generator 
can be seen in ExampleGallery.scala .  

== Usage ==

 * Copy the example directory tree and ExampleSite.scala .
 * Customise the site generation object.
 * Add content.  

The site is configured by combining Publisher traits into
a site generation object.   

 * Each Publisher trait is responsible
   for some aspect of site generation.  
   See the trait comment for a synopsis.  

 * A trait will declare abstract members that it
   expects others to implement (dependancies).

 * A trait will implement members 
   used by other traits (dependents).

 * Some abstract members represent configuration
   constants that should be defined in the site object. 
