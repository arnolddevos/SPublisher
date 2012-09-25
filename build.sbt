name := "spublisher"

version := "0.2"

libraryDependencies += "org.pegdown" % "pegdown" % "1.1.1"

resolvers += "Local Maven Repository" at "file://"+Path.userHome.absolutePath+"/.m2/repository"
