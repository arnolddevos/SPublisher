package au.com.langdale.publisher

object Main {
  def main( args: Array[String]) {
    new ExampleSite(args(0)).build
  }
}
