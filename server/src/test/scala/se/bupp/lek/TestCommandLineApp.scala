package se.bupp.lek

/**
 * Created with IntelliJ IDEA.
 * User: karlw
 * Date: 2012-10-31
 * Time: 06:34
 * To change this template use File | Settings | File Templates.
 */
object TestCommandLineApp {
  val usage = """
    Usage: mmlaln [options] filename
      options :
        --min-size num
        --max-size num
        --use-yeah
        """
  def main(args: Array[String]) {
    if (args.length == 0) println(usage)
    val arglist = args.toList
    type OptionMap = Map[Symbol, Any]

    def nextOption(map : OptionMap, list: List[String]) : OptionMap = {
      def isSwitch(s : String) = (s(0) == '-')
      list match {
        case Nil => map
        case "--use-yeah" :: tail => nextOption(map ++ Map('useyeah -> true), tail)
        case "--max-size" :: value :: tail =>
          nextOption(map ++ Map('maxsize -> value.toInt), tail)
        case "--min-size" :: value :: tail =>
          nextOption(map ++ Map('minsize -> value.toInt), tail)
        /*case string :: opt2 :: tail if isSwitch(opt2) =>
          nextOption(map ++ Map('infile -> string), list.tail)
        case string :: Nil =>  nextOption(map ++ Map('infile -> string), list.tail)*/
        case option :: tail => println("Unknown option "+option)
        exit(1)
      }
    }
    val options = nextOption(Map(),arglist)
    println(options)
  }
}
