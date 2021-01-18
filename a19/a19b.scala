import scala.io.StdIn
import scala.collection.immutable.List

object main {

    val literalR = """(\d+): "(\w)"""".r
    val refR = """(\d+): (.+)""".r

    trait Rule
    case class Literal(c: Char) extends Rule
    case class Ref(opts: List[List[Int]]) extends Rule

    /**
      * Parse at given offset and return list of possible end offsets
      *
      * @param s
      * @param pos
      * @param ruleNum
      * @param rules
      * @return
      */
    def parse(s: String, pos: Int, ruleNum: Int, rules: Map[Int, Rule]): Set[Int] = {
        if (pos >= s.length) {
            // println(s"$pos >= $s.length ")
            return Set[Int]()
        }
        val rule = rules(ruleNum)
        val res = rule match {
            case Literal(c) => {
                if (pos < s.length && s.charAt(pos) == c) {
                    Set[Int](pos + 1)
                } else {
                    Set[Int]()
                }             
            }
            case Ref(options) => {
                options.foldLeft(Set[Int]())((acc, seq) => {
                    val newSet = {
                        seq.foldLeft(Set(pos))((prevOffsets, currentRuleNum) => {
                            prevOffsets.flatMap(o => parse(s, o, currentRuleNum, rules))
                        })
                    }
                    acc.concat(newSet)
                })
            }
        }
        res
    }

    def matches(s: String, rules: Map[Int, Rule]): Boolean = {
        val set = parse(s, 0, 0, rules)
        set.contains(s.length)
    }

    def solve1(lines: List[String]): Unit = {
        var rules = Map[Int, Rule]()
        val (ruleLines, textLines) = lines.span(s => !s.trim.isEmpty)
        
        ruleLines.foreach(l => {
            l match {
                case literalR(num, c) => rules = rules.updated(num.toInt, Literal(c.charAt(0)))
                case refR(num, rest) => {
                    val optionsStr = rest.split("""\|""").toList
                    // println(optionsStr)
                    val options = optionsStr.map(s => s.split(" ").toList.filter(s => !s.trim.isEmpty).map(_.toInt))
                    rules = rules.updated(num.toInt, Ref(options))
                }
            }
        })
        rules = rules.updated(8, Ref(List(List(42), List(42, 8))))
        rules = rules.updated(11, Ref(List(List(42, 31), List(42, 11, 31))))
    
        // println(rules)

        val validLines = textLines.filter(l => !l.isEmpty && matches(l, rules))
        println(validLines)
        println(validLines.length)
    }
    
    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        solve1(lines)
    }
}
