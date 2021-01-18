import scala.io.StdIn
import scala.collection.immutable.List

object main {

    val literalR = """(\d+): "(\w)"""".r
    val refR = """(\d+): (.+)""".r

    trait Rule
    case class Literal(c: Char) extends Rule
    case class Ref(opts: List[List[Int]]) extends Rule

    def parse(s: String, pos: Int, ruleNum: Int, rules: Map[Int, Rule]): (Boolean, Int) = {
        if (pos >= s.length) {
            // println(s"$pos >= $s.length ")
            return (false, -1)
        }
        val rule = rules(ruleNum)
        val res = rule match {
            case Literal(c) => {
                if (pos < s.length && s.charAt(pos) == c) {
                    (true, pos + 1)
                } else {
                    (false, -1)
                }             
            }
            case Ref(options) => {
                options.sortBy(_.length).reverse.foldLeft((false, -1))((lastResult, seq) => {
                    if (lastResult._1 /* && (ruleNum != 0 || lastResult._2 == s.length)*/ ) {
                        lastResult
                    } else {
                        seq.foldLeft((true, pos))((prev, currentRuleNum) => {
                            if (prev._1) {
                                parse(s, prev._2, currentRuleNum, rules)
                            } else {
                                prev
                            }
                        })
                    }
                })
            }
        }
        // println(s"pos = $pos, ruleNum = $ruleNum, matched = ${if (res._1) s.substring(pos, res._2) else ""}")
        res
    }

    def matches(s: String, rules: Map[Int, Rule]): Boolean = {
        val (valid, pos) = parse(s, 0, 0, rules)
        // println(valid, pos)
        valid && pos == s.length
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
