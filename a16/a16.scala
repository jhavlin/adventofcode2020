import scala.io.StdIn
import scala.collection.immutable.List

object main {

    case class Range(from: Long, to: Long) {
        def satisfies(v: Long): Boolean = v >= from && v <= to
    }
    case class Rule(name: String, ranges: List[Range]) {
        def satisfies(v: Long): Boolean = ranges.exists(_.satisfies(v))
    }

    def parseNumbers(s: String): List[Long] = {
        s.split(",").toList.map(_.toLong)
    }

    val ruleR = """(\w.+): (.*)""".r
    val rangeR = """(\d+)-(\d+)""".r

    def parseRule(s: String): Rule = {
        s match {
            case ruleR(name, rangeStr) => {
                val rangesRaw = rangeStr.split(" or ")
                val ranges = rangesRaw.map({ case rangeR(f, t) => Range(f.toLong, t.toLong) }).toList
                Rule(name, ranges)
            }
        }
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        var mode = "rules"
        var rules = List[Rule]()
        var myNumbers = List[Long]()
        var nearby = List[List[Long]]()

        lines.foreach((line) => {
            if (line == "") {
            } else if (mode == "rules") {
                if (line == "your ticket:") {
                    mode  = "my"
                } else {
                    rules = parseRule(line) +: rules
                }
            } else if (mode == "my") {
                myNumbers = parseNumbers(line)
                mode = "near"
            } else {
                if (line != "nearby tickets:") {
                    nearby = parseNumbers(line) +: nearby
                }
            }
        })

        var sum = 0L
        nearby.foreach(ticket => ticket.foreach(v => {
            if (!rules.exists(r => r.satisfies(v))) {
                sum += v
            }
        }))
        println("Sum of values invalid in all rules")
        println(sum)

        // remove invalid tickets
        nearby = nearby.filter(ticket => {
            ticket.forall(v => rules.exists(r => r.satisfies(v)))
        })

        var indexToValues = List[List[Long]]()
        for (i <- 0 until myNumbers.length) {
            val valuesAtIndex = nearby.map(t => t(i))
            indexToValues = valuesAtIndex +: indexToValues
        }
        indexToValues = indexToValues.reverse
        println(indexToValues)

        var indexToPossibleRules = indexToValues.map(values => rules.filter(rule => values.forall(rule.satisfies(_))))
        println(indexToPossibleRules.map(l => l.map(r => r.name)))

        var round = 0
        while (!indexToPossibleRules.forall(rules => rules.length == 1) && round < 1000000) {
            round += 1
            val determined = indexToPossibleRules.filter(rules => rules.length == 1).map(rules => rules(0))
            println("determined", determined)
            indexToPossibleRules = indexToPossibleRules.map(rules => {
                if (rules.length > 1) {
                    rules.filter(rule => !determined.contains(rule))
                } else {
                    rules
                }
            })
        }
        println(round)
        println(indexToPossibleRules.map(l => l.map(r => r.name)))

        val namesAndIndexes = indexToPossibleRules
            .map(list => list(0).name)
            .zipWithIndex.filter({ case (name, index) => name.startsWith("departure") })

        println(namesAndIndexes);

        val departureIndexes = namesAndIndexes.map(_._2)

        var prod = 1L
        departureIndexes.foreach(i => prod = prod * myNumbers(i))

        println(prod)
    }
}
