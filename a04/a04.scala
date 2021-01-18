import scala.io.StdIn
import scala.collection.immutable.List

object main {

    val required = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    val optional = List("cid")

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        val lineGroups = lines.mkString("\n").split("\n\n").toList.map(s => s.replaceAll("\n", " "))
     
        val  valids = lineGroups.filter(isValid)
        println(valids.length)

        val valids2 = lineGroups.filter(isValid2)
        println(valids2.length)
    }

    def isValid(str: String): Boolean = {
        val infos = str.split(" ").toList
        val fields = infos.map(i => i.split(":")(0))
        required.forall(r => fields.contains(r))
    }

    def isValid2(str: String): Boolean = {
        val infos = str.split(" ").toList
        val fieldsAndValues = infos.map(i => i.split(":").toList)
        required.forall(r =>
            isValidValue(r, fieldsAndValues.find(fv => fv(0) == r).getOrElse(List("", ""))(1))
        )
    }

    def isValidValue(field: String, value: String): Boolean = {
        val res = field match {
            case "byr" => { """\d\d\d\d""".r.matches(value) && { val i = value.toInt; i >= 1920 && i <= 2002 } } 
            case "iyr" => { """\d\d\d\d""".r.matches(value) && { val i = value.toInt; i >= 2010 && i <= 2020 } }
            case "eyr" => { """\d\d\d\d""".r.matches(value) && { val i = value.toInt; i >= 2020 && i <= 2030 }}
            case "hgt" => {
                val cm = """(\d\d\d)cm""".r
                val in = """(\d\d)in""".r
                value match {
                    case cm(v) => { val i = v.toInt; i >= 150 && i <= 193 }
                    case in(v) => { val i = v.toInt; i >= 59 && i <= 76}
                    case _ => false
                }
            }
            case "hcl" => { """#[0-9a-f]{6}""".r.matches(value) }
            case "ecl" => { Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(value)}
            case "pid" => { """\d{9}""".r.matches(value) }
            case _ => false
        }
        if (!res) {
            println("invalid", field, value)
        }
        res
    }
}
