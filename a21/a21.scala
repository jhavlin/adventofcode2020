import scala.io.StdIn
import scala.collection.immutable.List

object main {

    val foodR = """(.+) \(contains (.+)\)""".r

    def solve(lines: List[String]): Unit = {
        var allIngredients = Set[String]()
        var allergenToIngredientList = Map[String, List[Set[String]]]()
        var feed = List[Set[String]]()
        lines.foreach(l => {
            l match {
                case foodR(inStr, alStr) => {
                    val ingredients = inStr.split(" ").toSet
                    val allergens = alStr.split(", ").toSet
                    feed +:= ingredients
                    allIngredients ++= ingredients
                    allergens.foreach(a => {
                        allergenToIngredientList = allergenToIngredientList.updated(a, ingredients +: allergenToIngredientList.getOrElse(a, List()))
                    })
                }
            }
        })
        val allergenToPossible = allergenToIngredientList.mapValues(l => l.reduce(_.intersect(_))).toMap
        val allPossible = allergenToPossible.values.reduce(_ ++ _)
        val without = allIngredients.filter(i => !allPossible.contains(i))

        println("all ingredients")
        println(allIngredients)
        println("allergen to ingredient list")
        println(allergenToIngredientList)
        println("allerget to possible")
        println(allergenToPossible)
        println("all possible")
        println(allPossible)
        println("without")
        println(without)
        println("count")
        println(feed.foldLeft(0)((count, ingredients) => count + ingredients.filter(i => without.contains(i)).size))

        var mapping = allergenToPossible
        while (mapping.values.filter(s => s.size > 1).size > 0) {
            val determined = mapping.values.filter(s => s.size == 1).toList.reduce(_ ++ _)
            mapping = mapping.mapValues(s => {
                if (s.size == 1) {
                    s
                } else {
                    s -- determined
                }
            }).toMap
        }
        println("mapping")
        println(mapping)

        val pairs = mapping.toList.map({ case (k, v) => (k, v.toList(0)) }).sortBy(_._1).map(_._2).mkString(",")

        println("pairs")
        println(pairs)
    }

    def main(args: Array[String]): Unit = {
        val lines = io.Source.stdin.getLines().toList
        solve(lines)
    }
}
