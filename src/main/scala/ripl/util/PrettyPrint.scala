package ripl.util

object PrettyPrint {
  def apply(term: Any): String = {

    def printTerm(term: Any, indentation: Int): String =
      (term match {
        case s: String => "\"" + s + "\""
        case traversable: TraversableOnce[Any] =>
          printTraversable(traversable, indentation + 1)
        case product: Product => printProduct(product, indentation + 1)
        case other            => other.toString
      })

    def printTraversable[A](
        traversable: TraversableOnce[Any],
        indentation: Int
      ): String =
      "\n" + (" " * indentation) + "[ " + traversable
        .map { term =>
          printTerm(term, indentation + 1)
        }
        .mkString("\n" + (" " * indentation) + ", ") + "\n" + (" " * indentation) + "]"

    def printProduct(product: Product, indentation: Int): String =
      product.productPrefix + (product.productArity match {
        case 0 => ""
        case 1 => printTerm(product.productIterator.next, indentation)
        case _ => printTraversable(product.productIterator, indentation + 1)
      })

    printTerm(term, 0)
  }
}
