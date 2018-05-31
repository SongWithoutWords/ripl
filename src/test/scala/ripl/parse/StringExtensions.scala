package ripl.parse

object StringExtensions {
  implicit class richString(string: String) {
    def nl(rhs: String) = string + "\n" + rhs
  }
}
