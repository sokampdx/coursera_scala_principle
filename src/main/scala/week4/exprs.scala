package week4

object exprs {
  def show(e: Expr) = e match {
    case Number(x) => x.toString
    case Sum(l, r) => show(l) + " + " + show(r)
  }
}
