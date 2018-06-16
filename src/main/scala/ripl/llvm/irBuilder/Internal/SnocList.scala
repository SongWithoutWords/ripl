package ripl.llvm.irBuilder.Internal

case class SnocList[A](unSnocList: List[A]) {
  def snoc(a: A) = SnocList(a :: unSnocList)

  def getSnocList() = unSnocList.reverse
}

// instance Semigroup (SnocList a) where
//   SnocList xs <> SnocList ys = SnocList $ ys ++ xs

// instance Monoid (SnocList a) where
// #if !(MIN_VERSION_base(4,11,0))
//   mappend = (<>)
// #endif
//   mempty = SnocList []
