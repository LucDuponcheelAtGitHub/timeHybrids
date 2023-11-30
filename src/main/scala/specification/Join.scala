package specification

trait Join[T: Ordered: Arbitrary]:

  // declared

  extension (lt: T) def ∨(rt: T): T

  // laws

  trait JoinLaws[L[_]: Law]:

    val at = summon[Arbitrary[T]].arbitrary

    val smallestGreaterThanBoth: T => T => L[Boolean] =
      lt =>
        rt =>
          {
            (lt `<=` (lt ∨ rt)) `=` true `&` (rt `<=` (lt ∨ rt)) `=` true
          } `&` {
            (lt `<=` at) `=` true `&` (rt `<=` at) `=` true
          } `=>` {
            (lt ∨ rt) `<=` at `=` true
          }
