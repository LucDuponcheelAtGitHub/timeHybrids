package specification

trait Supremum[Collection[_]: Sets, T: Ordered: Arbitrary]:

  val sup: Function[Collection[T], T]

  // laws

  trait SupremumLaws[L[_]: Law]:

    val sets: Sets[Collection] = summon[Sets[Collection]]

    import sets.{map, all}

    val at = summon[Arbitrary[T]].arbitrary

    val smallestGreaterThanAll: Collection[T] => L[Boolean] =
      c =>
        {
          all apply {
            for {
              t <- c
            } yield t `<=` sup(c) `=` true
          }
        } `&` {
          all apply {
            for {
              t <- c
            } yield t `<=` at `=` true
          }
        } `=>` {
          sup(c) `<=` at `=` true
        }

    given join: Join[T]

    val joinAsSupremum: Tuple2[T, T] => L[T] =
      val sets = summon[Sets[Collection]]
      import sets.{collection2}
      (lt, rt) =>
        {
          sup(collection2(lt, rt))
        } `=` {
          lt ∨ rt
        }
