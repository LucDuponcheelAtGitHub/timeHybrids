package specification

trait Supremum[Set[_]: Sets, T: Ordered: Arbitrary]:

  val sup: Function[Set[T], T]

  // laws

  trait SupremumLaws[L[_]: Law]:

    val sets: Sets[Set] = summon[Sets[Set]]

    import sets.{map, all}

    val at = summon[Arbitrary[T]].arbitrary

    val smallestGreaterThanAll: Set[T] => L[Boolean] =
      ts =>
        {
          all apply {
            for {
              t <- ts
            } yield t `<=` sup(ts) `=` true
          }
        } `&` {
          all apply {
            for {
              t <- ts
            } yield t `<=` at `=` true
          }
        } `=>` {
          sup(ts) `<=` at `=` true
        }

    given join: Join[T]

    val joinAsSupremum: Tuple2[T, T] => L[T] =
      val sets = summon[Sets[Set]]
      import sets.{set2}
      (lt, rt) =>
        {
          sup(set2(lt, rt))
        } `=` {
          lt ∨ rt
        }
