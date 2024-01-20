package specification

trait JoinComplete[Z] extends Join[Z]:

  // declared

  val V: Function[Set[Z], Z]

  // defined

  import utilities.set.{tuple2ToSet}

  extension (lz: Z) def ∨(rz: Z): Z = V(tuple2ToSet(lz, rz))

  trait SupremumLaws[L[_]: Law]:

    val law: Law[L] = summon[Law[L]]

    import law.{all}

    val smallestGreaterThanAll: Z => Set[Z] => L[Boolean] =
      az =>
        zs =>
          {
            all {
              for {
                z <- zs
              } yield z <= V(zs) `=` true
            }
          } `&` {
            all {
              for {
                z <- zs
              } yield z <= az `=` true
            }
          } `=>` {
            V(zs) <= az `=` true
          }
