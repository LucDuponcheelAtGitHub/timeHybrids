package specification

trait Ordered[Z]:

  // declared

  extension (lz: Z) def <(rz: Z): Boolean

  // defined

  extension (lz: Z) def <=(rz: Z): Boolean = lz < rz || lz == rz

  // nested `trait`s

  type Interval = Set[Z]

  trait IntervalUtilities:

    extension (b: Z) def to(e: Z): Interval

    def begin: Function[Interval, Z]

    def end: Function[Interval, Z]

    val initialIntervals: Function[Interval, Set[Interval]] =
      i =>
        val b = begin(i)
        for {
          e <- i
          if (b <= e && e <= end(i))
        } yield {
          b `to` e
        }

    val subIntervals: Function[Interval, Set[Interval]] =
      i =>
        for {
          b <- i
          e <- i
          if (begin(i) <= b &&
            b <= e &&
            e <= end(i))
        } yield {
          b `to` e
        }

    // laws

    trait IntervalUtilitiesLaws[L[_]: Law]:

      val beginToEnd: Interval => L[Interval] = i =>
        {
          i
        } `=` {
          begin(i) `to` end(i)
        }

  // laws

  trait OrderedLaws[L[_]: Law]:

    val reflexive: Z => L[Boolean] = z =>
      {
        z <= z
      } `=` {
        true
      }

    val antiSymmetric: Z => Z => L[Boolean] = lz =>
      rz =>
        {
          lz <= rz `=` true `&` rz <= lz `=` true
        } `=>` {
          lz == rz `=` true
        }

    val transitive: Z => Z => Z => L[Boolean] = lz =>
      mz =>
        rz =>
          {
            lz <= mz `=` true `&` mz <= rz `=` true
          } `=>` {
            lz <= rz `=` true
          }

  trait TotallyOrderedLaws[L[_]: Law]:

    val stronglyConnected: Z => Z => L[Boolean] = lz =>
      rz =>
        {
          lz <= rz `|` rz <= lz
        } `=` {
          true
        }
