package specification

trait Ordered[T] extends Equality[T]:

  // declared

  extension (lt: T) def `<`(rt: T): Boolean

  // defined

  extension (lt: T) def `<=`(rt: T): Boolean = lt `<` rt || lt `=` rt

  // laws

  trait OrderedLaws[L[_]: Law]:

    val reflexive: T => L[Boolean] = t =>
      {
        t `<=` t
      } `=` {
        true
      }

    val antiSymmetric: T => T => L[Boolean] = lt =>
      rt =>
        {
          lt `<=` rt `=` true `&` rt `<=` lt `=` true
        } `=>` {
          lt `=` rt `=` true
        }

    val transitive: T => T => T => L[Boolean] = lt =>
      mt =>
        rt =>
          {
            lt `<=` mt `=` true `&` mt `<=` rt `=` true
          } `=>` {
            lt `<=` rt `=` true
          }

  // laws

  trait TotallyOrderedLaws[L[_]: Law]:

    val stronglyConnected: T => T => L[Boolean] = lt =>
      rt =>
        {
          lt `<=` rt `|` rt `<=` lt
        } `=` {
          true
        }
