package specification

trait Equality[T]:

  extension (lt: T) def `=`(rt: T): Boolean

  // laws

  trait EqualityLaws[L[_]: Law]:

    val reflexive: T => L[Boolean] = t =>
      {
        t `=` t
      } `=` {
        true
      }

    val symmetric: T => T => L[Boolean] = lt =>
      rt =>
        {
          lt `=` rt `=` true
        } `=>` {
          rt `=` lt `=` true
        }

    val transitive: T => T => T => L[Boolean] = lt =>
      mt =>
        rt =>
          {
            lt `=` mt `=` true `&` mt `=` rt `=` true
          } `=>` {
            lt `=` rt `=` true
          }
