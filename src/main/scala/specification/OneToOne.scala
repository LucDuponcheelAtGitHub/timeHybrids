package specification

trait OneToOne[Z, Y] extends Isomorphism[Function, Z, Y]:

  val fromAll: Function[Set[Z], Set[Y]] =
    zs =>
      for {
        z <- zs
      } yield {
        from(z)
      }

  val toAll: Function[Set[Y], Set[Z]] =
    ys =>
      for {
        y <- ys
      } yield {
        to(y)
      }
