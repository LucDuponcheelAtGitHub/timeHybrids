package specification

trait Join[Z] extends Ordered[Z]:

  // declared

  extension (lz: Z) def ∨(rz: Z): Z

  trait JoinLaws[L[_]: Law]:

    val smallestGreaterThanBoth: Z => Z => Z => L[Boolean] =
      az =>
        lz =>
          rz =>
            {
              (lz <= (lz ∨ rz)) `=` true `&` (rz <= (lz ∨ rz)) `=` true
            } `&` {
              (lz <= az) `=` true `&` (rz <= az) `=` true
            } `=>` {
              (lz ∨ rz) <= az `=` true
            }
