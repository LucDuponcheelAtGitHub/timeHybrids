package specification

trait NcMeet[Z] extends Ordered[Z]:

  // declared

  extension (lz: Z) def ∧(rz: Z): Z

  trait MeetLaws[L[_]: Law]:

    val greatestSmallerThanBoth: Z => Z => Z => L[Boolean] =
      az =>
        lz =>
          rt =>
            {
              ((lz ∧ rt) <= lz) `=` true `&` ((lz ∧ rt) <= rt) `=` true
            } `&` {
              (az <= lz) `=` true `&` (az <= rt) `=` true
            } `=>` {
              az <= (lz ∧ rt) `=` true
            }
