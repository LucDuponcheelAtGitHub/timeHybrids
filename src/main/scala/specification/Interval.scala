// package specification

// trait Interval[Z: Ordered] extends Set[Z]:

//   def begin: Function[Interval[Z], Z]

//   def end: Function[Interval[Z], Z]

//   extension (bz: Z) def `to`(ez: Z): Interval[Z]

//   val initialIntervals: Interval[Z] => Set[Interval[Z]] =
//     mi =>
//       val bm = begin(mi)
//       for {
//         em <- mi
//         if (bm <= em && em <= end(mi))
//       } yield {
//         bm `to` em
//       }

//   val subIntervals: Interval[Z] => Set[Interval[Z]] =
//     mi =>
//       for {
//         bm <- mi
//         em <- mi
//         if (begin(mi) <= bm &&
//           bm <= em &&
//           em <= end(mi))
//       } yield {
//         bm `to` em
//       }