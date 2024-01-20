// package specification

// trait Equality[Z]:

//   extension (lz: Z) def `=`(rz: Z): Boolean

//   // laws

//   trait EqualityLaws[L[_]: Law]:

//     val reflexive: Z => L[Boolean] = z =>
//       {
//         z `=` z
//       } `=` {
//         true
//       }

//     val symmetric: Z => Z => L[Boolean] = lz =>
//       rz =>
//         {
//           lz `=` rz `=` true
//         } `=>` {
//           rz `=` lz `=` true
//         }

//     val transitive: Z => Z => Z => L[Boolean] = lz =>
//       mz =>
//         rz =>
//           {
//             lz `=` mz `=` true `&` mz `=` rz `=` true
//           } `=>` {
//             lz `=` rz `=` true
//           }
