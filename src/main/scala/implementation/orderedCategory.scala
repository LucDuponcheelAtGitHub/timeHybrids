package implementation

import specification.{Arbitrary, Ordered, Sets, Category}

given orderedCategory[Collection[_]: Sets, T: Arbitrary: Ordered]
    : Category[[_, _] =>> Tuple2[T, T]] with

  type BTC = [_, _] =>> Tuple2[T, T]

  extension [Z, Y, X](yμx: BTC[Y, X])
    def `o`(zμy: BTC[Z, Y]): BTC[Z, X] =
      (yμx, zμy) match
        case ((llt, lrt), (rlt, rrt)) =>
          require(llt `<=` lrt && lrt == rlt && rlt `<=` rrt)
          (llt, rrt)

  def ι[Z]: BTC[Z, Z] =
    val at = summon[Arbitrary[T]].arbitrary
    (at, at)

// ............................

//   import specification.{Proof}

//   trait OrderedTransitionProofs[P[_]: Proof]:

//     val proof = summon[Proof[P]]
//     import proof.{qed}

//     def leftIdentity1[Z, Y]: OrderedTransition => P[OrderedTransition] =
//       zμy =>
//         identity `o` zμy `=:`
//           // definition of identity
//           One(o, o) `o` zμy `=:`
//           // definition of `o`
//           {
//             (One(o, o), zμy) match
//               case (One(_, _), One(_, _)) =>
//                 require(o `=` value(zμy)._1)
//                 One(o, value(zμy)._2)
//               case _ =>
//                 Zero(o, value(zμy)._2)
//           } `=:`
//           // simplification
//           {
//             zμy match
//               case One(_, _) =>
//                 require(o `=` value(zμy)._1)
//                 One(o, value(zμy)._2)
//               case Zero(_, _) =>
//                 Zero(o, value(zμy)._2)
//           } `=:`
//           // first case
//           One(o, value(zμy)._2) `=:`
//           //  o `=` value(zμy)._1
//           One(value(zμy)._1, value(zμy)._2) `=:`
//           // trivial
//           zμy `=:`
//           // done
//           qed

//     def leftIdentity2[Z, Y]: OrderedTransition => P[OrderedTransition] =
//       zμy =>
//         identity `o` zμy `=:`
//           // definition of identity
//           One(o, o) `o` zμy `=:`
//           // definition of `o`
//           {
//             (One(o, o), zμy) match
//               case (One(_, _), One(_, _)) =>
//                 require(o `=` value(zμy)._1)
//                 One(o, value(zμy)._2)
//               case _ =>
//                 Zero(o, value(zμy)._2)
//           } `=:`
//           // simplification
//           {
//             zμy match
//               case One(_, _) =>
//                 require(o `=` value(zμy)._1)
//                 One(o, value(zμy)._2)
//               case Zero(_, _) =>
//                 Zero(o, value(zμy)._2)
//           } `=:`
//           // second case
//           Zero(o, value(zμy)._2) `=:`
//           //  ordered0
//           Zero(value(zμy)._1, value(zμy)._2) `=:`
//           // trivial
//           zμy `=:`
//           // done
//           qed

//     def rightIdentity1[Z, Y]: OrderedTransition => P[OrderedTransition] =
//       yμx =>
//         yμx `o` identity `=:`
//           // definition of identity
//           yμx `o` One(o, o) `=:`
//           // definition of `o`
//           {
//             (yμx, One(o, o)) match
//               case (One(_, _), One(_, _)) =>
//                 require(value(yμx)._2 `=` o)
//                 One(value(yμx)._2, o)
//               case _ =>
//                 Zero(value(yμx)._2, o)
//           } `=:`
//           // simplification
//           {
//             yμx match
//               case One(_, _) =>
//                 require(value(yμx)._2 `=` o)
//                 One(value(yμx)._1, o)
//               case _ =>
//                 Zero(value(yμx)._1, o)
//           } `=:`
//           // first case
//           One(value(yμx)._1, o) `=:`
//           // value(yμx)._2 `=` o
//           One(value(yμx)._1, value(yμx)._2) `=:`
//           // trivial
//           yμx `=:`
//           // done
//           qed

//     def rightIdentity2[Z, Y]: OrderedTransition => P[OrderedTransition] =
//       yμx =>
//         yμx `o` identity `=:`
//           // definition of identity
//           yμx `o` One(o, o) `=:`
//           // definition of `o`
//           {
//             (yμx, One(o, o)) match
//               case (One(_, _), One(_, _)) =>
//                 require(value(yμx)._2 `=` o)
//                 One(value(yμx)._2, o)
//               case _ =>
//                 Zero(value(yμx)._2, o)
//           } `=:`
//           // simplification
//           {
//             yμx match
//               case One(_, _) =>
//                 require(value(yμx)._2 `=` o)
//                 One(value(yμx)._1, o)
//               case _ =>
//                 Zero(value(yμx)._1, o)
//           } `=:`
//           // second case
//           Zero(value(yμx)._1, o) `=:`
//           // ordered0
//           Zero(value(yμx)._1, value(yμx)._2) `=:`
//           // trivial
//           yμx `=:`
//           // done
//           qed

//     def associativity[Z, Y, X, W]: OrderedTransition => OrderedTransition[
//       T
//     ] => OrderedTransition => P[
//       OrderedTransition
//     ] =
//       yμx =>
//         mor =>
//           zμy =>
//             (yμx `o` mor) `o` zμy `=:`
//               // definition `o`
//               {
//                 val lmor: OrderedTransition = (yμx, mor) match
//                   case (One(_, _), One(_, _)) =>
//                     require(value(yμx)._2 `=` value(mor)._1)
//                     One(value(yμx)._1, value(mor)._2)
//                   case _ =>
//                     Zero(value(yμx)._1, value(mor)._2)
//                 lmor `o` zμy
//               } `=:`
//               // definition `o`
//               {
//                 val lmor = (yμx, mor) match
//                   case (One(_, _), One(_, _)) =>
//                     require(value(yμx)._2 `=` value(mor)._1)
//                     One(value(yμx)._1, value(mor)._2)
//                   case _ =>
//                     Zero(value(yμx)._1, value(mor)._2)
//                 (lmor, zμy) match
//                   case (One(_, _), One(_, _)) =>
//                     require(value(lmor)._2 `=` value(zμy)._1)
//                     One(value(lmor)._1, value(zμy)._2)
//                   case _ =>
//                     Zero(value(lmor)._1, value(zμy)._2)
//               } `=:`
//               // merging
//               {
//                 ((yμx, mor), zμy) match
//                   case ((One(_, _), One(_, _)), One(_, _)) =>
//                     require(value(yμx)._2 `=` value(mor)._1)
//                     require(value(mor)._2 `=` value(zμy)._1)
//                     One(value(yμx)._1, value(zμy)._2)
//                   case _ =>
//                     Zero(value(yμx)._1, value(zμy)._2)
//               } `=:`
//               // trivial
//               {
//                 (yμx, (mor, zμy)) match
//                   case (One(_, _), (One(_, _), One(_, _))) =>
//                     require(value(mor)._2 `=` value(zμy)._1)
//                     require(value(yμx)._2 `=` value(zμy)._1)
//                     One(value(yμx)._1, value(zμy)._2)
//                   case _ =>
//                     Zero(value(yμx)._1, value(zμy)._2)
//               } `=:`
//               // merging
//               {
//                 val mror = (mor, zμy) match
//                   case (One(_, _), One(_, _)) =>
//                     require(value(mor)._2 `=` value(zμy)._1)
//                     One(value(mor)._1, value(zμy)._2)
//                   case _ =>
//                     Zero(value(mor)._1, value(zμy)._2)
//                 (yμx, mror) match
//                   case (One(_, _), One(_, _)) =>
//                     require(value(yμx)._2 `=` value(mror)._1)
//                     One(value(yμx)._1, value(mror)._2)
//                   case _ =>
//                     Zero(value(yμx)._1, value(mror)._2)
//               } `=:`
//               // definition `o`
//               {
//                 val mror: OrderedTransition = (mor, zμy) match
//                   case (One(_, _), One(_, _)) =>
//                     require(value(mor)._2 `=` value(zμy)._1)
//                     One(value(mor)._1, value(zμy)._2)
//                   case _ =>
//                     Zero(value(mor)._1, value(zμy)._2)
//                 yμx `o` mror
//               } `=:`
//               //
//               yμx `o` (mor `o` zμy) `=:`
//               // done
//               qed
