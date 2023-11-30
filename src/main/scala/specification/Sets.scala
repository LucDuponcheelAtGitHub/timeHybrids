package specification

trait Sets[Collection[_]] extends MonadPlus[Collection]:

  // types

  type Collection0 = [Z] =>> Collection[Z]

  type Collection1 = [Z] =>> Collection[Z]

  type Collection2 = [Z] =>> Collection[Z]

  type Interval = [Z] =>> Collection[Z]

  // declared

  extension [Z](lc: Collection[Z]) def `=s=`(rc: Collection[Z]): Boolean

  extension [Z](lc: Collection[Z]) def `<s<`(rc: Collection[Z]): Boolean

  def tuple2[Z]: Collection2[Z] => Tuple2[Z, Z]

  // declared related to Ordered (apply needed at use site)

  def interval[Z: Ordered]: Option[Tuple2[Z, Z]] => Collection[Z]

  // declared related to Law (apply needed at use site)

  def all[Z, L[_]: Law]: Collection[L[Z]] => L[Z]

  // defined

  def collection0[Z]: Collection0[Z] = ζ

  def collection1[Z]: Z => Collection1[Z] = ν

  extension [Z](lc: Collection[Z])
    def ∪(rc: Collection[Z]): Collection[Z] =
      lc `+` rc

  def collection2[Z]: Tuple2[Z, Z] => Collection2[Z] = (l, r) =>
    collection1(l) ∪ collection1(r)

  def union[Z]: Collection[Collection[Z]] => Collection[Z] = μ

  extension [Z](lc: Collection[Z])
    def `<=s<=`(rc: Collection[Z]): Boolean = lc `<s<` rc || lc `=s=` rc

  // laws

  trait SetsLaws[L[_]: Law]:

    trait Collection2Related:

      def unordered[Z]: Tuple2[Z, Z] => L[Collection2[Z]] =
        (l, r) =>
          {
            collection2(l, r)
          } `=` {
            collection2(r, l)
          }

      import implementation.{functionCategory}

      def iso2[Z]: L[Function[Tuple2[Z, Z], Tuple2[Z, Z]]] = {
        identity[Tuple2[Z, Z]]
      } `=` {
        tuple2 `o` collection2
      }

      def osi2[Z]: L[Function[Collection2[Z], Collection2[Z]]] = {
        identity[Collection2[Z]]
      } `=` {
        collection2 `o` tuple2
      }

    trait EqualityRelated:

      def reflexive[Z]: Collection[Z] => L[Boolean] = c =>
        {
          c `=s=` c
        } `=` {
          true
        }

      def symmetric[Z]: Collection[Z] => Collection[Z] => L[Boolean] =
        lc =>
          rc =>
            {
              lc `=s=` rc `=` true
            } `=>` {
              rc `=s=` lc `=` true
            }

      def transitive[Z]
          : Collection[Z] => Collection[Z] => Collection[Z] => L[Boolean] =
        lc =>
          mc =>
            rc =>
              {
                { lc `=s=` mc `=` true } `&` { mc `=s=` rc `=` true }
              } `=>` {
                lc `=s=` rc `=` true
              }

    trait OrderedRelated:

      def reflexive[Z]: Collection[Z] => L[Boolean] = c =>
        {
          c `<=s<=` c
        } `=` {
          true
        }

      def antiSymmetric[Z]: Collection[Z] => Collection[Z] => L[Boolean] =
        lc =>
          rc =>
            {
              { lc `<=s<=` rc } `=` true `&` { rc `<=s<=` lc } `=` true
            } `=>` {
              lc `=s=` rc `=` true
            }

      def transitive[Z]
          : Collection[Z] => Collection[Z] => Collection[Z] => L[Boolean] =
        lc =>
          mc =>
            rc =>
              {
                { lc `<=s<=` mc } `=` true `&` { mc `<=s<=` rc } `=` true
              } `=>` {
                { lc `<=s<=` rc } `=` true
              }
