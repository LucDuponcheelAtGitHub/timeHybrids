package specification

trait Sets[UTC[_]] extends MonadPlus[UTC]:

  // types

  type Set = [Z] =>> UTC[Z]

  type Set0 = [Z] =>> Set[Z]

  type Set1 = [Z] =>> Set[Z]

  type Set2 = [Z] =>> Set[Z]

  type Interval = [Z] =>> Set[Z]

  // declared

  extension [Z](l: Set[Z]) def `=s=`(r: Set[Z]): Boolean

  extension [Z](l: Set[Z]) def `<s<`(r: Set[Z]): Boolean

  def tuple2[Z]: Set2[Z] => Tuple2[Z, Z]

  def interval[Z: Ordered]: Tuple2[Z, Z] => Set[Z]

  def all[Z, L[_]: Law]: Set[L[Z]] => L[Z]

  // defined

  def set0[Z]: Set0[Z] = ζ

  def set1[Z]: Z => Set1[Z] = ν

  extension [Z](l: Set[Z]) def ∪(r: Set[Z]): Set[Z] = l `+` r

  def set2[Z]: Tuple2[Z, Z] => Set2[Z] = (l, r) => set1(l) ∪ set1(r)

  def union[Z]: Set[Set[Z]] => Set[Z] = μ

  extension [Z](l: Set[Z])
    def `<=s<=`(r: Set[Z]): Boolean = l `<s<` r || l `=s=` r

  // laws

  trait SetsLaws[L[_]: Law]:

    trait Set2Related:

      def unordered[Z]: Tuple2[Z, Z] => L[Set2[Z]] =
        (l, r) =>
          {
            set2(l, r)
          } `=` {
            set2(r, l)
          }

      import implementation.{functionCategory}

      def iso2[Z]: L[Function[Tuple2[Z, Z], Tuple2[Z, Z]]] = {
        identity[Tuple2[Z, Z]]
      } `=` {
        tuple2 `o` set2
      }

      def osi2[Z]: L[Function[Set2[Z], Set2[Z]]] = {
        identity[Set2[Z]]
      } `=` {
        set2 `o` tuple2
      }

    trait EqualityRelated:

      def reflexive[Z]: Set[Z] => L[Boolean] = s =>
        {
          s `=s=` s
        } `=` {
          true
        }

      def symmetric[Z]: Set[Z] => Set[Z] => L[Boolean] =
        l =>
          r =>
            {
              l `=s=` r `=` true
            } `=>` {
              r `=s=` l `=` true
            }

      def transitive[Z]: Set[Z] => Set[Z] => Set[Z] => L[Boolean] =
        l =>
          m =>
            r =>
              {
                { l `=s=` m `=` true } `&` { m `=s=` r `=` true }
              } `=>` {
                l `=s=` r `=` true
              }

    trait OrderedRelated:

      def reflexive[Z]: Set[Z] => L[Boolean] = s =>
        {
          s `<=s<=` s
        } `=` {
          true
        }

      def antiSymmetric[Z]: Set[Z] => Set[Z] => L[Boolean] =
        l =>
          r =>
            {
              { l `<=s<=` r } `=` true `&` { r `<=s<=` l } `=` true
            } `=>` {
              l `=s=` r `=` true
            }

      def transitive[Z]: Set[Z] => Set[Z] => Set[Z] => L[Boolean] =
        l =>
          m =>
            r =>
              {
                { l `<=s<=` m } `=` true `&` { m `<=s<=` r } `=` true
              } `=>` {
                { l `<=s<=` r } `=` true
              }
