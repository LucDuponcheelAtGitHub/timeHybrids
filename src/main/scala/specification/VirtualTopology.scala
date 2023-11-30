package specification

trait VirtualTopology[Set[_]: Sets, T]
    extends Ordered[T],
      Meet[T],
      Join[T],
      Supremum[Set, T]
