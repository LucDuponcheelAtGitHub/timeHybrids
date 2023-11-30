package specification

trait VirtualTopology[Collection[_]: Sets, T]
    extends Ordered[T],
      Meet[T],
      Join[T],
      Supremum[Collection, T]
