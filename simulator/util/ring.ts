
export class RingItem<T extends RingItem<T>> {
  public next: T;

  static createRing<T extends RingItem<T>>(size: number, constructor: () => T): T {
    const first: T = constructor();

    var current: T = first;

    for (var i = 1; i < size; ++i) {
      current.next = constructor();
      current = current.next;
    }

    current.next = first;

    return first;
  }
}
