import { Counter } from "./counter";
import { DeepCopy } from "./deep-copy";

export class Semaphore extends Counter implements DeepCopy<Semaphore> {
  constructor(size: number, value: number = 0) {
    super(size, value);
  }

  equals(other: Semaphore) {
    return this.value == other.value;
  }

  deepCopy() { return new Semaphore(this.size, this.value); }
}
