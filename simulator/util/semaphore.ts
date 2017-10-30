import { Counter } from "./counter";

export class Semaphore extends Counter {
  constructor(size: number) {
    super(size);
  }

  equals(other: Semaphore) {
    return this.value == other.value;
  }
}
