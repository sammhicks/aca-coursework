import { Comparable, compare } from "./compare";

export class Semaphore implements Comparable<Semaphore>{
  protected _value: number;

  constructor(value: number = 0) {
    this._value = value;
  }

  compare(other: Semaphore) { return compare(this._value, other._value); }

  increment() { this._value += 1; }

  reset() { this._value = 0; }
}

export class BiDirectionSemaphore extends Semaphore {
  decrement() { this._value -= 1; }

  isZero() { return this._value == 0; }
}
