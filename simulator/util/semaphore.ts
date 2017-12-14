import { Comparable, compare } from "./compare";
import { DeepCopy } from "./deep-copy";

export class Semaphore implements Comparable<Semaphore>, DeepCopy<Semaphore> {
  private _value: number;

  constructor(value: number = 0) {
    this._value = value;
  }

  compare(other: Semaphore) { return compare(this._value, other._value); }

  increment() { this._value += 1; }

  deepCopy() { return new Semaphore(this._value); }
}
