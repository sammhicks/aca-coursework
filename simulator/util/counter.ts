import { mathMod } from "./math-mod";


export class Counter {
  private _value: number;

  constructor(readonly size: number, value: number = 0) {
    this._value = mathMod(value, size);
  }

  get value() {
    return this._value;
  }

  increment() {
    this._value = (this._value + 1) % this.size;
  }
}
