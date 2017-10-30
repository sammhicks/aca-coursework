import { mathMod } from "./math-mod";


export class Counter {
  private _size: number;
  private _value: number;

  constructor(size: number, value: number = 0) {
    this._size = size;
    this._value = mathMod(value, size);
  }

  get value() {
    return this._value;
  }

  increment() {
    this._value = (this._value + 1) % this._size;
  }
}
