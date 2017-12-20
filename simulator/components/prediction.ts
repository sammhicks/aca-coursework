import { PC } from "./basic-types";

export class ValuePrediction<T>{
  private _certain: boolean;
  private _value: T;

  constructor(value: T) {
    this._certain = false;
    this._value = value;
  }

  get value() { return this._value; }

  set value(newValue: T) {
    if (this._value == newValue) {
      this._certain = true;
    } else if (this._certain) {
      this._certain = false;
    } else {
      this._value = newValue;
    }
  }
}

export class PredictedValues<T> {
  private _predictions: { [index: number]: ValuePrediction<T> };

  constructor() {
    this._predictions = {};
  }

  lookupValue(index: number, valueIfMissing: T) { return (index in this._predictions) ? this._predictions[index].value : valueIfMissing; }

  updateValue(index: number, value: T) {
    if (index in this._predictions) {
      this._predictions[index].value = value;
    } else {
      this._predictions[index] = new ValuePrediction(value);
    }
  }
}

export class Prediction {
  public branchPrediction: PredictedValues<boolean>;
  public returnPrediction: PredictedValues<PC>;

  constructor() {
    this.branchPrediction = new PredictedValues<boolean>();
    this.returnPrediction = new PredictedValues<PC>();
  }
}
