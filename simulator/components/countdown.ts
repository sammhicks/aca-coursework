import { ClockDriven } from "./clock-driven";

export abstract class Countdown extends ClockDriven {
  private _remaining: number;

  constructor() {
    super();
    this._remaining = 0;
  }

  protected reset(duration: number): void {
    this._remaining = duration;
  }

  protected abstract onCompletion(): void;


  public completed() {
    return this._remaining == 0;
  }

  public tick(): void {
    if (this.completed()) {
      return;
    }

    --this._remaining;

    if (this.completed()) {
      this.onCompletion();
    }
  }
}
