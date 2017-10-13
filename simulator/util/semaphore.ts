
export class Semaphore {
  private value: number

  constructor(private size: number) {
    this.value = 0;
  }

  equals(other: Semaphore) {
    return this.value == other.value;
  }

  increment() {
    this.value = (this.value + 1) % this.size;
  }
}
