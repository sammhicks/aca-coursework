export enum Comparison {
  LessThen = -1,
  Equals = 0,
  GreaterThan = 1
}

export function compare(a: number, b: number): Comparison {
  if (a < b) return Comparison.LessThen;
  if (a > b) return Comparison.GreaterThan;
  return Comparison.Equals;
}

export function areEqual<T extends Comparable<T>>(a: T, b: T) {
  return a.compare(b) == Comparison.Equals;
}

export interface Comparable<T extends Comparable<T>> {
  compare(other: T): Comparison;
}
