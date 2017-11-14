
export interface DeepCopy<T extends DeepCopy<T>> {
  deepCopy(): T
}

export function deepCopyArray<T extends DeepCopy<T>>(items: T[]) {
  return items.map(i => i.deepCopy());
}
