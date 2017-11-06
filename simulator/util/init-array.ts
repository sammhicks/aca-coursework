
export function initArray<T>(size: number, initSingle: () => T): T[] { return Array.apply(null, Array(size)).map(initSingle); }
