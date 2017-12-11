export function ReadOnlyError(): never {
  throw new Error("Readonly");
}
