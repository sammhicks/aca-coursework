export function thousandsMarker(n: number): string {
  return (n < 1000) ? n.toString() : (thousandsMarker(Math.floor(n / 1000)) + "," + zeroPad(n % 1000, 3));
}

function zeroPad(n: number, pad: number) {
  const strN = n.toString();

  return "0".repeat(Math.max(0, pad - strN.length)) + strN;
}
