import fs from "fs";

const data = fs
  .readFileSync("in")
  .toString()
  .trim()
  .split("\n")
  .map((line) =>
    line
      .split(" | ")
      .map((sequenceStr) =>
        sequenceStr.split(" ").map((signal) => new Set(signal))
      )
  );

const diff = (a, b) => new Set([...a].filter((el) => !b.has(el)));

const equal = (a, b) => a.size === b.size && [...a].every((el) => b.has(el));

/** @param {Set<number>[]} signals  */
function mapSignalsToDigits(signals) {
  const digitBySignal = new Map();
  const signalByDigit = new Map();
  const addMapping = (signal, digit) => {
    digitBySignal.set(signal, digit);
    signalByDigit.set(digit, signal);
  };

  const digitBySegmentsCount = {
    2: 1,
    3: 7,
    4: 4,
    7: 8,
  };

  const segmentCountByDigit = {
    0: 6,
    1: 2,
    2: 5,
    3: 5,
    4: 4,
    5: 5,
    6: 6,
    7: 3,
    8: 7,
    9: 6,
  };

  // find 1 4 7 8
  for (const signal of signals) {
    const digit = digitBySegmentsCount[signal.size];
    if (digit != null) {
      addMapping(signal, digit);
    }
  }

  // find 9
  addMapping(
    signals.find((signal) => {
      const requiredSegmentCount = segmentCountByDigit[9];
      const requiredSegments = new Set([
        ...signalByDigit.get(4),
        ...signalByDigit.get(7),
      ]);
      return (
        signal.size === requiredSegmentCount &&
        diff(requiredSegments, signal).size === 0
      );
    }),
    9
  );

  // find 0
  addMapping(
    signals.find((signal) => {
      const requiredSegmentCount = segmentCountByDigit[0];
      const requiredSegments = signalByDigit.get(7)
      return (
        signal.size === requiredSegmentCount &&
        diff(requiredSegments, signal).size === 0 &&
        !equal(signal, signalByDigit.get(9))
      );
    }),
    0
  );

  // find 6
  addMapping(
    signals.find((signal) => {
      const requiredSegmentCount = segmentCountByDigit[6];
      return (
        signal.size === requiredSegmentCount &&
        !(
          equal(signal, signalByDigit.get(9)) ||
          equal(signal, signalByDigit.get(0))
        )
      );
    }),
    6
  );

  // find 2
  addMapping(
    signals.find((signal) => {
      const requiredSegmentCount = segmentCountByDigit[2];
      return (
        signal.size === requiredSegmentCount &&
        diff(signalByDigit.get(4), signal).size === 2
      );
    }),
    2
  );

  // find 3
  addMapping(
    signals.find((signal) => {
      const requiredSegmentCount = segmentCountByDigit[3];
      return (
        signal.size === requiredSegmentCount &&
        diff(signalByDigit.get(2), signal).size === 1
      );
    }),
    3
  );

  // find 5
  addMapping(
    signals.find((signal) => {
      const requiredSegmentCount = segmentCountByDigit[5];
      return (
        signal.size === requiredSegmentCount &&
        !(
          equal(signal, signalByDigit.get(2)) ||
          equal(signal, signalByDigit.get(3))
        )
      );
    }),
    5
  );

  return digitBySignal;
}

const sum = data.reduce((acc, [signals, outputs]) => {
  const digitBySignal = mapSignalsToDigits(signals);
  const digits = outputs.map((output) => {
    for (const [signal, digit] of digitBySignal.entries()) {
      if (equal(signal, output)) {
        return digit;
      }
    }
  });
  const outputValue = parseInt(digits.join(""), 10);
  return acc + outputValue;
}, 0);

console.log(sum);
