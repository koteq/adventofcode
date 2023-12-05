import fs from "node:fs";

function main(input_file = "in") {
  const [seedRangesData, ...rangeMapsData] = fs
    .readFileSync(input_file)
    .toString()
    .trim()
    .split("\n\n");
  const seedRanges = Array.from(seedRangesData.matchAll(/(\d+) (\d+)/g)).map(
    ([_, rangeStart, length]) => [Number(rangeStart), Number(length)]
  );
  const rangeMaps = rangeMapsData.map((data) => new RangeMap(data));
  const almanac = new Almanac(rangeMaps);
  const locationRanges = seedRanges.flatMap((seedRange) =>
    almanac.mapSeedRangeToLocationRange(seedRange)
  );
  const answer = Math.min(
    ...locationRanges.map(([locationStart]) => locationStart)
  );
  console.log(answer);
}

class Almanac {
  constructor(rangeMaps) {
    this.rangeMaps = rangeMaps;
  }

  mapSeedRangeToLocationRange(seedRange) {
    let resultRanges = [seedRange];
    for (const rangeMap of this.rangeMaps) {
      resultRanges = resultRanges.flatMap((range) => rangeMap.map(range));
    }
    return resultRanges;
  }
}

class RangeMap {
  constructor(data) {
    const [, id, rangeData] = data.match(/([\w-]+) map:\n(.*)/s);
    this.id = id;
    this.data = rangeData
      .split("\n")
      .map((line) => Array.from(line.matchAll(/\d+/g)))
      .map((matches) => matches.map(([match]) => Number(match)));
  }

  map(range) {
    let inputRanges = [range];
    let outputRanges = [];
    for (const [destStart, sourceStart, mapLen] of this.data) {
      const sourceEnd = sourceStart + mapLen - 1;
      inputRanges = inputRanges.flatMap(([rangeStart, rangeLen]) => {
        const rangeEnd = rangeStart + rangeLen - 1;
        if (
          //  rrrr
          // ssssss
          sourceStart <= rangeStart &&
          rangeEnd <= sourceEnd
        ) {
          outputRanges.push([destStart + (rangeStart - sourceStart), rangeLen]);
          return [];
        } else if (
          // rrrr
          //   ssss
          rangeStart <= sourceStart &&
          sourceStart <= rangeEnd &&
          rangeEnd <= sourceEnd
        ) {
          const overlapLen = rangeEnd - sourceStart + 1;
          outputRanges.push([destStart, overlapLen]);
          return [[rangeStart, rangeLen - overlapLen]];
        } else if (
          //   rrrr
          // ssss
          sourceStart <= rangeStart &&
          rangeStart <= sourceEnd &&
          sourceEnd <= rangeEnd
        ) {
          const overlapLen = sourceEnd - rangeStart + 1;
          outputRanges.push([destStart + (rangeStart - sourceStart), overlapLen]);
          return [[sourceEnd, rangeLen - overlapLen]];
        } else if (
          // rrrrrr
          //  ssss
          rangeStart <= sourceStart &&
          sourceEnd <= rangeEnd
        ) {
          outputRanges.push([destStart, mapLen]);
          return [
            [rangeStart, sourceStart - rangeStart + 1],
            [sourceEnd, rangeEnd - sourceEnd + 1],
          ]
        } else {
          return [[rangeStart, rangeLen]]
        }
      });
    }
    return [...inputRanges, ...outputRanges];
  }
}

main();
