import fs from "node:fs";

function main(input_file = "in") {
  const [seedsData, ...rangeMapsData] = fs
    .readFileSync(input_file)
    .toString()
    .trim()
    .split("\n\n");
  const seeds = Array.from(seedsData.matchAll(/\d+/g)).map(([match]) =>
    Number(match)
  );
  const rangeMaps = rangeMapsData.map((data) => new RangeMap(data));
  const almanac = new Almanac(rangeMaps);
  const locations = seeds.map((seed) => almanac.mapSeedToLocation(seed));
  const answer = Math.min(...locations);
  console.log(answer);
}

class Almanac {
  constructor(rangeMaps) {
    this.rangeMaps = rangeMaps;
  }

  mapSeedToLocation(seed) {
    let result = seed;
    for (const rangeMap of this.rangeMaps) {
      result = rangeMap[result];
    }
    return result;
  }
}

class RangeMap {
  constructor(data) {
    const [, id, rangeData] = data.match(/([\w-]+) map:\n(.*)/s);
    this.id = id;
    this.map = rangeData
      .split("\n")
      .map((line) => Array.from(line.matchAll(/\d+/g)))
      .map((matches) => matches.map(([match]) => Number(match)));
    return new Proxy(this, {
      get: function (target, prop, receiver) {
        const source = Number(prop);
        for (const [destStart, sourceStart, length] of target.map) {
          if (sourceStart <= source && source <= sourceStart + length) {
            return destStart + (source - sourceStart);
          }
        }
        return source;
      },
    });
  }
}

main();
