import { promises as fs } from "fs";
import { fileURLToPath } from "url";

async function main() {
  const data = (await fs.readFile("16-1.in")).toString();
  const [rulesData, myTicketData, nearbyTicketsData] = data.split("\n\n");

  const rules = parseRules(rulesData);
  const allRules = Object.values(rules);

  const validTickets = [parseTicket(myTicketData.split("\n")[1])];

  for (const ticketData of nearbyTicketsData.split("\n").slice(1)) {
    const ticket = parseTicket(ticketData);
    if (isTicketValid(ticket, allRules)) {
      validTickets.push(ticket);
    }
  }

  const fieldsMap = {};
  const fieldsCount = validTickets[0].length;
  for (const field of Object.keys(rules)) {
    fieldsMap[field] = new Set([...Array(fieldsCount).keys()]);
  }

  for (let pos = 0; pos < fieldsCount; pos++) {
    const colData = validTickets.map((e) => e[pos]);
    for (const [field, rule] of Object.entries(rules)) {
      if (!isColumnValid(colData, rule)) {
        fieldsMap[field].delete(pos);
      }
    }
  }

  const figuredPositionsSet = new Set();
  for (let i = 0; i < fieldsCount; i++) {
    const [field, matchedSet] = Object.entries(fieldsMap).find(
      ([, set]) =>
        set.size === 1 && !figuredPositionsSet.has(set.values().next().value)
    );
    const pos = matchedSet.values().next().value;
    for (const [otherField, otherSet] of Object.entries(fieldsMap)) {
      if (otherField !== field) {
        otherSet.delete(pos);
      }
    }
    figuredPositionsSet.add(pos);
  }

  console.log(fieldsMap);

  for (const [field, set] of Object.entries(fieldsMap)) {
    const pos = set.values().next().value;
    const myTicket = validTickets[0];
    console.log(`${field}: ${myTicket[pos]}`);
  }
}

function isTicketValid(ticket, rules) {
  for (const val of ticket) {
    if (!rules.some((rule) => isValueValid(val, rule))) {
      return false;
    }
  }
  return true;
}

function isColumnValid(data, rule) {
  return data.every((val) => isValueValid(val, rule));
}

function isValueValid(val, rule) {
  const [[a, b], [c, d]] = rule;
  return (a <= val && val <= b) || (c <= val && val <= d);
}

function parseRules(data) {
  const result = {};
  for (const match of data.matchAll(/([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)/g)) {
    const [, field, a, b, c, d] = match;
    result[field] = [
      [Number(a), Number(b)],
      [Number(c), Number(d)],
    ];
  }
  return result;
}

function parseTicket(data) {
  return data.split(",").map(Number);
}

function isMainProgram() {
  return process.argv[1] === fileURLToPath(import.meta.url);
}

if (isMainProgram) {
  main().catch((e) => {
    console.trace(e);
    process.exit(1);
  });
}
