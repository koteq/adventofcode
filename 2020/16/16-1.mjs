import { promises as fs } from "fs";
import { fileURLToPath } from "url";

async function main() {
  const data = (await fs.readFile("16-1.in")).toString();
  const [rulesData, myTicketData, nearbyTicketsData] = data.split("\n\n");

  const rules = parseRules(rulesData);
  const allRules = Object.values(rules);

  let errorRate = 0;
  for (const ticketData of nearbyTicketsData.split("\n").slice(1)) {
    const ticket = parseTicket(ticketData);
    const invalidFields = validateTicket(ticket, allRules);
    for (const field of invalidFields) {
      errorRate += field;
    }
  }

  console.log(`Error rate is ${errorRate}`);
}

function validateTicket(ticket, rules) {
  const invalid = [];
  for (const field of ticket) {
    if (
      !rules.some(
        ([[a, b], [c, d]]) =>
          (a <= field && field <= b) || (c <= field && field <= d)
      )
    ) {
      invalid.push(field);
    }
  }
  return invalid;
}

function parseRules(data) {
  const result = {};
  for (const match of data.matchAll(/(\w+): (\d+)-(\d+) or (\d+)-(\d+)/g)) {
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
