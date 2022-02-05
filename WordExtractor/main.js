const fs = require('fs')

const ctx = fs.readFileSync('./test.txt', "utf8");
const words = ctx.split(/[\s\n]+/);
const s = new Set(words);
fs.writeFileSync('./result.txt', [...s].join("\n"));
