const fs = require("fs");
const path = require("path");
const R = require("ramda");

const rankedWords = require("../ranks.json");

const allMappings = require("../src/assets/ChineseQuickMapping.json");

const smallerMapping = R.pipe(
  R.map((char) => ({ [char]: allMappings[char] })),
  R.mergeAll
)(rankedWords);

fs.writeFileSync(
  path.join(__dirname, "../src/assets/ChineseQuickMappingSmall.json"),
  JSON.stringify(smallerMapping)
);