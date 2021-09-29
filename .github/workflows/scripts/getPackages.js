const { readFileSync } = require("fs");
const { join } = require("path");

let fileContent = readFileSync(join(process.env.GITHUB_WORKSPACE, "Cheat Engine/cheatengine.lpi"), { encoding: "utf-8", flag: "r" });
let dependencies = fileContent.match(/(?<=<PackageName Value=").*?(?="\/>)/g).join(",");

console.log(dependencies);
