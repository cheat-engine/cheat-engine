const { join } = require("path");
const { execSync } = require("child_process");
const fetchSync = require("@the-bds-maneger/fetchsync");

fetchSync(
  `https://downloads.sourceforge.net/project/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%20${process.env.LAZARUS_VERSION}/lazarus-${process.env.LAZARUS_VERSION}-fpc-${process.env.FPC_VERSION}-cross-i386-win32-win64.exe`,
  {
    Binary: true,
  }
).save(join(process.env.GITHUB_WORKSPACE, "32bit.exe"));
execSync(`cd "${process.env.GITHUB_WORKSPACE}" && 32bit.exe /VERYSILENT /DIR=${join(process.env.RUNNER_TEMP, "lazarus")}`);
