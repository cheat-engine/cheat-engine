const { join } = require("path");
const { exec } = require("child_process");
const { createWriteStream } = require("fs");
const { get: _get } = require("https");

get(
  `https://downloads.sourceforge.net/project/lazarus/Lazarus%20Windows%2064%20bits/Lazarus%20${process.env.LAZARUS_VERSION}/lazarus-${process.env.LAZARUS_VERSION}-fpc-${process.env.FPC_VERSION}-cross-i386-win32-win64.exe`,
  (res) => {
    const download = createWriteStream(join(process.env.GITHUB_WORKSPACE, "32bit.exe"));
    res.pipe(download);
    download.on("finish", () => {
      exec(`cd "${process.env.GITHUB_WORKSPACE}" && 32bit.exe /VERYSILENT /DIR=${join(process.env.RUNNER_TEMP, "lazarus")}`);
    });
  }
);

function get(url, callback) {
  _get(url, (res) => {
    if (res.statusCode === 301 || res.statusCode === 302 || res.statusCode === 307 || res.statusCode === 308) {
      get(res.headers.location, callback);
    } else {
      callback(res);
    }
  });
}
