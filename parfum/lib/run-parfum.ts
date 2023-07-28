import { DockerParser, File } from "@tdurieux/dinghy";
import { Matcher } from "./rule-matcher";

import {
  aptGetUpdatePrecedesInstall, // use-no-install-recommends
  aptGetInstallUseNoRec // do-not-use-apt-get-update-alone
} from "./rules/binnacle";

import {
  DL3020, // use-copy-instead-of-add
  DL3002 // have-a-user
} from "./rules/hadolint";

import * as fs from 'fs';

const SMELLY_DOCKERFILES_PATH = "../dataset/smelly_dockerfiles_bianncle";
const FIXED_DOCKERFILES_PATH = "../dataset/fixed_bianncle_dockerfiles";

const RULE_MAPINGS = {
  "aptGetInstallUseNoRec": "use-no-install-recommends",
  "aptGetUpdatePrecedesInstall": "do-not-use-apt-get-update-alone",
  "DL3020": "use-copy-instead-of-add",
  "DL3002": "have-a-user"
};

const run_parfum = async (dockerfile_name) => {

  const dockerParser = new DockerParser(new File(SMELLY_DOCKERFILES_PATH + "/" + dockerfile_name)); // the path or the content can be provided
  const ast = await dockerParser.parse();
  const matcher = new Matcher(ast);

  //const violations = matcher.matchAll();

  // look for specific rules
  const violations = matcher.matchAll([aptGetInstallUseNoRec, aptGetUpdatePrecedesInstall, DL3020, DL3002]);
  
  let violatedRules = new Set();
  for (const violation of violations) {
    violatedRules.add(RULE_MAPINGS[violation.rule.name]);
  }

  console.log("> Detect: " + dockerfile_name + "," + Array.from(violatedRules).join(','))

  for (const violation of violations) {
    try {
      await violation.repair(); // repair the violation
    }
    catch (error) {
      console.error("Error while fixing violation " + violation.rule.name + " for " + dockerfile_name);
    }
  }

  const repairedDockerfile = ast.toString(true);
  
  try {
    fs.writeFileSync(FIXED_DOCKERFILES_PATH + "/" + dockerfile_name, repairedDockerfile);
  } 
  catch (err) {
    console.error(err);
  }
};

async function find_and_repair_dockerfiles() {
  try {
    const dockerfiles = await fs.promises.readdir(SMELLY_DOCKERFILES_PATH);
    for (let i = 0; i < dockerfiles.length; i++) {
      let f = dockerfiles[i];
      if (f.endsWith(".dockerfile")) {
        try {
          await run_parfum(f);
          console.log("> Fixed " + f);
        }
        catch (err) {
          console.error("> Error while fixing " + f);
        }
      }
    }
  }
  catch (err) {
    console.error('Error occurred while reading directory!', err);
  }
}

find_and_repair_dockerfiles();