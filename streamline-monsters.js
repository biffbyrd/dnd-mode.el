const fs = require("fs");

const monsters = JSON.parse(fs.readFileSync("./monsters.json"));
const monsterMeta = JSON.parse(fs.readFileSync("./monster-meta.json")).legendaryGroup;
const legendaryGroups = {};
for (const i in monsterMeta) {
  const item = monsterMeta[i];
  legendaryGroups[item.name] = item;
}

const newMonsters = () => (
  monsters.map((m) => ({
    name: m.name,
    size: m.size || "n/a",
    type: getTypeString(m.type),
    alignment: getAlignmentString(m.alignment),
    ac: getACString(m.ac),
    hp: getHPString(m.hp),
    speed: getSpeedString(m.speed),
    str: m.str,
    dex: m.dex,
    con: m.con,
    int: m.int,
    wis: m.wis,
    cha: m.cha,
    save: getSaveString(m.save),
    skill: getSkillString(m.skill),
    resist: getResistString(m.resist),
    immune: getImmuneString(m.immune),
    conditionImmune: getConditionImmuneString(m.conditionImmune),
    senses: m.senses || undefined,
    languages: m.languages,
    cr: formatCR(m.cr),
    trait: formatEntries(m.trait),
    action: formatEntries(m.action),
    spellcasting: formatSpellCasting(m.spellcasting),
    legendary: formatEntries(m.legendary),
    lairActions: (legendaryGroups[m.name]) ? legendaryGroups[m.name].lairActions : undefined,
    regionalEffects: (legendaryGroups[m.name]) ? legendaryGroups[m.name].regionalEffects : undefined
  }))
);

const getTypeString = (type) => {
  if (!type) {
    return "n/a";
  }

  if (typeof type === "string") {
    return type;
  }

  if (type.type && type.tags) {
    let tags = type.tags.map((t) => {
      if (typeof t === "string") {return t;}
      if (t.tag && t.prefix) {return t.prefix + " " + t.tag;}
      if (t.tag) {return t.tag;}
      return "n/a";
    });
    return type.type + " (" + tags.join(",") + ")";
  }

  if (type.type) {
    return type.type;
  }

  return "n/a";
};


const getAlignmentString = (alignment) => {
  if (!alignment || !alignment.length) {
    return "n/a";
  }

  if (typeof alignment[0] === "string") {
    return alignment.join("/");
  }

  let result = [];
  for (const i in alignment) {
    const a = alignment[i];
    if (a.special) {
      result.push(a.special);
    } else {
      result.push(a.alignment.join("/") + " (" + a.chance + "%)");
    }
  }
  return result.join(", ");
};

const getACString = (acArr) => {
  if (!acArr || !acArr.length) {
    return "n/a";
  }

  let result = "";
  for (const i in acArr) {
    const ac = acArr[i];
    if (typeof ac === "number") {
      result = result + ac;
    } else if (ac.from != null) {
      result = result + ac.ac + " (from " + ac.from.join(", ") + ")";
    } else if (ac.condition != null) {
      result = result + " (" + ac.ac + " " + ac.condition + ")";
    }
  }

  return result;
};

const getHPString = (hp) => {
  if (!hp) {return "n/a";}

  if (hp.average) {
    return hp.average + " (" + hp.formula + ")";
  }

  if (hp.special) {
    return hp.special;
  }

  return "n/a";
};

const getSpeedString = (speed) => {
  if (!speed) {
    return "n/a";
  }
  
  let result = [];
  for (const k in speed) {
    const s = speed[k];
    if (typeof s === "number") {
      result.push(k + " " + s);
      continue;
    }

    const n = s.number;
    const c = s.condition;
    if (n != null && c != null) {
      result.push(k + " " + n + " " + c);
      continue;
    }

    if (n != null) {
      result.push(k + " " + n);
      continue;
    }
  }

  return result.join(", ");
};

const getSaveString = (save) => {
  if (!save) {return "n/a";}

  let result = [];
  for(const k in save) {
    result.push(k + " " +  save[k]);
  }
  return result.join("; ");
};

const getSkillString = (skill) => {
  if (!skill) {return "n/a";}

  let result = [];
  for(const k in skill) {
    if (k === "other") {
      if (skill[k].length && skill[k].length > 0 && skill[k][0].oneOf) {
        const skill2 = skill[k][0].oneOf;
        let oneOf = [];
        for (const k2 in skill2) {
          oneOf.push(k2 + " " +  skill2[k2]);
        }
        result.push("one of " + oneOf.join(", "));
      }
      continue;
    }
    result.push(k + " " +  skill[k]);
  }
  return result.join("; ");
};

const getResistString = (resists) => {
  if (!resists) {return "n/a";}

  return resists.map((r) => {
    if (typeof r === "string") {
      return r;
    }
    if (r.special) {
      return r.special;
    }
    return r.resist.join(", ") + " " + r.note;
  }).join("; ");
};

const getImmuneString = (immune) => {
  if (!immune) {return "n/a";}

  let result = [];
  for(const i in immune) {
    const item = immune[i];
    
    if (typeof item === "string") {
      result.push(item);
      continue;
    }

    if (!item.immune || !item.note) {
      continue;
    }

    result.push(item.immune.join(", ") + " " + item.note);
  }
  return result.join("; ");
};

const getConditionImmuneString = (immune) => {
  if (!immune) {return "n/a";}

  let result = [];
  for(const i in immune) {
    const item = immune[i];
    
    if (typeof item === "string") {
      result.push(item);
      continue;
    }

    if (!item.conditionImmune || !item.preNote) {
      continue;
    }

    result.push(item.preNote + " " + item.conditionImmune.join(", "));
  }
  return result.join("; ");
};

const formatEntries = (entries) =>
      !entries ? undefined : entries.map((e) => e.name + ". " + e.entries.join("\n\n"));

const formatSpellCasting = (spellArr) => {
  if (!spellArr) {
    return undefined;
  }

  const formattedArr = [];
  for(const i in spellArr){
    const item = spellArr[i];
    const formattedItem = {};

    formattedItem.name = item.name;
    formattedItem.header = item.headerEntries.join("\n\n");
    formattedItem.spells = [];
    
    if (item.will && item.will.length) {
      formattedItem.spells.push("At will: " + item.will.join(", "));
    }

    if (item.daily) {
      for(const d in item.daily) {
        formattedItem.spells.push(d + ": " + item.daily[d].join(", "));
      }
    }

    if (item.spells) {
      for(const l in item.spells) {
        const spells = item.spells[l].spells.join(", ");
        const slots = item.spells[l].slots ? ` (${item.spells[l].slots} slots)` : "";
        formattedItem.spells.push(`${l}${slots}: ${spells}`);
      }
    }
    formattedArr.push(formattedItem);
  }
  
  return formattedArr;
};

const formatCR = (cr) => {
  if (!cr) return "n/a";
  if (typeof cr === "string") return cr;

  let res = [];
  for(const k in cr) {
    res.push(`${k} ${cr[k]}`);
  }
  return res.join("; ");
};

console.log(JSON.stringify(newMonsters()));
