const nationalities: string[] = [
  "African",
  "Sudanese",
  "Caribbean",
  "Indian",
  "Melanesian",
  "Australian",
  "Chinese",
  "Guamanian",
  "Japanese",
  "Korean",
  "Polynesian",
  "European",
  "Irish",
  "Russian",
  "American",
  "Pakistani",
  "Bangladeshi",
  "Vietnamese",
  "Micronesian",
  "Hispanic",
  "Filipino",
  "Latin American",
  "Puerto Rican",
  "Mexican",
  "African American",
  "Native American",
  "Caucasian",
  "Arab",
];

const genders: string[] = ["man", "woman"];

const ages: string[] = ["young", "30 year old", "middle aged", "old"];

export const getDiversityString = (): string => {
  const nationality: string =
    nationalities[Math.floor(Math.random() * nationalities.length)];
  const age = ages[Math.floor(Math.random() * ages.length)];
  const gender = genders[Math.floor(Math.random() * genders.length)];
  return `${nationality} ${age} ${gender}`;
};
