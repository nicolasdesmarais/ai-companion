const nationalities: string[] = [
  "latin",
  "indian",
  "asian",
  "caucasian",
  "black",
];

const genders: string[] = ["man", "woman"];

const ages: string[] = ["adult", "middle aged"];

export const getDiversityString = (): string => {
  const nationality: string =
    nationalities[Math.floor(Math.random() * nationalities.length)];
  const age = ages[Math.floor(Math.random() * ages.length)];
  const gender = genders[Math.floor(Math.random() * genders.length)];
  return `${nationality} ${age} ${gender}`;
};
