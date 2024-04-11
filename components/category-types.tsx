export const CategoryTypes = [
    { name: "AI Models", id: "AI_MODELS" },
    { name: "Productivity", id: "PRODUCTIVITY" },
    { name: "Learning & Development", id: "LEARNING_DEVELOPMENT" },
    { name: "Marketing", id: "MARKETING" },
    { name: "Sales", id: "SALES" },
    { name: "Information Technology", id: "INFORMATION_TECHNOLOGY" },
    { name: "Engineering", id: "ENGINEERING" },
    { name: "Human Resources", id: "HUMAN_RESOURCES" },
    { name: "Accounting & Finance", id: "ACCOUNTING_FINANCE" },
];

export const CategoryTypesMap: Record<string, string> = {};
CategoryTypes.forEach((category : { name: string; id: string }) => CategoryTypesMap[category.id] = category.name);

export const CategoryTypesHardcoded: Record<string, string> = {
    "ACCOUNTING" : "ACCOUNTING_FINANCE",
    "ENG" : "ENGINEERING",
    "HR" : "HUMAN_RESOURCES",
    "IT" : "INFORMATION_TECHNOLOGY",
    "LEARNING" : "LEARNING_DEVELOPMENT",
    "MARKETING" : "MARKETING",
    "MODELS" : "AI_MODELS",
    "NONE" : "NONE",
    "PRODUCTIVITY" : "PRODUCTIVITY",
    "SALES" : "SALES"
};
