const EMAIL_REGEX = /^\S+@\S+\.\S+$/;

export class Utilities {
  /**
   * Parses a string containing a comma-separated list of emails into an array of unique, valid emails.
   * @param emailsCsv
   * @returns
   */
  public static parseEmailCsv(emailsCsv: string): string[] {
    const validEmailsArray = emailsCsv
      .split(",")
      .map((email) => email.trim())
      .filter((email) => EMAIL_REGEX.test(email));

    const uniqueEmailsSet = new Set(validEmailsArray);
    return Array.from(uniqueEmailsSet);
  }
}
