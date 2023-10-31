import { User } from "@clerk/nextjs/server";

const EMAIL_REGEX = /^\S+@\S+\.\S+$/;

export class EmailUtils {
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

  public static getUserPrimaryEmailAddress(user: User): string | null {
    const primaryEmailId = user.primaryEmailAddressId;
    const primaryEmail = user.emailAddresses.find(
      (emailAddress) => emailAddress.id === primaryEmailId
    );
    return primaryEmail ? primaryEmail.emailAddress : null;
  }
}

export default EmailUtils;
