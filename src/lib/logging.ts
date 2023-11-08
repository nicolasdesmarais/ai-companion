export function logWithTimestamp(message: string): void {
  const timestamp = new Date().toISOString(); // ISO 8601 format
  console.log(`[${timestamp}] ${message}`);
}
