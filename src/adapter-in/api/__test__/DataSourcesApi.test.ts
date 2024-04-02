import { isMsftConvertible } from "../DataSourcesApi";

test("isMsftConvertible should return false for unsupported file names", () => {
  expect(isMsftConvertible("test.html")).toBe(false);
  expect(isMsftConvertible("test.json")).toBe(false);
  expect(isMsftConvertible("test.pdf")).toBe(false);
  expect(isMsftConvertible("test.docx")).toBe(false);
});

test("isMsftConvertible should return true for supported file names", () => {
  expect(isMsftConvertible("test.eml")).toBe(true);
  expect(isMsftConvertible("test.msg")).toBe(true);
  expect(isMsftConvertible("test.odp")).toBe(true);
  expect(isMsftConvertible("test.ods")).toBe(true);
  expect(isMsftConvertible("test.odt")).toBe(true);
  expect(isMsftConvertible("test.rtf")).toBe(true);
  expect(isMsftConvertible("test.xls")).toBe(true);
  expect(isMsftConvertible("test.xlsm")).toBe(true);
  expect(isMsftConvertible("test.xlsx")).toBe(true);
});
