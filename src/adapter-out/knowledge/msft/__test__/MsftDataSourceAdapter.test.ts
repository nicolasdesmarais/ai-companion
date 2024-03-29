import { isConvertible } from "../MsftDataSourceAdapter";

test("isConvertible should return false for unsupported file names", () => {
  expect(isConvertible("test.html")).toBe(false);
  expect(isConvertible("test.json")).toBe(false);
  expect(isConvertible("test.pdf")).toBe(false);
  expect(isConvertible("test.docx")).toBe(false);
});

test("isConvertible should return true for supported file names", () => {
  expect(isConvertible("test.doc")).toBe(true);
  expect(isConvertible("test.eml")).toBe(true);
  expect(isConvertible("test.msg")).toBe(true);
  expect(isConvertible("test.odp")).toBe(true);
  expect(isConvertible("test.ods")).toBe(true);
  expect(isConvertible("test.odt")).toBe(true);
  expect(isConvertible("test.pps")).toBe(true);
  expect(isConvertible("test.ppt")).toBe(true);
  expect(isConvertible("test.rtf")).toBe(true);
  expect(isConvertible("test.tif")).toBe(true);
  expect(isConvertible("test.tiff")).toBe(true);
  expect(isConvertible("test.xls")).toBe(true);
  expect(isConvertible("test.xlsm")).toBe(true);
  expect(isConvertible("test.xlsx")).toBe(true);
});
