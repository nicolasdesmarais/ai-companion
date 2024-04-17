import { codeFileExtensions } from "@/src/lib/mime";
import { isMsftConvertible, isSupportedFileType } from "../DataSourcesApi";

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

test("source code files are supported as text", () => {
  expect(isSupportedFileType("Dockerfile")).toBe(true);
  expect(isSupportedFileType("Jenkinsfile")).toBe(true);
  expect(isSupportedFileType("Makefile")).toBe(true);
  expect(isSupportedFileType("test.js")).toBe(true);
  expect(isSupportedFileType("test.properties")).toBe(true);
  expect(isSupportedFileType("test.css")).toBe(true);
  expect(isSupportedFileType("test.html")).toBe(true);
  expect(isSupportedFileType("test.htm")).toBe(true);
  expect(isSupportedFileType("test.f90")).toBe(true);
  expect(isSupportedFileType("test.c")).toBe(true);
  expect(isSupportedFileType("test.erl")).toBe(true);
  expect(isSupportedFileType("test.java")).toBe(true);
});

test("all programming languages are supported as text", () => {
  const mimes = [];

  Object.keys(codeFileExtensions).forEach((ext) => {
    expect(isSupportedFileType(`test.${ext}`)).toBe(true);
  });
  expect(true).toBe(true);
});
