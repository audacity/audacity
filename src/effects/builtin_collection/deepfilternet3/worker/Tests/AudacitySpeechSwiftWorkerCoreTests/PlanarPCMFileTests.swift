import Foundation
import XCTest

@testable import AudacitySpeechSwiftWorkerCore

final class PlanarPCMFileTests: XCTestCase {
  func testRoundTripsPlanarFloatAudio() throws {
    let url = temporaryURL("roundtrip.sspcm")
    let header = try PlanarPCMHeader(sampleRate: 44_100, channelCount: 2, frameCount: 4)
    let output = try PlanarPCMOutput(url: url, header: header)
    try output.appendChannel([0, 0.25, -0.5, 1])
    try output.appendChannel([1, -1, 0.125, -0.125])
    try output.close()

    let reader = try PlanarPCMReader(url: url)
    XCTAssertEqual(reader.header, header)
    XCTAssertEqual(try reader.readChannel(0), [0, 0.25, -0.5, 1])
    XCTAssertEqual(try reader.readChannel(1), [1, -1, 0.125, -0.125])
  }

  func testRejectsTruncatedPayload() throws {
    let url = temporaryURL("truncated.sspcm")
    let header = try PlanarPCMHeader(sampleRate: 48_000, channelCount: 1, frameCount: 4)
    try header.encoded.write(to: url)

    XCTAssertThrowsError(try PlanarPCMReader(url: url)) { error in
      guard case PlanarPCMError.unexpectedFileSize = error else {
        return XCTFail("Unexpected error: \(error)")
      }
    }
  }

  func testRejectsNonzeroReservedHeaderField() throws {
    var encoded = try PlanarPCMHeader(
      sampleRate: 48_000,
      channelCount: 1,
      frameCount: 4
    ).encoded
    encoded.replaceSubrange(20..<24, with: [1, 0, 0, 0])

    XCTAssertThrowsError(try PlanarPCMHeader.decode(encoded)) { error in
      XCTAssertEqual(error as? PlanarPCMError, .nonzeroReservedField(1))
    }
  }

  func testNormalizesOutputLength() {
    XCTAssertEqual(normalizeSampleCount([1, 2, 3], expectedCount: 2), [1, 2])
    XCTAssertEqual(normalizeSampleCount([1], expectedCount: 3), [1, 0, 0])
  }

  func testResolvesModelDirectoryBelowCacheRoot() throws {
    let root = temporaryURL("model-cache")
    let resolved = try XCTUnwrap(
      resolveModelCacheDirectory(
        modelId: "aufklarer/DeepFilterNet3-CoreML",
        cacheRoot: root
      ))

    XCTAssertEqual(
      resolved.standardizedFileURL.path,
      root.appendingPathComponent("models/aufklarer/DeepFilterNet3-CoreML")
        .standardizedFileURL.path
    )
  }

  private func temporaryURL(_ name: String) -> URL {
    let directory = FileManager.default.temporaryDirectory
      .appendingPathComponent(UUID().uuidString, isDirectory: true)
    try? FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
    addTeardownBlock { try? FileManager.default.removeItem(at: directory) }
    return directory.appendingPathComponent(name)
  }
}
