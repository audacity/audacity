import Foundation
import XCTest

@testable import AudacitySpeechSwiftWorkerCore

final class E2EDeepFilterWorkerTests: XCTestCase {
  func testEnhancesAudioWithDeepFilterNet3() async throws {
    let environment = ProcessInfo.processInfo.environment
    guard environment["RUN_DEEPFILTER_E2E"] == "1" else {
      throw XCTSkip("Set RUN_DEEPFILTER_E2E=1 to run Core ML model inference")
    }

    let directory = FileManager.default.temporaryDirectory
      .appendingPathComponent(UUID().uuidString, isDirectory: true)
    try FileManager.default.createDirectory(at: directory, withIntermediateDirectories: true)
    addTeardownBlock { try? FileManager.default.removeItem(at: directory) }

    let inputURL = directory.appendingPathComponent("input.sspcm")
    let outputURL = directory.appendingPathComponent("output.sspcm")
    let header = try PlanarPCMHeader(
      sampleRate: 48_000,
      channelCount: 1,
      frameCount: 48_000
    )
    let input = (0..<header.frameCount).map { index -> Float in
      let time = Double(index) / Double(header.sampleRate)
      return Float(
        0.2 * sin(2 * .pi * 220 * time)
          + 0.04 * sin(2 * .pi * 3_137 * time))
    }
    let writer = try PlanarPCMOutput(url: inputURL, header: header)
    try writer.appendChannel(input)
    try writer.close()
    XCTAssertTrue(
      FileManager.default.createFile(
        atPath: outputURL.path,
        contents: Data("pending".utf8),
        attributes: [.posixPermissions: 0o600]
      ))
    let outputInode = try XCTUnwrap(
      FileManager.default.attributesOfItem(atPath: outputURL.path)[.systemFileNumber]
        as? NSNumber
    ).uint64Value

    var arguments = [
      "enhance",
      "--input", inputURL.path,
      "--output", outputURL.path,
    ]
    if environment["DEEPFILTER_USE_TEMP_CACHE"] == "1" {
      arguments += [
        "--cache-dir",
        directory.appendingPathComponent("model-cache", isDirectory: true).path,
      ]
    } else if let cacheRoot = environment["DEEPFILTER_CACHE_ROOT"], !cacheRoot.isEmpty {
      arguments += ["--cache-dir", cacheRoot]
    }
    if environment["DEEPFILTER_OFFLINE"] == "1" {
      arguments.append("--offline")
    }

    if let workerPath = environment["DEEPFILTER_WORKER_BINARY"], !workerPath.isEmpty {
      let process = Process()
      let standardOutput = Pipe()
      let standardError = Pipe()
      process.executableURL = URL(fileURLWithPath: workerPath)
      process.arguments = arguments
      process.standardOutput = standardOutput
      process.standardError = standardError
      try process.run()
      process.waitUntilExit()

      let outputText =
        String(
          data: standardOutput.fileHandleForReading.readDataToEndOfFile(),
          encoding: .utf8
        ) ?? ""
      let errorText =
        String(
          data: standardError.fileHandleForReading.readDataToEndOfFile(),
          encoding: .utf8
        ) ?? ""
      guard process.terminationStatus == 0 else {
        return XCTFail("Worker failed: \(errorText)")
      }
      XCTAssertTrue(
        outputText.split(separator: "\n").contains { line in
          guard let data = line.data(using: .utf8),
            let event = try? JSONSerialization.jsonObject(with: data) as? [String: Any]
          else {
            return false
          }
          return event["type"] as? String == "result"
        })
    } else {
      try await DeepFilterWorker.run(options: WorkerOptions.parse(arguments)) { _ in }
    }

    let reader = try PlanarPCMReader(url: outputURL)
    let enhanced = try reader.readChannel(0)
    XCTAssertEqual(reader.header, header)
    XCTAssertEqual(enhanced.count, input.count)
    XCTAssertTrue(enhanced.allSatisfy(\.isFinite))
    XCTAssertGreaterThan(
      zip(input, enhanced).reduce(0.0) { sum, pair in
        sum + Double(abs(pair.0 - pair.1))
      },
      0.001
    )
    let finalOutputInode = try XCTUnwrap(
      FileManager.default.attributesOfItem(atPath: outputURL.path)[.systemFileNumber]
        as? NSNumber
    ).uint64Value
    XCTAssertEqual(finalOutputInode, outputInode)
  }
}
