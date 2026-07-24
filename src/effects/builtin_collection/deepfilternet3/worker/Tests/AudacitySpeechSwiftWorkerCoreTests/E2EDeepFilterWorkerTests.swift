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
    let sampleRate = 48_000
    let header = try PlanarPCMHeader(
      sampleRate: sampleRate,
      channelCount: 1,
      frameCount: sampleRate * 3
    )
    var noiseState: UInt64 = 0xD3_EE_5E_ED
    let input = (0..<header.frameCount).map { index -> Float in
      let time = Double(index) / Double(header.sampleRate)
      noiseState = noiseState &* 6_364_136_223_846_793_005 &+ 1_442_695_040_888_963_407
      let noise = Double(noiseState >> 40) / Double(1 << 23) - 1

      let speech: Double
      if (1..<2).contains(time) {
        let localTime = time - 1
        let envelope = sin(.pi * localTime)
        let fundamental = 175 + 25 * sin(2 * .pi * 1.7 * localTime)
        speech =
          envelope
          * (0.18 * sin(2 * .pi * fundamental * localTime)
            + 0.08 * sin(2 * .pi * fundamental * 2 * localTime)
            + 0.04 * sin(2 * .pi * fundamental * 3 * localTime))
      } else {
        speech = 0
      }
      return Float(speech + 0.06 * noise)
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
      let outputTask = Task.detached {
        standardOutput.fileHandleForReading.readDataToEndOfFile()
      }
      let errorTask = Task.detached {
        standardError.fileHandleForReading.readDataToEndOfFile()
      }
      process.waitUntilExit()

      let outputText =
        String(
          data: await outputTask.value,
          encoding: .utf8
        ) ?? ""
      let errorText =
        String(
          data: await errorTask.value,
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
    let noiseOnlyRanges = [
      sampleRate / 4..<(sampleRate * 3 / 4),
      sampleRate * 9 / 4..<(sampleRate * 11 / 4),
    ]
    let inputNoiseRMS = rootMeanSquare(input, in: noiseOnlyRanges)
    let enhancedNoiseRMS = rootMeanSquare(enhanced, in: noiseOnlyRanges)
    let noiseSuppressionDB = 20 * log10(inputNoiseRMS / max(enhancedNoiseRMS, 1e-12))
    XCTAssertGreaterThan(
      noiseSuppressionDB,
      20,
      "Expected at least 20 dB of noise-only suppression, measured \(noiseSuppressionDB) dB"
    )
    XCTAssertGreaterThan(
      rootMeanSquare(enhanced, in: [sampleRate * 5 / 4..<(sampleRate * 7 / 4)]),
      0.005,
      "Enhancement must not collapse the active signal to silence"
    )
    let finalOutputInode = try XCTUnwrap(
      FileManager.default.attributesOfItem(atPath: outputURL.path)[.systemFileNumber]
        as? NSNumber
    ).uint64Value
    XCTAssertEqual(finalOutputInode, outputInode)
  }

  private func rootMeanSquare(_ samples: [Float], in ranges: [Range<Int>]) -> Double {
    var sumOfSquares = 0.0
    var sampleCount = 0
    for range in ranges {
      for sample in samples[range] {
        let value = Double(sample)
        sumOfSquares += value * value
        sampleCount += 1
      }
    }
    return sqrt(sumOfSquares / Double(sampleCount))
  }
}
