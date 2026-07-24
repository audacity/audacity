import Foundation
import XCTest

@testable import AudacitySpeechSwiftWorkerCore

final class WorkerOptionsTests: XCTestCase {
  func testParsesMultipleJobsAndRuntimeOptions() throws {
    let options = try WorkerOptions.parse([
      "enhance",
      "--input", "/tmp/in-1.sspcm",
      "--output", "/tmp/out-1.sspcm",
      "--input", "/tmp/in-2.sspcm",
      "--output", "/tmp/out-2.sspcm",
      "--cache-dir", "/tmp/models",
      "--model", "example/model",
      "--chunk-seconds", "30",
      "--overlap-ms", "250",
      "--offline",
    ])

    XCTAssertEqual(options.jobs.count, 2)
    XCTAssertEqual(options.modelId, "example/model")
    XCTAssertEqual(options.cacheRoot?.path, "/tmp/models")
    XCTAssertEqual(options.chunkSeconds, 30)
    XCTAssertEqual(options.overlapMilliseconds, 250)
    XCTAssertTrue(options.offlineMode)
  }

  func testRejectsMismatchedInputOutputPairs() {
    XCTAssertThrowsError(
      try WorkerOptions.parse([
        "enhance", "--input", "/tmp/in.sspcm",
      ])
    ) { error in
      XCTAssertEqual(
        error as? WorkerOptionsError,
        .mismatchedJobs(inputs: 1, outputs: 0)
      )
    }
  }

  func testRejectsSameInputAndOutput() {
    XCTAssertThrowsError(
      try WorkerOptions.parse([
        "enhance",
        "--input", "/tmp/audio.sspcm",
        "--output", "/tmp/audio.sspcm",
      ])
    ) { error in
      XCTAssertEqual(
        error as? WorkerOptionsError,
        .sameInputAndOutput("/tmp/audio.sspcm")
      )
    }
  }

  func testRejectsOutputThatOverwritesAnotherInput() {
    XCTAssertThrowsError(
      try WorkerOptions.parse([
        "enhance",
        "--input", "/tmp/in-1.sspcm",
        "--output", "/tmp/in-2.sspcm",
        "--input", "/tmp/in-2.sspcm",
        "--output", "/tmp/out-2.sspcm",
      ])
    ) { error in
      XCTAssertEqual(
        error as? WorkerOptionsError,
        .outputOverwritesInput("/tmp/in-2.sspcm")
      )
    }
  }
}
