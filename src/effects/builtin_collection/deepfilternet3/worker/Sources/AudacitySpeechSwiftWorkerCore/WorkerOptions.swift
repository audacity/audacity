import Foundation

public struct WorkerJob: Equatable, Sendable {
  public let input: URL
  public let output: URL
}

public struct WorkerOptions: Equatable, Sendable {
  public let jobs: [WorkerJob]
  public let cacheRoot: URL?
  public let modelId: String?
  public let offlineMode: Bool
  public let chunkSeconds: Double
  public let overlapMilliseconds: Int

  public static func parse(_ arguments: [String]) throws -> WorkerOptions {
    guard arguments.first == "enhance" else {
      throw WorkerOptionsError.usage
    }

    var inputs: [URL] = []
    var outputs: [URL] = []
    var cacheRoot: URL?
    var modelId: String?
    var offlineMode = false
    var chunkSeconds = 45.0
    var overlapMilliseconds = 500

    var index = 1
    while index < arguments.count {
      let argument = arguments[index]
      switch argument {
      case "--input":
        inputs.append(URL(fileURLWithPath: try value(after: argument, at: &index, in: arguments)))
      case "--output":
        outputs.append(URL(fileURLWithPath: try value(after: argument, at: &index, in: arguments)))
      case "--cache-dir":
        cacheRoot = URL(fileURLWithPath: try value(after: argument, at: &index, in: arguments))
      case "--model":
        modelId = try value(after: argument, at: &index, in: arguments)
      case "--chunk-seconds":
        let raw = try value(after: argument, at: &index, in: arguments)
        guard let parsed = Double(raw), parsed > 1 else {
          throw WorkerOptionsError.invalidValue(option: argument, value: raw)
        }
        chunkSeconds = parsed
      case "--overlap-ms":
        let raw = try value(after: argument, at: &index, in: arguments)
        guard let parsed = Int(raw), parsed >= 0 else {
          throw WorkerOptionsError.invalidValue(option: argument, value: raw)
        }
        overlapMilliseconds = parsed
      case "--offline":
        offlineMode = true
      default:
        throw WorkerOptionsError.unknownOption(argument)
      }
      index += 1
    }

    guard !inputs.isEmpty else {
      throw WorkerOptionsError.noJobs
    }
    guard inputs.count == outputs.count else {
      throw WorkerOptionsError.mismatchedJobs(inputs: inputs.count, outputs: outputs.count)
    }
    guard Double(overlapMilliseconds) < chunkSeconds * 500 else {
      throw WorkerOptionsError.invalidOverlap
    }

    let jobs = try zip(inputs, outputs).map { input, output in
      let normalizedInput = input.standardizedFileURL
      let normalizedOutput = output.standardizedFileURL
      guard normalizedInput != normalizedOutput else {
        throw WorkerOptionsError.sameInputAndOutput(normalizedInput.path)
      }
      return WorkerJob(input: normalizedInput, output: normalizedOutput)
    }
    guard Set(jobs.map(\.output)).count == jobs.count else {
      throw WorkerOptionsError.duplicateOutput
    }
    let inputPaths = Set(jobs.map(\.input))
    if let collidingOutput = jobs.lazy.map(\.output).first(where: inputPaths.contains) {
      throw WorkerOptionsError.outputOverwritesInput(collidingOutput.path)
    }

    return WorkerOptions(
      jobs: jobs,
      cacheRoot: cacheRoot?.standardizedFileURL,
      modelId: modelId,
      offlineMode: offlineMode,
      chunkSeconds: chunkSeconds,
      overlapMilliseconds: overlapMilliseconds
    )
  }

  private static func value(
    after option: String,
    at index: inout Int,
    in arguments: [String]
  ) throws -> String {
    index += 1
    guard index < arguments.count else {
      throw WorkerOptionsError.missingValue(option)
    }
    return arguments[index]
  }
}

public enum WorkerOptionsError: Error, LocalizedError, Equatable {
  case usage
  case noJobs
  case missingValue(String)
  case unknownOption(String)
  case invalidValue(option: String, value: String)
  case mismatchedJobs(inputs: Int, outputs: Int)
  case sameInputAndOutput(String)
  case duplicateOutput
  case outputOverwritesInput(String)
  case invalidOverlap

  public var errorDescription: String? {
    switch self {
    case .usage:
      return
        "Usage: audacity-speech-swift-worker enhance --input INPUT --output OUTPUT [--cache-dir ROOT] [--offline]"
    case .noJobs:
      return "At least one --input/--output pair is required"
    case .missingValue(let option):
      return "Missing value for \(option)"
    case .unknownOption(let option):
      return "Unknown option: \(option)"
    case .invalidValue(let option, let value):
      return "Invalid value for \(option): \(value)"
    case .mismatchedJobs(let inputs, let outputs):
      return "Input/output count mismatch (\(inputs) inputs, \(outputs) outputs)"
    case .sameInputAndOutput(let path):
      return "Input and output paths must differ: \(path)"
    case .duplicateOutput:
      return "Every job must use a distinct output path"
    case .outputOverwritesInput(let path):
      return "An output path must not overwrite any input: \(path)"
    case .invalidOverlap:
      return "The overlap must be less than half of the chunk duration"
    }
  }
}
