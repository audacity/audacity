import AudacitySpeechSwiftWorkerCore
import Darwin
import Foundation

@main
enum AudacitySpeechSwiftWorkerMain {
  static func main() async {
    do {
      let options = try WorkerOptions.parse(Array(CommandLine.arguments.dropFirst()))
      try await DeepFilterWorker.run(options: options) { event in
        emit(event)
      }
    } catch {
      emitError(error.localizedDescription)
      Darwin.exit(EXIT_FAILURE)
    }
  }

  private static func emit(_ event: WorkerEvent) {
    switch event {
    case .progress(let fraction, let message):
      emitJSON([
        "type": "progress",
        "fraction": fraction,
        "message": message,
      ])
    case .completed(let jobCount, let channelCount):
      emitJSON([
        "type": "result",
        "jobs": jobCount,
        "channels": channelCount,
      ])
    }
  }

  private static func emitError(_ message: String) {
    emitJSON([
      "type": "error",
      "message": message,
    ])
  }

  private static func emitJSON(_ object: [String: Any]) {
    guard var data = try? JSONSerialization.data(withJSONObject: object) else {
      return
    }
    data.append(0x0A)
    try? FileHandle.standardOutput.write(contentsOf: data)
  }
}
