import AudioCommon
import Foundation
import SpeechEnhancement

public enum WorkerEvent: Equatable, Sendable {
  case progress(fraction: Double, message: String)
  case completed(jobCount: Int, channelCount: Int)
}

public enum DeepFilterWorkerError: Error, LocalizedError {
  case emptyEnhancementOutput

  public var errorDescription: String? {
    switch self {
    case .emptyEnhancementOutput:
      return "DeepFilterNet3 returned no audio"
    }
  }
}

public enum DeepFilterWorker {
  public static func run(
    options: WorkerOptions,
    eventHandler: @escaping (WorkerEvent) -> Void
  ) async throws {
    let readers = try options.jobs.map { try PlanarPCMReader(url: $0.input) }
    let totalChannels = readers.reduce(0) { $0 + $1.header.channelCount }
    var processedChannels = 0

    eventHandler(.progress(fraction: 0, message: "Loading DeepFilterNet3…"))
    let modelId = options.modelId ?? SpeechEnhancer.defaultModelId
    let enhancer = try await SpeechEnhancer.fromPretrained(
      modelId: modelId,
      cacheDir: try resolveModelCacheDirectory(
        modelId: modelId,
        cacheRoot: options.cacheRoot
      ),
      offlineMode: options.offlineMode
    ) { fraction, message in
      eventHandler(
        .progress(
          fraction: min(max(fraction, 0), 1) * 0.35,
          message: message
        ))
    }

    for (jobIndex, job) in options.jobs.enumerated() {
      let reader = readers[jobIndex]
      let output = try PlanarPCMOutput(url: job.output, header: reader.header)

      for channelIndex in 0..<reader.header.channelCount {
        let fraction = 0.35 + 0.65 * Double(processedChannels) / Double(totalChannels)
        eventHandler(
          .progress(
            fraction: fraction,
            message: "Enhancing channel \(processedChannels + 1) of \(totalChannels)…"
          ))

        let input = try reader.readChannel(channelIndex)
        let inputAt48k: [Float]
        if reader.header.sampleRate == SpeechEnhancer.sampleRate {
          inputAt48k = input
        } else {
          inputAt48k = AudioFileLoader.resample(
            input,
            from: reader.header.sampleRate,
            to: SpeechEnhancer.sampleRate
          )
        }

        let enhancedAt48k = try enhancer.enhanceChunked(
          audio: inputAt48k,
          sampleRate: SpeechEnhancer.sampleRate,
          chunkSeconds: options.chunkSeconds,
          overlapMs: options.overlapMilliseconds
        )
        guard !enhancedAt48k.isEmpty else {
          throw DeepFilterWorkerError.emptyEnhancementOutput
        }

        let restoredRate: [Float]
        if reader.header.sampleRate == SpeechEnhancer.sampleRate {
          restoredRate = enhancedAt48k
        } else {
          restoredRate = AudioFileLoader.resample(
            enhancedAt48k,
            from: SpeechEnhancer.sampleRate,
            to: reader.header.sampleRate
          )
        }
        try output.appendChannel(
          normalizeSampleCount(
            restoredRate,
            expectedCount: reader.header.frameCount
          ))
        processedChannels += 1
      }
      try output.close()
    }

    eventHandler(.progress(fraction: 1, message: "DeepFilterNet3 complete"))
    eventHandler(.completed(jobCount: options.jobs.count, channelCount: totalChannels))
  }
}

func resolveModelCacheDirectory(modelId: String, cacheRoot: URL?) throws -> URL? {
  guard let cacheRoot else {
    return nil
  }
  return try HuggingFaceDownloader.getCacheDirectory(
    for: modelId,
    basePath: cacheRoot
  )
}

func normalizeSampleCount(_ samples: [Float], expectedCount: Int) -> [Float] {
  if samples.count == expectedCount {
    return samples
  }
  if samples.count > expectedCount {
    return Array(samples.prefix(expectedCount))
  }
  return samples + [Float](repeating: 0, count: expectedCount - samples.count)
}
