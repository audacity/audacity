import Foundation

public struct PlanarPCMHeader: Equatable, Sendable {
  public static let byteCount = 32
  static let magic = Data([0x53, 0x53, 0x44, 0x46, 0x4E, 0x33, 0x00, 0x00])
  static let version: UInt32 = 1

  public let sampleRate: Int
  public let channelCount: Int
  public let frameCount: Int

  public init(sampleRate: Int, channelCount: Int, frameCount: Int) throws {
    guard (8_000...384_000).contains(sampleRate) else {
      throw PlanarPCMError.invalidSampleRate(sampleRate)
    }
    guard (1...32).contains(channelCount) else {
      throw PlanarPCMError.invalidChannelCount(channelCount)
    }
    guard frameCount > 0 else {
      throw PlanarPCMError.invalidFrameCount(frameCount)
    }
    guard frameCount <= Int.max / channelCount / MemoryLayout<Float>.size else {
      throw PlanarPCMError.invalidFrameCount(frameCount)
    }

    self.sampleRate = sampleRate
    self.channelCount = channelCount
    self.frameCount = frameCount
  }

  var payloadByteCount: UInt64 {
    UInt64(frameCount) * UInt64(channelCount) * UInt64(MemoryLayout<Float>.size)
  }

  var encoded: Data {
    var data = Self.magic
    data.appendLittleEndian(Self.version)
    data.appendLittleEndian(UInt32(sampleRate))
    data.appendLittleEndian(UInt32(channelCount))
    data.appendLittleEndian(UInt32(0))
    data.appendLittleEndian(UInt64(frameCount))
    return data
  }

  static func decode(_ data: Data) throws -> PlanarPCMHeader {
    guard data.count == byteCount else {
      throw PlanarPCMError.truncatedHeader
    }
    guard data.prefix(magic.count) == magic else {
      throw PlanarPCMError.invalidMagic
    }

    let fileVersion = data.uint32LittleEndian(at: 8)
    guard fileVersion == version else {
      throw PlanarPCMError.unsupportedVersion(fileVersion)
    }
    let reserved = data.uint32LittleEndian(at: 20)
    guard reserved == 0 else {
      throw PlanarPCMError.nonzeroReservedField(reserved)
    }

    let rawFrameCount = data.uint64LittleEndian(at: 24)
    guard let frameCount = Int(exactly: rawFrameCount) else {
      throw PlanarPCMError.frameCountOutOfRange(rawFrameCount)
    }

    return try PlanarPCMHeader(
      sampleRate: Int(data.uint32LittleEndian(at: 12)),
      channelCount: Int(data.uint32LittleEndian(at: 16)),
      frameCount: frameCount
    )
  }
}

public enum PlanarPCMError: Error, LocalizedError, Equatable {
  case invalidMagic
  case unsupportedVersion(UInt32)
  case nonzeroReservedField(UInt32)
  case invalidSampleRate(Int)
  case invalidChannelCount(Int)
  case invalidFrameCount(Int)
  case frameCountOutOfRange(UInt64)
  case truncatedHeader
  case truncatedPayload(expected: UInt64, actual: UInt64)
  case unexpectedFileSize(expected: UInt64, actual: UInt64)
  case channelOutOfRange(Int)
  case channelLengthMismatch(expected: Int, actual: Int)
  case cannotCreateFile(String)

  public var errorDescription: String? {
    switch self {
    case .invalidMagic:
      return "The input is not a speech-swift planar PCM file"
    case .unsupportedVersion(let version):
      return "Unsupported planar PCM version: \(version)"
    case .nonzeroReservedField(let value):
      return "The planar PCM reserved field must be zero (found \(value))"
    case .invalidSampleRate(let rate):
      return "Invalid sample rate: \(rate)"
    case .invalidChannelCount(let count):
      return "Invalid channel count: \(count)"
    case .invalidFrameCount(let count):
      return "Invalid frame count: \(count)"
    case .frameCountOutOfRange(let count):
      return "Frame count is too large for this system: \(count)"
    case .truncatedHeader:
      return "The planar PCM header is truncated"
    case .truncatedPayload(let expected, let actual):
      return "The planar PCM payload is truncated (expected \(expected) bytes, read \(actual))"
    case .unexpectedFileSize(let expected, let actual):
      return "Unexpected planar PCM file size (expected \(expected) bytes, found \(actual))"
    case .channelOutOfRange(let index):
      return "Channel index is out of range: \(index)"
    case .channelLengthMismatch(let expected, let actual):
      return "Channel length mismatch (expected \(expected) frames, found \(actual))"
    case .cannotCreateFile(let path):
      return "Cannot create output file: \(path)"
    }
  }
}

public final class PlanarPCMReader {
  public let url: URL
  public let header: PlanarPCMHeader

  public init(url: URL) throws {
    self.url = url

    let handle = try FileHandle(forReadingFrom: url)
    defer { try? handle.close() }
    let headerData = try handle.readExactly(PlanarPCMHeader.byteCount)
    self.header = try PlanarPCMHeader.decode(headerData)

    let attributes = try FileManager.default.attributesOfItem(atPath: url.path)
    let actualSize = (attributes[.size] as? NSNumber)?.uint64Value ?? 0
    let expectedSize = UInt64(PlanarPCMHeader.byteCount) + header.payloadByteCount
    guard actualSize == expectedSize else {
      throw PlanarPCMError.unexpectedFileSize(expected: expectedSize, actual: actualSize)
    }
  }

  public func readChannel(_ index: Int) throws -> [Float] {
    guard (0..<header.channelCount).contains(index) else {
      throw PlanarPCMError.channelOutOfRange(index)
    }

    let byteCount = header.frameCount * MemoryLayout<Float>.size
    let offset = UInt64(PlanarPCMHeader.byteCount) + UInt64(index * byteCount)
    let handle = try FileHandle(forReadingFrom: url)
    defer { try? handle.close() }
    try handle.seek(toOffset: offset)
    let data = try handle.readExactly(byteCount)

    var samples = [Float](repeating: 0, count: header.frameCount)
    let copied = samples.withUnsafeMutableBytes { destination in
      data.copyBytes(to: destination)
    }
    guard copied == byteCount else {
      throw PlanarPCMError.truncatedPayload(
        expected: UInt64(byteCount), actual: UInt64(copied))
    }
    return samples
  }
}

public final class PlanarPCMOutput {
  public let header: PlanarPCMHeader
  private let handle: FileHandle
  private var writtenChannels = 0

  public init(url: URL, header: PlanarPCMHeader) throws {
    self.header = header
    let fileManager = FileManager.default
    try fileManager.createDirectory(
      at: url.deletingLastPathComponent(),
      withIntermediateDirectories: true
    )
    if !fileManager.fileExists(atPath: url.path) {
      guard
        fileManager.createFile(
          atPath: url.path,
          contents: nil,
          attributes: [.posixPermissions: 0o600]
        )
      else {
        throw PlanarPCMError.cannotCreateFile(url.path)
      }
    }
    self.handle = try FileHandle(forWritingTo: url)
    try handle.truncate(atOffset: 0)
    try handle.write(contentsOf: header.encoded)
  }

  deinit {
    try? handle.close()
  }

  public func appendChannel(_ samples: [Float]) throws {
    guard samples.count == header.frameCount else {
      throw PlanarPCMError.channelLengthMismatch(
        expected: header.frameCount, actual: samples.count)
    }
    guard writtenChannels < header.channelCount else {
      throw PlanarPCMError.channelOutOfRange(writtenChannels)
    }

    let samplesPerWrite = 1 << 20
    try samples.withUnsafeBytes { bytes in
      var offset = 0
      while offset < bytes.count {
        let end = min(offset + samplesPerWrite * MemoryLayout<Float>.size, bytes.count)
        try handle.write(contentsOf: Data(bytes[offset..<end]))
        offset = end
      }
    }
    writtenChannels += 1
  }

  public func close() throws {
    guard writtenChannels == header.channelCount else {
      throw PlanarPCMError.channelLengthMismatch(
        expected: header.channelCount, actual: writtenChannels)
    }
    try handle.close()
  }
}

extension FileHandle {
  fileprivate func readExactly(_ byteCount: Int) throws -> Data {
    var result = Data()
    result.reserveCapacity(byteCount)
    while result.count < byteCount {
      let remaining = byteCount - result.count
      guard let chunk = try read(upToCount: remaining), !chunk.isEmpty else {
        throw PlanarPCMError.truncatedPayload(
          expected: UInt64(byteCount), actual: UInt64(result.count))
      }
      result.append(chunk)
    }
    return result
  }
}

extension Data {
  fileprivate mutating func appendLittleEndian<T: FixedWidthInteger>(_ value: T) {
    var littleEndian = value.littleEndian
    Swift.withUnsafeBytes(of: &littleEndian) { append(contentsOf: $0) }
  }

  fileprivate func uint32LittleEndian(at offset: Int) -> UInt32 {
    let value = self[offset..<(offset + MemoryLayout<UInt32>.size)]
      .withUnsafeBytes { $0.loadUnaligned(as: UInt32.self) }
    return UInt32(littleEndian: value)
  }

  fileprivate func uint64LittleEndian(at offset: Int) -> UInt64 {
    let value = self[offset..<(offset + MemoryLayout<UInt64>.size)]
      .withUnsafeBytes { $0.loadUnaligned(as: UInt64.self) }
    return UInt64(littleEndian: value)
  }
}
