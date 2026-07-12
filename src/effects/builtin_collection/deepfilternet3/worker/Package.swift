// swift-tools-version: 5.10

import Foundation
import PackageDescription

let speechSwiftDependency: Package.Dependency
if let localPath = Context.environment["SPEECH_SWIFT_PACKAGE_PATH"], !localPath.isEmpty {
  speechSwiftDependency = .package(path: localPath)
} else {
  speechSwiftDependency = .package(
    url: "https://github.com/soniqo/speech-swift.git",
    exact: "0.0.21"
  )
}

let package = Package(
  name: "AudacitySpeechSwiftWorker",
  platforms: [
    .macOS("15.0")
  ],
  products: [
    .executable(
      name: "audacity-speech-swift-worker",
      targets: ["AudacitySpeechSwiftWorker"]
    )
  ],
  dependencies: [
    speechSwiftDependency
  ],
  targets: [
    .target(
      name: "AudacitySpeechSwiftWorkerCore",
      dependencies: [
        .product(name: "AudioCommon", package: "speech-swift"),
        .product(name: "SpeechEnhancement", package: "speech-swift"),
      ]
    ),
    .executableTarget(
      name: "AudacitySpeechSwiftWorker",
      dependencies: ["AudacitySpeechSwiftWorkerCore"]
    ),
    .testTarget(
      name: "AudacitySpeechSwiftWorkerCoreTests",
      dependencies: ["AudacitySpeechSwiftWorkerCore"]
    ),
  ]
)
