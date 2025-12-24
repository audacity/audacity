import QtQuick
import QtQuick.Particles
import Muse.Ui
import Muse.UiComponents

import Audacity.Playback 1.0

Item {
    id: root

    width: sled.width
    height: sled.height

    clip: false

    Image {
        id: sled
        source: "qrc:/images/sled.png"
        width: 53
        height: 32
        fillMode: Image.PreserveAspectFit
    }

    PlaybackStateModel {
        id: playbackState
    }

    ParticleSystem {
        id: snowSystem
    }

    Emitter {
        id: snowEmitter
        system: snowSystem
        enabled: playbackState.isPlaying
        x: sled.width / 2 - width / 2
        y: sled.height
        width: 20
        height: 4

        emitRate: 150
        lifeSpan: 2000
        lifeSpanVariation: 400

        velocity: AngleDirection {
            angle: 210
            angleVariation: 15
            magnitude: 40
            magnitudeVariation: 15
        }

        size: 3
        sizeVariation: 2
    }

    ImageParticle {
        id: snowParticle
        system: snowSystem
        source: "qrc:/images/snow.png"
        smooth: true

        alpha: 0.6
        alphaVariation: 0.3

        rotationVariation: 180
        rotationVelocityVariation: 90
    }

    Wander {
        system: snowSystem
        xVariance: 15
        pace: 60
    }

    Gravity {
        system: snowSystem
        angle: 90
        magnitude: 20
    }
}
