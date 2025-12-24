import QtQuick 2.15
import QtQuick.Particles 2.15

Item {
	id: root

	ParticleSystem {
		id: system
	}

	Emitter {
		id: snowEmitter
		system: system
		width: root.width
		height: 1
		emitRate: .02 * width

		lifeSpan: 6000
		lifeSpanVariation: 5000

		velocity: AngleDirection {
			angle: 90
			angleVariation: 40
			magnitude: 40
			magnitudeVariation: 30
		}

		size: 16
		sizeVariation: 10
	}

	ImageParticle {
		system: system
		source: "snowflake.png"
		smooth: true

		alpha: .3
		alphaVariation: .3

		rotationVariation: 45
		rotationVelocityVariation: 90
	}


	Wander {
		system: system
		xVariance: 30
		pace: 40
	}

	Gravity {
		system: system
		angle: 90
		magnitude: 20
	}
}
