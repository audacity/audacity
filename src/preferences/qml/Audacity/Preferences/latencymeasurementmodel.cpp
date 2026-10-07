/*
 * Audacity: A Digital Audio Editor
 */
#include "latencymeasurementmodel.h"

#include <QPointer>

#include "framework/global/translation.h"

using namespace au::appshell;
using Status = au::audio::LatencyMeasurement::Status;

namespace {
// Below this the test signal did not reach the input at all
constexpr float SILENT_PEAK = 0.01f;
constexpr float CLIPPING_PEAK = 0.99f;
// More spread than this means the input and output run on different clocks
constexpr double MAX_STABLE_SPREAD_MS = 1.0;

QString milliseconds(double ms)
{
    return muse::qtrc("preferences", "%1 ms").arg(ms, 0, 'f', 1);
}
}

LatencyMeasurementModel::LatencyMeasurementModel(QObject* parent)
    : QObject(parent), muse::Contextable(muse::iocCtxForQmlObject(this))
{
}

void LatencyMeasurementModel::start()
{
    if (m_state == State::Measuring) {
        return;
    }
    m_state = State::Measuring;
    emit changed();

    QPointer<LatencyMeasurementModel> self(this);
    latencyMeasurement()->measure(iocContext(), [self](const audio::LatencyMeasurement& result) {
        if (!self) {
            return;
        }
        self->m_result = result;
        self->m_state = result.status == Status::Measured ? State::Measured : State::Failed;
        emit self->changed();
    });
}

LatencyMeasurementModel::State LatencyMeasurementModel::state() const
{
    return m_state;
}

QString LatencyMeasurementModel::resultText() const
{
    if (m_state != State::Measured) {
        return {};
    }
    //: Measured round-trip latency, e.g. "Round trip: 42.8 ms (1889 samples at 44100 Hz)"
    return muse::qtrc("preferences", "Round trip: %1 (%2 samples at %3 Hz)")
           .arg(milliseconds(m_result.roundTripMs()))
           .arg(m_result.roundTripFrames)
           .arg(m_result.sampleRate, 0, 'f', 0);
}

QString LatencyMeasurementModel::reportedText() const
{
    if (m_state != State::Measured) {
        return {};
    }
    return muse::qtrc("preferences", "The audio driver reports %1 (input %2 + output %3).")
           .arg(milliseconds(m_result.reportedInputLatencyMs + m_result.reportedOutputLatencyMs))
           .arg(milliseconds(m_result.reportedInputLatencyMs))
           .arg(milliseconds(m_result.reportedOutputLatencyMs));
}

QString LatencyMeasurementModel::detailsText() const
{
    if (m_state != State::Measured) {
        return {};
    }
    return muse::qtrc("preferences", "%1 of %2 test signals came back.")
           .arg(m_result.signalsFound)
           .arg(m_result.signalsSent);
}

QString LatencyMeasurementModel::warningText() const
{
    if (m_state == State::Failed) {
        switch (m_result.status) {
        case Status::Busy:
            return muse::qtrc("preferences", "Stop playback and recording, then try again.");
        case Status::DeviceError:
            return muse::qtrc("preferences", "The audio devices could not be opened. Check the selected devices.");
        case Status::NoSignal:
            if (m_result.inputPeak < SILENT_PEAK) {
                return muse::qtrc("preferences",
                                  "No sound came back. Check the cable, or hold the microphone closer to the speaker and raise the volume.");
            }
            return muse::qtrc("preferences", "The test signal was not clear enough. Reduce background noise, or use a cable.");
        case Status::Measured:
            break;
        }
        return {};
    }

    if (m_state != State::Measured) {
        return {};
    }
    QStringList warnings;
    if (m_result.inputPeak >= CLIPPING_PEAK) {
        warnings << muse::qtrc("preferences", "The input was too loud. Lower the input level for a precise result.");
    }
    const double spreadMs = m_result.sampleRate > 0 ? 1000.0 * m_result.spreadFrames / m_result.sampleRate : 0.0;
    if (spreadMs > MAX_STABLE_SPREAD_MS) {
        warnings << muse::qtrc("preferences",
                               "The result changed by %1 between test signals. The input and output devices probably do not share a clock.")
            .arg(milliseconds(spreadMs));
    }
    return warnings.join('\n');
}

double LatencyMeasurementModel::compensationMs() const
{
    return m_state == State::Measured ? -m_result.roundTripMs() : 0.0;
}
