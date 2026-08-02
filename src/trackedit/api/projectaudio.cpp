/*
 * Audacity: A Digital Audio Editor
 */
#include "internal/projecteditstate.h"
#include "projectaudio.h"

#include <algorithm>
#include <cmath>
#include <cstring>
#include <utility>

#include <QVariantList>

#include "au3-audio-graph/AudioGraphBuffers.h"
#include "au3-audio-graph/AudioGraphSource.h"
#include "au3-audio-graph/AudioGraphTask.h"
#include "au3-math/SampleFormat.h"
#include "au3-mixer/Mix.h"
#include "au3-mixer/WideSampleSource.h"
#include "au3-stretching-sequence/StretchingSequence.h"
#include "au3-track/Track.h"
#include "au3-wave-track/TimeStretching.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"
#include "au3-wave-track/WaveTrackSink.h"

#include "audiotransform.h"
#include "extensions/native/bytebuffer.h"

namespace au::trackedit::api::detail {
std::optional<AudioFormat> audioFormat(const QVariantMap& value)
{
    AudioFormat result{
        value.value(QStringLiteral("channelCount")).toUInt(),
        value.value(QStringLiteral("sampleRate")).toDouble(),
    };
    constexpr double maxSampleRate = 384000.0;
    if ((result.channelCount != 1 && result.channelCount != 2) || !std::isfinite(result.sampleRate) || result.sampleRate < 1.0
        || result.sampleRate > maxSampleRate || result.sampleRate != std::floor(result.sampleRate)) {
        return std::nullopt;
    }
    return result;
}

class TransformSource final : public AudioGraph::Source
{
public:
    TransformSource(AudioGraph::Source& source, AudioTransform& transform, uint32_t channelCount)
        : m_source(source), m_transform(transform), m_channelCount(channelCount)
    {
    }

    bool AcceptsBuffers(const Buffers& buffers) const override
    {
        return buffers.Channels() == m_channelCount && m_source.AcceptsBuffers(buffers);
    }

    bool AcceptsBlockSize(size_t blockSize) const override
    {
        return m_source.AcceptsBlockSize(blockSize);
    }

    std::optional<size_t> Acquire(Buffers& buffers, size_t bound) override
    {
        const auto count = m_source.Acquire(buffers, bound);
        if (!count || *count == 0) {
            return count;
        }
        float* channels[2]{};
        for (uint32_t channel = 0; channel < m_channelCount; ++channel) {
            channels[channel] = &buffers.GetWritePosition(channel);
        }
        return m_transform.apply(channels, m_channelCount, *count) ? count : std::nullopt;
    }

    sampleCount Remaining() const override
    {
        return m_source.Remaining();
    }

    bool Release() override
    {
        return m_source.Release();
    }

private:
    AudioGraph::Source& m_source;
    AudioTransform& m_transform;
    uint32_t m_channelCount = 0;
};

bool reportProgress(AudioTransformTask* task, double fraction)
{
    return !task || task->report(fraction, {});
}

AudioChunkObject::AudioChunkObject(AudioFormat format, size_t samples, QObject* parent)
    : QObject(parent), m_format(format), m_samples(samples)
{
    const size_t bytes = samples * sizeof(float);
    for (uint32_t channel = 0; channel < format.channelCount; ++channel) {
        auto* buffer = new muse::extensions::ByteBuffer(bytes, this);
        m_buffers.push_back(buffer);
        m_channels.push_back(QVariant::fromValue<QObject*>(buffer));
    }
}

uint AudioChunkObject::channelCount() const
{
    return m_format.channelCount;
}

qulonglong AudioChunkObject::sampleCount() const
{
    return m_samples;
}

double AudioChunkObject::sampleRate() const
{
    return m_format.sampleRate;
}

QVariantList AudioChunkObject::channels() const
{
    return m_channels;
}

const AudioFormat& AudioChunkObject::format() const
{
    return m_format;
}

muse::extensions::ByteBuffer* AudioChunkObject::buffer(size_t channel) const
{
    return m_buffers.at(channel);
}

bool AudioChunkObject::released() const
{
    return m_released;
}

bool AudioChunkObject::reusable() const
{
    return std::all_of(m_buffers.begin(), m_buffers.end(), [](const muse::extensions::ByteBuffer* buffer) {
        return buffer->isExclusive();
    });
}

void AudioChunkObject::reuse()
{
    m_released = false;
}

void AudioChunkObject::release()
{
    m_released = true;
}

AudioReaderObject::AudioReaderObject(std::shared_ptr<EditState> state, std::shared_ptr<WaveTrack> source, AudioFormat format, double start,
                                     double end, QObject* parent)
    : QObject(parent), m_state(std::move(state)), m_source(std::move(source)), m_format(format)
{
    const bool matchingFormat = m_format.channelCount == m_source->NChannels() && m_format.sampleRate == m_source->GetRate();
    const bool stretched = TimeStretching::HasPitchOrSpeed(*m_source, start, end);
    if (matchingFormat && !stretched) {
        m_position = m_source->TimeToLongSamples(start);
        m_end = m_source->TimeToLongSamples(end);
    } else {
        Mixer::Inputs inputs;
        inputs.emplace_back(StretchingSequence::Create(*m_source, m_source->GetClipInterfaces()), Mixer::Stages {});
        m_mixer = std::make_unique<Mixer>(
            std::move(inputs), std::nullopt, true, Mixer::WarpOptions { m_state->project.get() }, start, end, format.channelCount,
            AUDIO_CHUNK_SAMPLES, false, format.sampleRate, floatSample, true, nullptr, Mixer::ApplyVolume::Mixdown);
    }
}

AudioReaderObject::~AudioReaderObject() = default;

uint AudioReaderObject::channelCount() const
{
    return m_format.channelCount;
}

double AudioReaderObject::sampleRate() const
{
    return m_format.sampleRate;
}

QObject* AudioReaderObject::read()
{
    return read(AUDIO_CHUNK_SAMPLES);
}

QObject* AudioReaderObject::read(qulonglong requested)
{
    if (!m_state->usable()) {
        throwProjectError(this, QStringLiteral("The project edit is no longer active"));
        return nullptr;
    }
    if (requested == 0) {
        throwProjectError(this, QStringLiteral("Audio reads must request at least one sample"));
        return nullptr;
    }
    if (m_chunk && !m_chunk->released()) {
        throwProjectError(this, QStringLiteral("Release the previous audio chunk before reading again"));
        return nullptr;
    }

    try {
        const size_t limit = static_cast<size_t>(std::min<qulonglong>(requested, AUDIO_CHUNK_SAMPLES));
        size_t count = 0;
        if (m_mixer) {
            count = m_mixer->Process(limit);
        } else {
            count = limitSampleBufferSize(limit, m_end - m_position);
        }
        if (count == 0) {
            return nullptr;
        }
        if (!m_chunk || m_chunk->sampleCount() != count || !m_chunk->reusable()) {
            delete m_chunk;
            m_chunk = nullptr;
            m_chunk = new AudioChunkObject(m_format, count, this);
        } else {
            m_chunk->reuse();
        }
        if (m_mixer) {
            for (uint32_t channel = 0; channel < m_format.channelCount; ++channel) {
                std::memcpy(m_chunk->buffer(channel)->data(), m_mixer->GetBuffer(channel), count * sizeof(float));
            }
        } else {
            float* buffers[2]{};
            for (uint32_t channel = 0; channel < m_format.channelCount; ++channel) {
                buffers[channel] = static_cast<float*>(m_chunk->buffer(channel)->data());
            }
            if (!m_source->GetFloats(0, m_format.channelCount, buffers, m_position, count)) {
                m_chunk->release();
                throwProjectError(this, QStringLiteral("Could not read project audio"));
                return nullptr;
            }
            m_position += count;
        }
        return m_chunk;
    } catch (const std::exception& exception) {
        throwProjectError(this, QString::fromUtf8(exception.what()));
    } catch (...) {
        throwProjectError(this, QStringLiteral("Could not read project audio"));
    }
    return nullptr;
}

AudioClipObject::AudioClipObject(std::shared_ptr<EditState> state, size_t trackIndex, size_t clipIndex, QObject* parent)
    : QObject(parent), m_state(std::move(state)), m_trackIndex(trackIndex), m_clipIndex(clipIndex)
{
}

double AudioClipObject::start() const
{
    return source().start;
}

double AudioClipObject::end() const
{
    return source().end;
}

QString AudioClipObject::title() const
{
    return QString::fromStdString(source().title);
}

bool AudioClipObject::updateTitle(const QString& title)
{
    if (!m_state->writable()) {
        throwProjectError(this, QStringLiteral("The audio clip cannot be updated"));
        return false;
    }
    const auto matches = [&](const ClipChange& change) {
        return change.trackIndex == m_trackIndex && change.nativeId == source().nativeId;
    };
    auto found = std::find_if(m_state->clipChanges.begin(), m_state->clipChanges.end(), matches);
    if (found == m_state->clipChanges.end()) {
        m_state->clipChanges.push_back({ m_trackIndex, source().nativeId, title.toStdString() });
    } else {
        found->title = title.toStdString();
    }
    return true;
}

const ProjectAudioClip& AudioClipObject::source() const
{
    return m_state->audioTracks.at(m_trackIndex).clips.at(m_clipIndex);
}

AudioTrackObject::AudioTrackObject(std::shared_ptr<EditState> state, size_t index, bool selectionOnly, QObject* parent)
    : QObject(parent), m_state(std::move(state)), m_index(index), m_selectionOnly(selectionOnly)
{
}

QString AudioTrackObject::name() const
{
    return QString::fromStdString(input().name);
}

uint AudioTrackObject::channelCount() const
{
    return static_cast<uint>(input().source->NChannels());
}

double AudioTrackObject::sampleRate() const
{
    return input().source->GetRate();
}

double AudioTrackObject::start() const
{
    return input().source->GetStartTime();
}

double AudioTrackObject::end() const
{
    return input().source->GetEndTime();
}

QVariantList AudioTrackObject::clips() const
{
    QVariantList result;
    for (size_t index = 0; index < input().clips.size(); ++index) {
        const auto& clip = input().clips[index];
        if (m_selectionOnly) {
            if (m_state->hasSelectedClips()) {
                if (!clip.selected) {
                    continue;
                }
            } else if (clip.end < m_state->selectionStart || clip.start > m_state->selectionEnd) {
                continue;
            }
        }
        QObject*& wrapper = m_clipWrappers[index];
        if (!wrapper) {
            wrapper = new AudioClipObject(m_state, m_index, index, const_cast<AudioTrackObject*>(this));
        }
        result.push_back(QVariant::fromValue<QObject*>(wrapper));
    }
    return result;
}

size_t AudioTrackObject::index() const
{
    return m_index;
}

const std::shared_ptr<EditState>& AudioTrackObject::state() const
{
    return m_state;
}

QObject* AudioTrackObject::openReader(const QVariantMap& options)
{
    if (!m_state->usable()) {
        throwProjectError(this, QStringLiteral("The project edit is no longer active"));
        return nullptr;
    }
    const auto format = audioFormat(options);
    const double rangeStart = options.value(QStringLiteral("start")).toDouble();
    const double rangeEnd = options.value(QStringLiteral("end")).toDouble();
    if (!format || !validTimes(rangeStart, rangeEnd)) {
        throwProjectError(this, QStringLiteral("Invalid audio reader options"));
        return nullptr;
    }
    try {
        return new AudioReaderObject(m_state, input().source, *format, rangeStart, rangeEnd, this);
    } catch (const std::exception& exception) {
        throwProjectError(this, QString::fromUtf8(exception.what()));
    } catch (...) {
        throwProjectError(this, QStringLiteral("Could not open the audio reader"));
    }
    return nullptr;
}

const ProjectAudioTrack& AudioTrackObject::input() const
{
    return m_state->audioTracks.at(m_index);
}

AudioWriterObject::AudioWriterObject(std::shared_ptr<EditState> state, AudioFormat format, std::shared_ptr<WaveTrack> audio,
                                     QObject* parent)
    : QObject(parent), m_state(std::move(state)), m_format(format), m_audio(std::move(audio))
{
}

uint AudioWriterObject::channelCount() const
{
    return m_format.channelCount;
}

double AudioWriterObject::sampleRate() const
{
    return m_format.sampleRate;
}

QObject* AudioWriterObject::createChunk(qulonglong samples)
{
    if (!m_state->writable() || samples > AUDIO_CHUNK_SAMPLES) {
        throwProjectError(this, QStringLiteral("Invalid audio chunk size"));
        return nullptr;
    }
    try {
        return new AudioChunkObject(m_format, static_cast<size_t>(samples), this);
    } catch (...) {
        throwProjectError(this, QStringLiteral("Could not allocate the audio chunk"));
        return nullptr;
    }
}

bool AudioWriterObject::write(QObject* object)
{
    auto* chunk = qobject_cast<AudioChunkObject*>(object);
    if (!m_state->writable() || m_routed || !chunk || chunk->released() || chunk->format().channelCount != m_format.channelCount
        || chunk->format().sampleRate != m_format.sampleRate) {
        throwProjectError(this, QStringLiteral("Invalid audio write"));
        return false;
    }
    for (uint32_t channel = 0; channel < m_format.channelCount; ++channel) {
        if (!chunk->buffer(channel)->isExclusive()) {
            throwProjectError(this, QStringLiteral("Audio is in use by a native call"));
            return false;
        }
    }
    if (chunk->sampleCount() == 0) {
        return true;
    }

    for (uint32_t channel = 0; channel < m_format.channelCount; ++channel) {
        const auto* samples = static_cast<const float*>(chunk->buffer(channel)->data());
        if (!std::all_of(samples, samples + chunk->sampleCount(), [](float sample) {
            return std::isfinite(sample);
        })) {
            throwProjectError(this, QStringLiteral("Audio contains non-finite samples"));
            return false;
        }
    }

    try {
        constSamplePtr buffers[2]{};
        for (uint32_t channel = 0; channel < m_format.channelCount; ++channel) {
            buffers[channel] = reinterpret_cast<constSamplePtr>(chunk->buffer(channel)->data());
        }
        m_audio->RightmostOrNewClip()->Append(buffers, floatSample, static_cast<size_t>(chunk->sampleCount()), 1, floatSample);
        return true;
    } catch (...) {
        throwProjectError(this, QStringLiteral("Could not write project audio"));
        return false;
    }
}

bool AudioWriterObject::replace(QObject* object, double start, double end)
{
    auto* input = qobject_cast<AudioTrackObject*>(object);
    if (!m_state->writable() || m_routed || !input || input->state() != m_state || !validTimes(start, end)) {
        throwProjectError(this, QStringLiteral("Invalid audio replacement"));
        return false;
    }
    const auto& destination = m_state->audioTracks.at(input->index()).destination;
    if (!destination || destination->NChannels() != m_format.channelCount
        || std::any_of(m_state->replacements.begin(), m_state->replacements.end(), [&](const Replacement& replacement) {
        return replacement.destination == destination;
    })) {
        throwProjectError(this, QStringLiteral("The audio track cannot be replaced"));
        return false;
    }
    try {
        m_audio->Flush();
        m_state->replacements.push_back({
                destination,
                m_audio,
                start,
                end,
                {},
                false,
            });
        route();
        return true;
    } catch (...) {
        throwProjectError(this, QStringLiteral("Could not stage replacement audio"));
        return false;
    }
}

bool AudioWriterObject::addTrack(const QString& name, double start)
{
    if (!m_state->writable() || m_routed || !std::isfinite(start) || start < 0.0) {
        throwProjectError(this, QStringLiteral("Invalid audio track"));
        return false;
    }
    try {
        m_audio->Flush();
        const auto& destination = m_state->preferredAudioDestination;
        const bool destinationUsed = destination
                                     && std::any_of(m_state->replacements.begin(), m_state->replacements.end(),
                                                    [&](const Replacement& replacement) {
            return replacement.destination == destination;
        });
        if (destination && !destinationUsed) {
            m_state->replacements.push_back({
                    destination,
                    m_audio,
                    start,
                    start,
                    name.toStdString(),
                    true,
                });
        } else {
            m_state->addedAudio.push_back({
                    name.toStdString(),
                    start,
                    m_audio,
                });
        }
        route();
        return true;
    } catch (...) {
        throwProjectError(this, QStringLiteral("Could not stage the audio track"));
        return false;
    }
}

void AudioWriterObject::route()
{
    m_routed = true;
    ++m_state->routedWriters;
}

QObject* makeAudioTrack(std::shared_ptr<EditState> state, size_t index, bool selectionOnly, QObject* parent)
{
    return new AudioTrackObject(std::move(state), index, selectionOnly, parent);
}

std::optional<size_t> audioTrackIndex(const std::shared_ptr<EditState>& state, QObject* object)
{
    auto* track = qobject_cast<AudioTrackObject*>(object);
    return track && track->state() == state ? std::optional<size_t> { track->index() } : std::nullopt;
}

QObject* makeAudioWriter(std::shared_ptr<EditState> state, AudioFormat format, std::shared_ptr<WaveTrack> audio, QObject* parent)
{
    return new AudioWriterObject(std::move(state), format, std::move(audio), parent);
}

bool transformAudio(const std::shared_ptr<EditState>& state, QObject* object, const QVariantMap& options, QObject* processor, QObject* task,
                    QObject* errorContext)
{
    auto* track = qobject_cast<AudioTrackObject*>(object);
    auto* transform = dynamic_cast<AudioTransform*>(processor);
    const double start = options.value(QStringLiteral("start")).toDouble();
    const double end = options.value(QStringLiteral("end")).toDouble();
    const double progressStart = options.value(QStringLiteral("progressStart"), 0.0).toDouble();
    const double progressEnd = options.value(QStringLiteral("progressEnd"), 1.0).toDouble();
    if (!state->writable() || !track || track->state() != state || !transform
        || !validTimes(start,
                       end) || !std::isfinite(progressStart) || !std::isfinite(progressEnd) || progressStart < 0.0
        || progressEnd < progressStart
        || progressEnd > 1.0) {
        throwProjectError(errorContext, QStringLiteral("Invalid audio transform"));
        return false;
    }

    auto* progressTask = dynamic_cast<AudioTransformTask*>(task);
    if (task && !progressTask) {
        throwProjectError(errorContext, QStringLiteral("Invalid progress task"));
        return false;
    }

    const auto& input = state->audioTracks.at(track->index());
    const auto& destination = input.destination;
    if (!destination || input.source->NChannels() > 2
        || std::any_of(state->replacements.begin(), state->replacements.end(), [&](const Replacement& replacement) {
        return replacement.destination == destination;
    })) {
        throwProjectError(errorContext, QStringLiteral("The audio track cannot be transformed"));
        return false;
    }

    try {
        auto output = std::static_pointer_cast<WaveTrack>(input.source->Duplicate(Track::DuplicateOptions {}.Backup()));
        const double outputStart = output->GetStartTime();
        const double rangeStart = std::max(start, output->GetStartTime());
        const double rangeEnd = std::min(end, output->GetEndTime());
        if (rangeEnd > rangeStart) {
            const sampleCount first = output->TimeToLongSamples(rangeStart);
            const sampleCount last = output->TimeToLongSamples(rangeEnd);
            const sampleCount length = last - first;
            const uint32_t channelCount = static_cast<uint32_t>(output->NChannels());
            const size_t blockSize = std::max<size_t>(1, output->GetMaxBlockSize() * 2);
            AudioGraph::Buffers buffers(channelCount, blockSize, 1);
            WideSampleSource source(*output, channelCount, first, length, [&](sampleCount position) {
                const double fraction = length > 0 ? (position - first).as_double() / length.as_double() : 1.0;
                return reportProgress(progressTask, progressStart + std::clamp(fraction, 0.0, 1.0) * (progressEnd - progressStart));
            });
            TransformSource transformed(source, *transform, channelCount);
            const auto channels = output->Channels();
            WaveTrackSink sink(**channels.begin(), channelCount == 2 ? (*channels.rbegin()).get() : nullptr, nullptr, first, true,
                               widestSampleFormat);
            AudioGraph::Task graph(transformed, buffers, sink);
            if (!graph.RunLoop()) {
                if (progressTask && progressTask->cancelled()) {
                    return false;
                }
                throwProjectError(errorContext, QStringLiteral("Could not transform project audio"));
                return false;
            }
            sink.Flush(buffers);
            if (!sink.IsOk()) {
                throwProjectError(errorContext, QStringLiteral("Could not write transformed audio"));
                return false;
            }
        } else if (!reportProgress(progressTask, progressEnd)) {
            return false;
        }
        state->replacements.push_back({
                destination,
                std::move(output),
                outputStart,
                0.0,
                {},
                true,
            });
        return true;
    } catch (const std::exception& exception) {
        throwProjectError(errorContext, QString::fromUtf8(exception.what()));
    } catch (...) {
        throwProjectError(errorContext, QStringLiteral("Could not transform project audio"));
    }
    return false;
}
} // namespace au::trackedit::api::detail
