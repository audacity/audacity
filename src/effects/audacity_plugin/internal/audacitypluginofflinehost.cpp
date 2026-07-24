/*
 * Audacity: A Digital Audio Editor
 */
#include "audacitypluginofflinehost.h"

#include <algorithm>
#include <array>
#include <limits>
#include <utility>

#include "au3-math/SampleCount.h"
#include "au3-math/SampleFormat.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"

namespace au::effects {
namespace {
struct ProcessingCancelled {};
struct ConversionValidationFailed {};

bool convertedSampleCount(uint64_t inputSamples, uint32_t inputRate,
                          uint32_t outputRate, uint64_t& outputSamples) noexcept
{
    outputSamples = 0;
    if (inputRate == 0 || outputRate == 0) {
        return false;
    }
    if (inputSamples == 0) {
        return true;
    }

    const uint64_t quotient = inputSamples / inputRate;
    const uint64_t remainder = inputSamples % inputRate;
    if (quotient > std::numeric_limits<uint64_t>::max() / outputRate) {
        return false;
    }

    const uint64_t base = quotient * outputRate;
    const uint64_t tail
        = (remainder * outputRate + inputRate / 2u) / inputRate;
    if (base > std::numeric_limits<uint64_t>::max() - tail) {
        return false;
    }
    outputSamples = base + tail;
    return true;
}

void appendSamples(WaveTrack& track, const au::audacityplugin::AudioChunk& audio)
{
    constSamplePtr buffers[2] {};
    for (uint32_t channel = 0; channel < audio.channelCount; ++channel) {
        buffers[channel] = reinterpret_cast<constSamplePtr>(audio.channels[channel]);
    }
    track.RightmostOrNewClip()->Append(
        buffers, floatSample, static_cast<size_t>(audio.sampleCount), 1, floatSample);
}
} // namespace

AudacityPluginOfflineHost::AudacityPluginOfflineHost(
    WaveTrackFactory& factory,
    std::vector<AudacityPluginSelectedAudio> selectedAudio,
    const std::vector<au::audacityplugin::LabelTrack>& labelTracks,
    std::function<bool(double, const std::string&)> progress,
    std::function<bool()> cancelled)
    : m_factory(factory)
    , m_selectedAudio(std::move(selectedAudio))
    , m_progress(std::move(progress))
    , m_cancelled(std::move(cancelled))
{
    for (size_t index = 0; index < m_selectedAudio.size(); ++index) {
        auto& selected = m_selectedAudio[index];
        m_nextTrack = std::max(m_nextTrack, selected.track);
        m_trackIndices.emplace(selected.track, index);
        if (!selected.selectionTrack || selected.sampleCount == 0) {
            continue;
        }
        selected.audio = ++m_nextAudio;
        m_audio.emplace(
            selected.audio,
            Audio { std::move(selected.selectionTrack), selected.sampleCount, false });
    }
    for (const auto& track : labelTracks) {
        m_nextTrack = std::max(m_nextTrack, track.track);
        m_labelTrackHandles.insert(track.track);
        for (const auto& label : track.labels) {
            m_nextLabel = std::max(m_nextLabel, label.label);
            m_existingLabels.emplace(label.label, track.track);
        }
    }
}

const std::vector<AudacityPluginSelectedAudio>&
AudacityPluginOfflineHost::selectedAudio() const noexcept
{
    return m_selectedAudio;
}

au::audacityplugin::Status AudacityPluginOfflineHost::convertAudio(
    au::audacityplugin::AudioHandle audio,
    const au::audacityplugin::AudioFormat& format,
    au::audacityplugin::AudioHandle& converted,
    au::audacityplugin::AudioInfo& info)
{
    const auto iterator = m_audio.find(audio);
    if (iterator == m_audio.end() || iterator->second.writable) {
        return au::audacityplugin::Status::InvalidState;
    }
    try {
        auto convertedAudio = convertTrack(iterator->second, format);
        const auto convertedInfo = au::audacityplugin::AudioInfo {
            { static_cast<uint32_t>(convertedAudio.track->NChannels()),
              static_cast<uint32_t>(convertedAudio.track->GetRate()) },
            convertedAudio.sampleCount,
        };
        const auto handle = ++m_nextAudio;
        m_audio.emplace(handle, std::move(convertedAudio));
        converted = handle;
        info = convertedInfo;
        return au::audacityplugin::Status::Ok;
    } catch (const ProcessingCancelled&) {
        return au::audacityplugin::Status::Cancelled;
    } catch (const ConversionValidationFailed&) {
        return au::audacityplugin::Status::ValidationFailed;
    }
}

au::audacityplugin::Status AudacityPluginOfflineHost::readAudio(
    au::audacityplugin::AudioHandle audio,
    uint64_t sampleOffset, uint64_t requestedSamples,
    au::audacityplugin::AudioChunk& chunk)
{
    const auto iterator = m_audio.find(audio);
    if (iterator == m_audio.end() || iterator->second.writable) {
        return au::audacityplugin::Status::InvalidState;
    }

    const auto& audioData = iterator->second;
    if (sampleOffset > audioData.sampleCount) {
        return au::audacityplugin::Status::InvalidArgument;
    }
    if (sampleOffset == audioData.sampleCount) {
        return au::audacityplugin::Status::Ok;
    }

    const auto cursor = sampleCount {
        static_cast<sampleCount::type>(sampleOffset)
    };
    const auto available = std::min({
            requestedSamples,
            audioData.sampleCount - sampleOffset,
            static_cast<uint64_t>(audioData.track->GetBestBlockSize(cursor))
        });
    const auto channelCount = static_cast<uint32_t>(audioData.track->NChannels());
    const auto samples = static_cast<size_t>(available);
    std::array<float*, 2> buffers {};
    for (uint32_t channel = 0; channel < channelCount; ++channel) {
        m_readBuffers[channel].resize(samples);
        buffers[channel] = m_readBuffers[channel].data();
    }
    if (!audioData.track->GetFloats(
            0, channelCount, buffers.data(), cursor, samples)) {
        return au::audacityplugin::Status::HostError;
    }
    for (uint32_t channel = 0; channel < channelCount; ++channel) {
        chunk.channels[channel] = m_readBuffers[channel].data();
    }
    chunk.channelCount = channelCount;
    chunk.sampleCount = available;
    return au::audacityplugin::Status::Ok;
}

au::audacityplugin::Status AudacityPluginOfflineHost::createAudio(
    const au::audacityplugin::AudioFormat& format,
    au::audacityplugin::AudioHandle& audio)
{
    auto track = m_factory.Create(format.channelCount, floatSample,
                                  format.sampleRate);
    const auto handle = ++m_nextAudio;
    m_audio.emplace(handle, Audio { std::move(track), 0, true });
    audio = handle;
    return au::audacityplugin::Status::Ok;
}

au::audacityplugin::Status AudacityPluginOfflineHost::writeAudio(
    au::audacityplugin::AudioHandle handle,
    const au::audacityplugin::AudioChunk& chunk)
{
    const auto iterator = m_audio.find(handle);
    if (iterator == m_audio.end() || !iterator->second.writable) {
        return au::audacityplugin::Status::InvalidState;
    }
    auto& audio = iterator->second;
    if (chunk.channelCount != audio.track->NChannels()) {
        return au::audacityplugin::Status::InvalidArgument;
    }
    if (chunk.sampleCount == 0) {
        return au::audacityplugin::Status::Ok;
    }
    if (audio.sampleCount > std::numeric_limits<uint64_t>::max()
        - chunk.sampleCount) {
        return au::audacityplugin::Status::InvalidArgument;
    }
    appendSamples(*audio.track, chunk);
    audio.sampleCount += chunk.sampleCount;
    return au::audacityplugin::Status::Ok;
}

au::audacityplugin::Status AudacityPluginOfflineHost::replaceAudioTrack(
    au::audacityplugin::AudioHandle handle,
    au::audacityplugin::TrackHandle track)
{
    const auto iterator = m_audio.find(handle);
    if (iterator == m_audio.end()) {
        return au::audacityplugin::Status::InvalidState;
    }
    const auto trackIterator = m_trackIndices.find(track);
    if (trackIterator == m_trackIndices.end()) {
        return au::audacityplugin::Status::InvalidArgument;
    }
    const auto trackIndex = trackIterator->second;
    const auto& selected = m_selectedAudio[static_cast<size_t>(trackIndex)];
    if (iterator->second.track->NChannels() != selected.channelCount) {
        return au::audacityplugin::Status::InvalidArgument;
    }
    if (std::any_of(m_replacements.begin(), m_replacements.end(),
                    [trackIndex](const auto& replacement) {
        return replacement.trackIndex == trackIndex;
    })) {
        return au::audacityplugin::Status::InvalidState;
    }

    auto staged = std::move(iterator->second.track);
    m_audio.erase(iterator);
    staged->Flush();
    m_replacements.push_back({ trackIndex, std::move(staged) });
    return au::audacityplugin::Status::Ok;
}

au::audacityplugin::Status AudacityPluginOfflineHost::addAudioTrack(
    au::audacityplugin::AudioHandle handle,
    const std::string& name)
{
    const auto iterator = m_audio.find(handle);
    if (iterator == m_audio.end()) {
        return au::audacityplugin::Status::InvalidState;
    }
    auto staged = std::move(iterator->second.track);
    m_audio.erase(iterator);
    staged->Flush();
    m_audioTracks.push_back({ name, std::move(staged) });
    return au::audacityplugin::Status::Ok;
}

au::audacityplugin::Status AudacityPluginOfflineHost::releaseAudio(
    au::audacityplugin::AudioHandle handle)
{
    const auto iterator = m_audio.find(handle);
    if (iterator == m_audio.end()) {
        return au::audacityplugin::Status::InvalidState;
    }
    m_audio.erase(iterator);
    return au::audacityplugin::Status::Ok;
}

au::audacityplugin::Status AudacityPluginOfflineHost::createLabelTrack(
    const std::string& name,
    au::audacityplugin::TrackHandle& track)
{
    track = ++m_nextTrack;
    m_labelTrackHandles.insert(track);
    m_labelTracks.push_back({ name, track, {} });
    return au::audacityplugin::Status::Ok;
}

au::audacityplugin::Status AudacityPluginOfflineHost::addLabel(
    au::audacityplugin::TrackHandle track, double start, double end,
    const std::string& text, au::audacityplugin::LabelHandle& label)
{
    if (m_labelTrackHandles.find(track) == m_labelTrackHandles.end()) {
        return au::audacityplugin::Status::InvalidState;
    }
    label = ++m_nextLabel;
    const auto iterator = std::find_if(
        m_labelTracks.begin(), m_labelTracks.end(), [track](const auto& item) {
        return item.track == track;
    });
    if (iterator != m_labelTracks.end()) {
        iterator->labels.push_back({ label, start, end, text });
    } else {
        m_labelChanges.push_back({
                AudacityPluginLabelChangeType::Add,
                track, label, start, end, text,
            });
    }
    return au::audacityplugin::Status::Ok;
}

au::audacityplugin::Status AudacityPluginOfflineHost::updateLabel(
    au::audacityplugin::LabelHandle label, double start, double end,
    const std::string& text)
{
    const auto existing = m_existingLabels.find(label);
    if (existing != m_existingLabels.end()) {
        const auto change = std::find_if(
            m_labelChanges.begin(), m_labelChanges.end(), [label](const auto& item) {
            return item.label == label;
        });
        if (change != m_labelChanges.end()) {
            if (change->type == AudacityPluginLabelChangeType::Delete) {
                return au::audacityplugin::Status::InvalidState;
            }
            change->startSeconds = start;
            change->endSeconds = end;
            change->text = text;
            return au::audacityplugin::Status::Ok;
        }
        m_labelChanges.push_back({
                AudacityPluginLabelChangeType::Update,
                existing->second, label, start, end, text,
            });
        return au::audacityplugin::Status::Ok;
    }

    for (auto& change : m_labelChanges) {
        if (change.type == AudacityPluginLabelChangeType::Add
            && change.label == label) {
            change.startSeconds = start;
            change.endSeconds = end;
            change.text = text;
            return au::audacityplugin::Status::Ok;
        }
    }
    for (auto& track : m_labelTracks) {
        const auto iterator = std::find_if(
            track.labels.begin(), track.labels.end(), [label](const auto& item) {
            return item.label == label;
        });
        if (iterator != track.labels.end()) {
            iterator->startSeconds = start;
            iterator->endSeconds = end;
            iterator->text = text;
            return au::audacityplugin::Status::Ok;
        }
    }
    return au::audacityplugin::Status::InvalidState;
}

au::audacityplugin::Status AudacityPluginOfflineHost::deleteLabel(
    au::audacityplugin::LabelHandle label)
{
    const auto existing = m_existingLabels.find(label);
    if (existing != m_existingLabels.end()) {
        const auto change = std::find_if(
            m_labelChanges.begin(), m_labelChanges.end(), [label](const auto& item) {
            return item.label == label;
        });
        if (change != m_labelChanges.end()) {
            if (change->type == AudacityPluginLabelChangeType::Delete) {
                return au::audacityplugin::Status::InvalidState;
            }
            change->type = AudacityPluginLabelChangeType::Delete;
            change->startSeconds = 0.0;
            change->endSeconds = 0.0;
            change->text.clear();
            return au::audacityplugin::Status::Ok;
        }
        m_labelChanges.push_back({
                AudacityPluginLabelChangeType::Delete,
                existing->second, label, 0.0, 0.0, {},
            });
        return au::audacityplugin::Status::Ok;
    }

    const auto change = std::find_if(
        m_labelChanges.begin(), m_labelChanges.end(), [label](const auto& item) {
        return item.type == AudacityPluginLabelChangeType::Add
               && item.label == label;
    });
    if (change != m_labelChanges.end()) {
        m_labelChanges.erase(change);
        return au::audacityplugin::Status::Ok;
    }
    for (auto& track : m_labelTracks) {
        const auto iterator = std::find_if(
            track.labels.begin(), track.labels.end(), [label](const auto& item) {
            return item.label == label;
        });
        if (iterator != track.labels.end()) {
            track.labels.erase(iterator);
            return au::audacityplugin::Status::Ok;
        }
    }
    return au::audacityplugin::Status::InvalidState;
}

bool AudacityPluginOfflineHost::progress(double fraction, const std::string& message)
{
    return m_progress(fraction, message);
}

bool AudacityPluginOfflineHost::cancelled()
{
    return m_cancelled();
}

au::audacityplugin::Status AudacityPluginOfflineHost::finish() noexcept
{
    const bool incomplete = std::any_of(
        m_audio.begin(), m_audio.end(), [](const auto& item) {
        return item.second.writable;
    });
    m_audio.clear();
    return incomplete
           ? au::audacityplugin::Status::ValidationFailed
           : au::audacityplugin::Status::Ok;
}

const std::vector<AudacityPluginStagedReplacement>&
AudacityPluginOfflineHost::replacements() const noexcept
{
    return m_replacements;
}

const std::vector<AudacityPluginStagedAudioTrack>&
AudacityPluginOfflineHost::audioTracks() const noexcept
{
    return m_audioTracks;
}

const std::vector<AudacityPluginLabelChange>&
AudacityPluginOfflineHost::labelChanges() const noexcept
{
    return m_labelChanges;
}

const std::vector<au::audacityplugin::LabelTrack>&
AudacityPluginOfflineHost::labelTracks() const noexcept
{
    return m_labelTracks;
}

void AudacityPluginOfflineHost::throwIfCancelled() const
{
    if (m_cancelled()) {
        throw ProcessingCancelled {};
    }
}

AudacityPluginOfflineHost::Audio AudacityPluginOfflineHost::convertTrack(
    const Audio& audio, const au::audacityplugin::AudioFormat& format)
{
    throwIfCancelled();
    const auto sourceChannels = static_cast<uint32_t>(audio.track->NChannels());
    const auto sourceRate = static_cast<uint32_t>(audio.track->GetRate());
    uint64_t convertedSamples = 0;
    if (!convertedSampleCount(
            audio.sampleCount, sourceRate, format.sampleRate, convertedSamples)
        || convertedSamples > static_cast<uint64_t>(std::numeric_limits<sampleCount::type>::max())) {
        throw ConversionValidationFailed {};
    }
    if (sourceChannels == format.channelCount && sourceRate == format.sampleRate) {
        return { audio.track, convertedSamples, false };
    }
    auto output = std::static_pointer_cast<WaveTrack>(audio.track->Duplicate());

    if (sourceChannels == 2 && format.channelCount == 1) {
        const auto progress = [this](double) { throwIfCancelled(); };
        const auto cancelled = [this] { return m_cancelled(); };
        if (!output->MixDownToMono(progress, cancelled)) {
            throw ProcessingCancelled {};
        }
    }

    if (sourceRate != format.sampleRate) {
        output->Resample(static_cast<int>(format.sampleRate),
                         [this](size_t) { throwIfCancelled(); });
    }

    if (sourceChannels == 1 && format.channelCount == 2) {
        output = output->MonoToStereo();
    }
    throwIfCancelled();
    return { std::move(output), convertedSamples, false };
}
} // namespace au::effects
