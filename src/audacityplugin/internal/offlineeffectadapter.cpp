/*
 * Audacity: A Digital Audio Editor
 */
#include "offlineeffectadapter.h"

#include <algorithm>
#include <array>
#include <cmath>
#include <limits>
#include <new>
#include <vector>

namespace au::audacityplugin::internal {
namespace {
bool validFormat(const AudioFormat& format) noexcept
{
    return (format.channelCount == 1 || format.channelCount == 2)
           && format.sampleRate != 0
           && format.sampleRate
           <= static_cast<uint32_t>(std::numeric_limits<int>::max());
}

bool validChunk(const AudioChunk& chunk, bool allowEmpty) noexcept
{
    if (chunk.sampleCount == 0) {
        return allowEmpty && chunk.channelCount == 0;
    }
    if (chunk.channelCount != 1 && chunk.channelCount != 2) {
        return false;
    }
    return std::all_of(chunk.channels.begin(),
                       chunk.channels.begin() + chunk.channelCount,
                       [](const float* buffer) { return buffer != nullptr; });
}

class OfflineEffectAdapter
{
public:
    OfflineEffectAdapter(const OfflineArgs& args, IOfflineHost& host)
        : m_host(host)
    {
        m_audioTracks.reserve(args.audioTracks.size());
        for (const auto& track : args.audioTracks) {
            m_audioTracks.push_back({
                        stringView(track.name),
                        static_cast<aup_track>(track.track),
                        static_cast<aup_audio>(track.audio),
                        { { track.info.format.channelCount, track.info.format.sampleRate },
                            track.info.sampleCount }
                    });
        }

        m_labels.reserve(args.labelTracks.size());
        m_labelTracks.reserve(args.labelTracks.size());
        for (const auto& track : args.labelTracks) {
            auto& labels = m_labels.emplace_back();
            labels.reserve(track.labels.size());
            for (const auto& label : track.labels) {
                labels.push_back({ static_cast<aup_label>(label.label),
                                   label.startSeconds, label.endSeconds,
                                   stringView(label.text) });
            }
            m_labelTracks.push_back({ stringView(track.name),
                                      static_cast<aup_track>(track.track), labels.size(),
                                      labels.empty() ? nullptr : labels.data() });
        }

        m_abiArgs.selection_duration_seconds
            = args.selectionDurationSeconds;
        m_abiArgs.generator_duration_seconds
            = args.generatorDurationSeconds;
        m_abiArgs.audio_track_count = m_audioTracks.size();
        m_abiArgs.audio_tracks
            = m_audioTracks.empty() ? nullptr : m_audioTracks.data();
        m_abiArgs.label_track_count = m_labelTracks.size();
        m_abiArgs.label_tracks
            = m_labelTracks.empty() ? nullptr : m_labelTracks.data();

        m_abiHost.context = this;
        m_abiHost.query = &queryCallback;
        m_abiHost.convert_audio = &convertAudioCallback;
        m_abiHost.read_audio = &readAudioCallback;
        m_abiHost.create_audio = &createAudioCallback;
        m_abiHost.write_audio = &writeAudioCallback;
        m_abiHost.replace_audio_track = &replaceAudioTrackCallback;
        m_abiHost.add_audio_track = &addAudioTrackCallback;
        m_abiHost.release_audio = &releaseAudioCallback;
        m_abiHost.create_label_track = &createLabelTrackCallback;
        m_abiHost.add_label = &addLabelCallback;
        m_abiHost.update_label = &updateLabelCallback;
        m_abiHost.delete_label = &deleteLabelCallback;
        m_abiHost.progress = &progressCallback;
        m_abiHost.cancelled = &cancelledCallback;
    }

    const aup_effect_offline_args_v0* args() const noexcept
    {
        return &m_abiArgs;
    }

    const aup_effect_offline_host_v0* host() const noexcept { return &m_abiHost; }

    Status finish(Status pluginStatus) noexcept
    {
        const bool cancelled = observeCancellation();
        const auto finishStatus = check(m_host.finish());
        if (cancelled || pluginStatus == Status::Cancelled) {
            return Status::Cancelled;
        }
        if (m_failure != Status::Ok) {
            return m_failure;
        }
        if (pluginStatus != Status::Ok) {
            return pluginStatus;
        }
        return finishStatus;
    }

private:
    Status check(Status status) noexcept
    {
        fail(status);
        return status;
    }

    Status ready() noexcept
    {
        if (observeCancellation()) {
            return Status::Cancelled;
        }
        return m_failure == Status::Ok ? Status::Ok : Status::InvalidState;
    }

    void fail(Status status) noexcept
    {
        if (status == Status::Cancelled) {
            m_cancelled = true;
        } else if (status != Status::Ok && m_failure == Status::Ok) {
            m_failure = status;
        }
    }

    bool observeCancellation() noexcept
    {
        if (m_cancelled) {
            return true;
        }
        try {
            m_cancelled = m_host.cancelled();
        } catch (...) {
            fail(Status::HostError);
        }
        return m_cancelled;
    }

    Status convertAudio(aup_audio audio, const aup_audio_format* format,
                        aup_audio* converted, aup_audio_info* info)
    {
        const auto state = ready();
        if (state != Status::Ok) {
            return state;
        }
        if (!format || !converted || !info || audio == AUP_INVALID_AUDIO) {
            return check(Status::InvalidArgument);
        }
        const AudioFormat target { format->channel_count, format->sample_rate };
        if (!validFormat(target)) {
            return check(Status::InvalidArgument);
        }

        AudioHandle convertedAudio = AUP_INVALID_AUDIO;
        AudioInfo convertedInfo;
        const auto status = check(m_host.convertAudio(
                                      audio, target, convertedAudio, convertedInfo));
        if (status != Status::Ok) {
            return status;
        }
        if (convertedAudio == AUP_INVALID_AUDIO
            || convertedInfo.sampleCount == 0
            || convertedInfo.format.channelCount != target.channelCount
            || convertedInfo.format.sampleRate != target.sampleRate) {
            return check(Status::HostError);
        }
        *converted = convertedAudio;
        *info = { { convertedInfo.format.channelCount, convertedInfo.format.sampleRate },
            convertedInfo.sampleCount };
        return Status::Ok;
    }

    Status readAudio(aup_audio audio, uint64_t sampleOffset,
                     uint64_t requestedSamples,
                     aup_audio_chunk* output)
    {
        const auto state = ready();
        if (state != Status::Ok) {
            return state;
        }
        if (!output || audio == AUP_INVALID_AUDIO || requestedSamples == 0) {
            return check(Status::InvalidArgument);
        }

        AudioChunk chunk;
        const auto status = check(m_host.readAudio(
                                      audio, sampleOffset, requestedSamples, chunk));
        if (status != Status::Ok) {
            return status;
        }
        if (chunk.sampleCount > requestedSamples || !validChunk(chunk, true)) {
            return check(Status::HostError);
        }
        if (chunk.sampleCount == 0) {
            return Status::Ok;
        }

        std::copy_n(chunk.channels.begin(), chunk.channelCount, m_readBuffers.begin());
        output->channels = m_readBuffers.data();
        output->channel_count = chunk.channelCount;
        output->sample_count = chunk.sampleCount;
        return Status::Ok;
    }

    Status createAudio(const aup_audio_format* format, aup_audio* audio)
    {
        const auto state = ready();
        if (state != Status::Ok) {
            return state;
        }
        if (!format || !audio) {
            return check(Status::InvalidArgument);
        }
        const AudioFormat target { format->channel_count, format->sample_rate };
        if (!validFormat(target)) {
            return check(Status::InvalidArgument);
        }

        AudioHandle created = AUP_INVALID_AUDIO;
        const auto status = check(m_host.createAudio(target, created));
        if (status != Status::Ok) {
            return status;
        }
        if (created == AUP_INVALID_AUDIO) {
            return check(Status::HostError);
        }
        *audio = created;
        return Status::Ok;
    }

    Status writeAudio(aup_audio audio, const aup_audio_chunk* chunk)
    {
        const auto state = ready();
        if (state != Status::Ok) {
            return state;
        }
        if (audio == AUP_INVALID_AUDIO || !chunk
            || (chunk->channel_count != 1 && chunk->channel_count != 2)
            || (chunk->sample_count != 0 && !chunk->channels)) {
            return check(Status::InvalidArgument);
        }
        AudioChunk hostChunk;
        hostChunk.sampleCount = chunk->sample_count;
        hostChunk.channelCount = chunk->channel_count;
        if (chunk->sample_count != 0) {
            std::copy_n(chunk->channels, chunk->channel_count,
                        hostChunk.channels.begin());
            if (!validChunk(hostChunk, false)) {
                return check(Status::InvalidArgument);
            }
        }
        return check(m_host.writeAudio(audio, hostChunk));
    }

    Status replaceAudioTrack(aup_audio audio, aup_track track)
    {
        const auto state = ready();
        if (state != Status::Ok) {
            return state;
        }
        if (audio == AUP_INVALID_AUDIO || track == AUP_INVALID_TRACK) {
            return check(Status::InvalidArgument);
        }
        return check(m_host.replaceAudioTrack(audio, track));
    }

    Status addAudioTrack(aup_audio audio, aup_str name)
    {
        const auto state = ready();
        if (state != Status::Ok) {
            return state;
        }
        std::string copiedName;
        if (audio == AUP_INVALID_AUDIO
            || !copyString(name, copiedName) || copiedName.empty()) {
            return check(Status::InvalidArgument);
        }
        return check(m_host.addAudioTrack(audio, copiedName));
    }

    Status releaseAudio(aup_audio audio)
    {
        const auto state = ready();
        if (state != Status::Ok) {
            return state;
        }
        if (audio == AUP_INVALID_AUDIO) {
            return check(Status::InvalidArgument);
        }
        return check(m_host.releaseAudio(audio));
    }

    Status createLabelTrack(aup_str name, aup_track* track)
    {
        const auto state = ready();
        if (state != Status::Ok) {
            return state;
        }
        std::string copiedName;
        if (!track || !copyString(name, copiedName) || copiedName.empty()) {
            return check(Status::InvalidArgument);
        }
        TrackHandle created = AUP_INVALID_TRACK;
        const auto status = check(m_host.createLabelTrack(copiedName, created));
        if (status != Status::Ok) {
            return status;
        }
        if (created == AUP_INVALID_TRACK) {
            return check(Status::HostError);
        }
        *track = created;
        return Status::Ok;
    }

    Status addLabel(aup_track track, double start, double end, aup_str text,
                    aup_label* label)
    {
        const auto state = ready();
        if (state != Status::Ok) {
            return state;
        }
        std::string copiedText;
        if (track == AUP_INVALID_TRACK
            || !std::isfinite(start) || !std::isfinite(end)
            || end < start || !copyString(text, copiedText)) {
            return check(Status::InvalidArgument);
        }
        LabelHandle created = AUP_INVALID_LABEL;
        const auto status = check(
            m_host.addLabel(track, start, end, copiedText, created));
        if (status != Status::Ok) {
            return status;
        }
        if (created == AUP_INVALID_LABEL) {
            return check(Status::HostError);
        }
        if (label) {
            *label = created;
        }
        return Status::Ok;
    }

    Status updateLabel(aup_label label, double start, double end, aup_str text)
    {
        const auto state = ready();
        if (state != Status::Ok) {
            return state;
        }
        std::string copiedText;
        if (label == AUP_INVALID_LABEL
            || !std::isfinite(start) || !std::isfinite(end)
            || end < start || !copyString(text, copiedText)) {
            return check(Status::InvalidArgument);
        }
        return check(m_host.updateLabel(label, start, end, copiedText));
    }

    Status deleteLabel(aup_label label)
    {
        const auto state = ready();
        if (state != Status::Ok) {
            return state;
        }
        if (label == AUP_INVALID_LABEL) {
            return check(Status::InvalidArgument);
        }
        return check(m_host.deleteLabel(label));
    }

    bool progress(double fraction, aup_str message)
    {
        if (observeCancellation() || m_failure != Status::Ok) {
            return false;
        }
        std::string copiedMessage;
        if (!std::isfinite(fraction) || fraction < 0.0 || fraction > 1.0
            || !copyString(message, copiedMessage)) {
            fail(Status::InvalidArgument);
            return false;
        }
        if (!m_host.progress(fraction, copiedMessage)) {
            m_cancelled = true;
            return false;
        }
        return true;
    }

    template<typename Function>
    static aup_status invokeStatus(void* context, Function&& function) noexcept
    {
        if (!context) {
            return AUP_INVALID_ARGUMENT;
        }
        auto& adapter = *static_cast<OfflineEffectAdapter*>(context);
        try {
            return static_cast<aup_status>(function(adapter));
        } catch (const std::bad_alloc&) {
            adapter.fail(Status::OutOfMemory);
            return AUP_OUT_OF_MEMORY;
        } catch (...) {
            adapter.fail(Status::HostError);
            return AUP_HOST_ERROR;
        }
    }

    static const void* AUP_CALL queryCallback(void*, aup_str) noexcept
    {
        return nullptr;
    }

    static aup_status AUP_CALL convertAudioCallback(
        void* context, aup_audio audio, const aup_audio_format* format,
        aup_audio* converted, aup_audio_info* info) noexcept
    {
        if (converted) {
            *converted = AUP_INVALID_AUDIO;
        }
        if (info) {
            *info = {};
        }
        return invokeStatus(context, [&](auto& adapter) {
            return adapter.convertAudio(audio, format, converted, info);
        });
    }

    static aup_status AUP_CALL readAudioCallback(
        void* context, aup_audio audio, uint64_t sampleOffset,
        uint64_t requestedSamples,
        aup_audio_chunk* output) noexcept
    {
        if (output) {
            *output = {};
        }
        return invokeStatus(context, [&](auto& adapter) {
            return adapter.readAudio(
                audio, sampleOffset, requestedSamples, output);
        });
    }

    static aup_status AUP_CALL createAudioCallback(
        void* context, const aup_audio_format* format, aup_audio* audio) noexcept
    {
        if (audio) {
            *audio = AUP_INVALID_AUDIO;
        }
        return invokeStatus(context, [&](auto& adapter) {
            return adapter.createAudio(format, audio);
        });
    }

    static aup_status AUP_CALL writeAudioCallback(
        void* context, aup_audio audio, const aup_audio_chunk* chunk) noexcept
    {
        return invokeStatus(context, [&](auto& adapter) {
            return adapter.writeAudio(audio, chunk);
        });
    }

    static aup_status AUP_CALL replaceAudioTrackCallback(
        void* context, aup_audio audio, aup_track track) noexcept
    {
        return invokeStatus(context, [&](auto& adapter) {
            return adapter.replaceAudioTrack(audio, track);
        });
    }

    static aup_status AUP_CALL addAudioTrackCallback(
        void* context, aup_audio audio, aup_str name) noexcept
    {
        return invokeStatus(context, [&](auto& adapter) {
            return adapter.addAudioTrack(audio, name);
        });
    }

    static aup_status AUP_CALL releaseAudioCallback(
        void* context, aup_audio audio) noexcept
    {
        return invokeStatus(context, [&](auto& adapter) {
            return adapter.releaseAudio(audio);
        });
    }

    static aup_status AUP_CALL createLabelTrackCallback(
        void* context, aup_str name, aup_track* track) noexcept
    {
        if (track) {
            *track = AUP_INVALID_TRACK;
        }
        return invokeStatus(context, [&](auto& adapter) {
            return adapter.createLabelTrack(name, track);
        });
    }

    static aup_status AUP_CALL addLabelCallback(
        void* context, aup_track track, double start, double end,
        aup_str text, aup_label* label) noexcept
    {
        if (label) {
            *label = AUP_INVALID_LABEL;
        }
        return invokeStatus(context, [&](auto& adapter) {
            return adapter.addLabel(track, start, end, text, label);
        });
    }

    static aup_status AUP_CALL updateLabelCallback(
        void* context, aup_label label, double start, double end,
        aup_str text) noexcept
    {
        return invokeStatus(context, [&](auto& adapter) {
            return adapter.updateLabel(label, start, end, text);
        });
    }

    static aup_status AUP_CALL deleteLabelCallback(
        void* context, aup_label label) noexcept
    {
        return invokeStatus(context, [&](auto& adapter) {
            return adapter.deleteLabel(label);
        });
    }

    static bool AUP_CALL progressCallback(void* context, double fraction,
                                          aup_str message) noexcept
    {
        if (!context) {
            return false;
        }
        auto& adapter = *static_cast<OfflineEffectAdapter*>(context);
        try {
            return adapter.progress(fraction, message);
        } catch (const std::bad_alloc&) {
            adapter.fail(Status::OutOfMemory);
        } catch (...) {
            adapter.fail(Status::HostError);
        }
        return false;
    }

    static bool AUP_CALL cancelledCallback(void* context) noexcept
    {
        return !context || static_cast<OfflineEffectAdapter*>(context)->observeCancellation();
    }

    IOfflineHost& m_host;
    aup_effect_offline_args_v0 m_abiArgs {};
    aup_effect_offline_host_v0 m_abiHost {};
    std::vector<aup_audio_track> m_audioTracks;
    std::vector<std::vector<aup_label_info> > m_labels;
    std::vector<aup_label_track> m_labelTracks;
    std::array<const float*, 2> m_readBuffers {};
    Status m_failure = Status::Ok;
    bool m_cancelled = false;
};
} // namespace

Status invokeOfflineEffect(const aup_effect_offline_v0& offline,
                           const OfflineArgs& args,
                           IOfflineHost& host) noexcept
{
    try {
        OfflineEffectAdapter adapter(args, host);
        Status pluginStatus = Status::PluginError;
        try {
            pluginStatus = statusFromAbi(
                offline.process(
                    offline.context, adapter.args(), adapter.host()));
        } catch (const std::bad_alloc&) {
            pluginStatus = Status::OutOfMemory;
        } catch (...) {
            pluginStatus = Status::PluginError;
        }
        return adapter.finish(pluginStatus);
    } catch (const std::bad_alloc&) {
        return Status::OutOfMemory;
    } catch (...) {
        return Status::HostError;
    }
}
} // namespace au::audacityplugin::internal
