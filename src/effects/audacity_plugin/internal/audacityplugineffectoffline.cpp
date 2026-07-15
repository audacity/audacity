/*
 * Audacity: A Digital Audio Editor
 */
#include "audacityplugineffect.h"

#include <algorithm>
#include <atomic>
#include <chrono>
#include <cmath>
#include <functional>
#include <future>
#include <limits>
#include <memory>
#include <mutex>
#include <new>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "au3-components/EffectAutomationParameters.h"
#include "au3-effects/EffectOutputTracks.h"
#include "au3-exceptions/UserException.h"
#include "au3-label-track/LabelTrack.h"
#include "au3-math/SampleFormat.h"
#include "au3-strings/Internat.h"
#include "au3-time-frequency-selection/SelectedRegion.h"
#include "au3-track/TimeWarper.h"
#include "au3-track/Track.h"
#include "au3-wave-track/WaveClip.h"
#include "au3-wave-track/WaveTrack.h"

#include "au3wrap/internal/wxtypes_convert.h"
#include "framework/global/log.h"
#include "framework/global/modularity/ioc.h"

#include "audacityplugin/iaudacitypluginhost.h"
#include "audacitypluginofflinehost.h"

namespace au::effects {
namespace {
std::optional<uint32_t> projectSampleRate(double value)
{
    if (!std::isfinite(value) || value < 1.0
        || std::trunc(value) != value
        || value > std::numeric_limits<int32_t>::max()) {
        return std::nullopt;
    }
    return static_cast<uint32_t>(value);
}

struct SelectedAudioTrack {
    std::shared_ptr<WaveTrack> selectionTrack;
    std::shared_ptr<WaveTrack> destinationTrack;
    std::string name;
    uint32_t rate = 0;
    uint64_t sampleCount = 0;
    au::audacityplugin::TrackHandle track = 0;
};

struct SelectedLabel {
    au::audacityplugin::LabelHandle label = 0;
    int64_t nativeId = 0;
};

struct SelectedLabelTrack {
    std::shared_ptr<LabelTrack> destinationTrack;
    au::audacityplugin::LabelTrack track;
    std::vector<SelectedLabel> labels;
};

std::vector<SelectedAudioTrack> collectSelectedAudio(
    const AudacityPluginEffect& effect, EffectOutputTracks& renderedInputs)
{
    std::vector<SelectedAudioTrack> result;
    const double selectionStart = std::min(effect.mT0, effect.mT1);
    const double selectionEnd = std::max(effect.mT0, effect.mT1);

    const auto& descriptor = effect.descriptor();
    const bool acceptsAudio
        = (descriptor.inputTrackTypes
           & au::audacityplugin::InputTrackAudio) != 0;
    if (!acceptsAudio) {
        return result;
    }

    const auto append = [&](WaveTrack& track) {
        if (track.NChannels() != 1 && track.NChannels() != 2) {
            return;
        }

        const sampleCount first = track.TimeToLongSamples(selectionStart);
        const sampleCount last = track.TimeToLongSamples(selectionEnd);
        const sampleCount count = last - first;

        SelectedAudioTrack input;
        input.selectionTrack = track.SharedPointer<WaveTrack>();
        const auto& destination = static_cast<const WaveTrack&>(
            *renderedInputs.GetMatchingInput(track));
        input.destinationTrack = std::const_pointer_cast<WaveTrack>(
            destination.SharedPointer<const WaveTrack>());
        input.name = au3::wxToStdString(track.GetName());
        input.rate = static_cast<uint32_t>(track.GetRate());
        input.sampleCount = static_cast<uint64_t>(count.as_long_long());
        result.push_back(std::move(input));
    };

    for (auto* track : renderedInputs.Get().Selected<WaveTrack>()) {
        append(*track);
    }
    return result;
}

bool trackNeedsChannelNormalization(const WaveTrack& track)
{
    const auto channelCount = track.NChannels();
    const auto intervals = track.Intervals();
    return std::any_of(intervals.begin(), intervals.end(), [channelCount](const auto& clip) {
        return clip->NChannels() != channelCount;
    });
}

void prepareSelectedAudio(SelectedAudioTrack& input,
                          double selectionStart, double selectionEnd,
                          const ProgressReporter& progress,
                          const std::function<bool()>& cancelled)
{
    const auto throwIfCancelled = [&] {
        if (cancelled()) {
            throw UserException {};
        }
    };
    throwIfCancelled();

    input.selectionTrack = std::static_pointer_cast<WaveTrack>(
        input.selectionTrack->Copy(selectionStart, selectionEnd, false));
    if (trackNeedsChannelNormalization(*input.selectionTrack)) {
        const auto didNormalize = input.selectionTrack->FixClipChannels(
            [&](double fraction) {
            throwIfCancelled();
            progress(std::clamp(fraction, 0.0, 1.0));
        },
            [&] { return cancelled(); });
        if (!didNormalize) {
            throw UserException {};
        }
    }
    progress(1.0);
    throwIfCancelled();
}

std::string effectErrorMessage(au::audacityplugin::Status status)
{
    switch (status) {
    case au::audacityplugin::Status::NotReady: return "The plugin is not ready";
    case au::audacityplugin::Status::ValidationFailed: return "The plugin produced invalid output";
    case au::audacityplugin::Status::OutOfMemory: return "The plugin ran out of memory";
    default: return "The plugin could not complete the operation";
    }
}

SelectedLabelTrack labelTrackInput(
    LabelTrack& track, double selectionStart, double selectionEnd,
    au::audacityplugin::TrackHandle trackHandle,
    au::audacityplugin::LabelHandle& nextLabel)
{
    SelectedLabelTrack result;
    result.destinationTrack = track.SharedPointer<LabelTrack>();
    result.track.name = au3::wxToStdString(track.GetName());
    result.track.track = trackHandle;
    result.track.labels.reserve(static_cast<size_t>(track.GetNumLabels()));
    result.labels.reserve(static_cast<size_t>(track.GetNumLabels()));
    for (int index = 0; index < track.GetNumLabels(); ++index) {
        const auto* label = track.GetLabel(index);
        if (!label || label->getT1() < selectionStart
            || label->getT0() > selectionEnd) {
            continue;
        }
        const auto handle = ++nextLabel;
        result.track.labels.push_back({
                    handle, label->getT0() - selectionStart,
                    label->getT1() - selectionStart,
                    au3::wxToStdString(label->title),
                });
        result.labels.push_back({ handle, label->GetId() });
    }
    return result;
}
} // namespace

std::shared_ptr<::EffectInstance> AudacityPluginEffect::MakeInstance() const
{
    const double selectionDuration = std::abs(mT1 - mT0);

    const auto rate = projectSampleRate(mProjectRate);
    if (!rate) {
        return {};
    }

    static muse::GlobalInject<au::audacityplugin::IAudacityPluginHost> audacityPluginHost;
    if (!audacityPluginHost()) {
        return {};
    }
    auto result = audacityPluginHost()->createInstance(m_descriptor.pluginId, m_descriptor.effectId, {
            selectionDuration,
            *rate,
        });
    if (result.status != au::audacityplugin::Status::Ok || !result.instance) {
        LOGE() << "Could not create Plugin API v0 effect instance";
        return {};
    }
    return std::make_shared<AudacityPluginEffectInstance>(
        const_cast<AudacityPluginEffect&>(*this), std::move(result.instance));
}

bool AudacityPluginEffect::Process(::EffectInstance& effectInstance, ::EffectSettings& settings)
{
    auto& instance = dynamic_cast<AudacityPluginEffectInstance&>(effectInstance);

    const auto fail = [&](std::string error) {
        instance.setLastError(std::move(error));
        return false;
    };
    const auto cancel = [&] {
        return fail("The plugin operation was cancelled");
    };

    try {
        if (!instance.applySettings(settings)) {
            return false;
        }
        const bool isGenerator = GetType() == EffectTypeGenerate;
        const double generatorDuration
            = isGenerator ? instance.generatorDuration() : 0.0;
        auto& pluginInstance = instance.pluginInstance();
        if ((isGenerator && generatorDuration <= 0.0)
            || pluginInstance.validate() != au::audacityplugin::Status::Ok) {
            return fail("The plugin is not ready");
        }
        instance.writeCurrentSettings(settings);

        auto& tracks = *mTracks;
        auto& factory = *mFactory;
        WaveTrack* provisionalGeneratorTrack = nullptr;
        if (isGenerator && mNumTracks == 0) {
            const auto selected = tracks.Selected<WaveTrack>();
            if (selected.begin() != selected.end()) {
                provisionalGeneratorTrack = *selected.begin();
            }
        }

        const double selectionStart = std::min(mT0, mT1);
        const double selectionEnd = std::max(mT0, mT1);
        const double selectionDuration = selectionEnd - selectionStart;
        const bool acceptsAudio
            = (m_descriptor.inputTrackTypes
               & au::audacityplugin::InputTrackAudio) != 0;
        const bool acceptsLabels
            = (m_descriptor.inputTrackTypes
               & au::audacityplugin::InputTrackLabel) != 0;

        au::audacityplugin::TrackHandle nextTrack = 0;
        au::audacityplugin::LabelHandle nextLabel = 0;
        std::vector<SelectedLabelTrack> selectedLabels;
        if (acceptsLabels) {
            for (auto* track : tracks.Selected<LabelTrack>()) {
                selectedLabels.push_back(labelTrackInput(
                                             *track, selectionStart, selectionEnd,
                                             ++nextTrack, nextLabel));
            }
        }

        std::vector<au::audacityplugin::LabelTrack> labelTracks;
        labelTracks.reserve(selectedLabels.size());
        for (const auto& selected : selectedLabels) {
            labelTracks.push_back(selected.track);
        }

        std::vector<SelectedAudioTrack> selectedAudio;
        {
            EffectOutputTracks renderedInputs {
                tracks, EffectTypeProcess, { { mT0, mT1 } }
            };
            selectedAudio = collectSelectedAudio(*this, renderedInputs);

            const bool hasReadableAudio = std::any_of(
                selectedAudio.begin(), selectedAudio.end(), [](const auto& input) {
                return input.sampleCount != 0;
            });
            if (requiresInputSelection() && !hasReadableAudio
                && labelTracks.empty()) {
                return fail(acceptsAudio && !acceptsLabels
                            ? "No selected audio is available"
                            : "No selected audio or label track is available");
            }

            bool preparationCancelled = false;
            for (size_t index = 0; index < selectedAudio.size(); ++index) {
                if (selectedAudio[index].sampleCount == 0) {
                    continue;
                }
                const auto reporter = [&](double fraction) {
                    const double total = static_cast<double>(
                        std::max<size_t>(1, selectedAudio.size()));
                    if (TotalProgress(0.1 * (static_cast<double>(index)
                                             + std::clamp(fraction, 0.0, 1.0)) / total)) {
                        preparationCancelled = true;
                    }
                };
                prepareSelectedAudio(
                    selectedAudio[index], selectionStart, selectionEnd, reporter, [&] {
                    return preparationCancelled;
                });
            }
        }

        for (auto& selected : selectedAudio) {
            selected.track = ++nextTrack;
        }

        std::vector<AudacityPluginSelectedAudio> hostAudio;
        hostAudio.reserve(selectedAudio.size());
        for (auto& input : selectedAudio) {
            hostAudio.push_back({
                    std::move(input.selectionTrack),
                    static_cast<uint32_t>(input.destinationTrack->NChannels()),
                    input.sampleCount,
                    input.track,
                    0,
                });
        }

        std::atomic_bool cancelled { TotalProgress(0.1) };
        std::mutex progressMutex;
        double pluginFraction = 0.0;
        std::string pluginMessage;

        AudacityPluginOfflineHost offlineHost(
            factory,
            std::move(hostAudio),
            labelTracks,
            [&](double fraction, const std::string& message) {
            if (cancelled.load(std::memory_order_relaxed)) {
                return false;
            }
            const std::lock_guard lock(progressMutex);
            pluginFraction = fraction;
            pluginMessage = message;
            return true;
        },
            [&] { return cancelled.load(std::memory_order_relaxed); });

        std::vector<au::audacityplugin::AudioTrack> audioTracks;
        audioTracks.reserve(selectedAudio.size());
        for (size_t index = 0; index < selectedAudio.size(); ++index) {
            const auto& selected = selectedAudio[index];
            au::audacityplugin::AudioTrack track;
            track.name = selected.name;
            track.track = selected.track;
            track.audio = offlineHost.selectedAudio()[index].audio;
            track.info = {
                { static_cast<uint32_t>(selected.destinationTrack->NChannels()), selected.rate },
                selected.sampleCount,
            };
            audioTracks.push_back(std::move(track));
        }

        au::audacityplugin::OfflineArgs args;
        args.selectionDurationSeconds = selectionDuration;
        args.generatorDurationSeconds = generatorDuration;
        args.audioTracks = std::move(audioTracks);
        args.labelTracks = std::move(labelTracks);

        auto future = std::async(std::launch::async,
                                 [&]() -> au::audacityplugin::Status {
            return pluginInstance.apply(args, offlineHost);
        });
        const auto pollProgress = [&] {
            double fraction = 0.0;
            std::string message;
            {
                const std::lock_guard lock(progressMutex);
                fraction = pluginFraction;
                message = pluginMessage;
            }
            if (TotalProgress(0.1 + 0.9 * fraction, Verbatim(message))) {
                cancelled.store(true, std::memory_order_relaxed);
            }
        };
        while (future.wait_for(std::chrono::milliseconds(10))
               != std::future_status::ready) {
            pollProgress();
        }
        pollProgress();

        const auto result = future.get();
        if (cancelled.load(std::memory_order_relaxed)
            || result == au::audacityplugin::Status::Cancelled) {
            return cancel();
        }
        if (result != au::audacityplugin::Status::Ok) {
            LOGE() << "Audacity plugin offline effect failed with status "
                   << static_cast<int32_t>(result);
            return fail(effectErrorMessage(result));
        }

        auto replacementTracks = TrackList::Temporary(tracks.GetOwner());
        std::vector<std::shared_ptr<WaveTrack> > replacementDestinations;
        replacementDestinations.reserve(offlineHost.replacements().size());
        for (const auto& replacement : offlineHost.replacements()) {
            const auto& input = selectedAudio[static_cast<size_t>(replacement.trackIndex)];
            auto duplicate = std::static_pointer_cast<WaveTrack>(
                input.destinationTrack->Duplicate(Track::DuplicateOptions {}.Backup()));
            replacementTracks->Add(duplicate);
            PasteTimeWarper warper {
                selectionEnd,
                selectionStart + replacement.track->GetEndTime()
            };
            const bool preserve = !isGenerator;
            const bool merge = !isGenerator;
            duplicate->ClearAndPaste(
                selectionStart, selectionEnd, *replacement.track,
                preserve, merge, &warper);
            replacementDestinations.push_back(input.destinationTrack);
        }

        const bool provisionalWasReplaced = provisionalGeneratorTrack
                                            && std::any_of(replacementDestinations.begin(),
                                                           replacementDestinations.end(), [&](const auto& destination) {
            return destination.get() == provisionalGeneratorTrack;
        });

        auto addedTracks = TrackList::Temporary(tracks.GetOwner());
        size_t firstAddedTrack = 0;
        const auto& stagedAudioTracks = offlineHost.audioTracks();
        if (provisionalGeneratorTrack && !provisionalWasReplaced
            && !stagedAudioTracks.empty()) {
            const auto& generated = stagedAudioTracks.front();
            generated.track->SetName(au3::wxFromStdString(generated.name));
            generated.track->MoveTo(selectionStart);
            replacementTracks->Add(generated.track);
            replacementDestinations.push_back(
                provisionalGeneratorTrack->SharedPointer<WaveTrack>());
            firstAddedTrack = 1;
        }
        for (size_t index = firstAddedTrack; index < stagedAudioTracks.size(); ++index) {
            const auto& added = stagedAudioTracks[index];
            added.track->SetName(au3::wxFromStdString(added.name));
            added.track->MoveTo(selectionStart);
            addedTracks->Add(added.track);
        }

        auto labelReplacementTracks = TrackList::Temporary(tracks.GetOwner());
        std::vector<std::shared_ptr<LabelTrack> > labelReplacementDestinations;
        for (const auto& selected : selectedLabels) {
            const auto hasChanges = std::any_of(
                offlineHost.labelChanges().begin(),
                offlineHost.labelChanges().end(), [&](const auto& change) {
                return change.track == selected.track.track;
            });
            if (!hasChanges) {
                continue;
            }

            auto duplicate = std::static_pointer_cast<LabelTrack>(
                selected.destinationTrack->Duplicate(
                    Track::DuplicateOptions {}.Backup()));
            std::vector<SelectedLabel> duplicateLabels;
            duplicateLabels.reserve(selected.labels.size());
            for (const auto& label : selected.labels) {
                const auto index
                    = selected.destinationTrack->GetLabelIndex(label.nativeId);
                const auto* duplicateLabel
                    = index < 0 ? nullptr : duplicate->GetLabel(index);
                if (!duplicateLabel) {
                    return fail("The plugin produced an invalid label change");
                }
                duplicateLabels.push_back({ label.label, duplicateLabel->GetId() });
            }
            for (const auto& change : offlineHost.labelChanges()) {
                if (change.track != selected.track.track) {
                    continue;
                }
                if (change.type == AudacityPluginLabelChangeType::Add) {
                    duplicate->AddLabel(
                        SelectedRegion(selectionStart + change.startSeconds,
                                       selectionStart + change.endSeconds),
                        au3::wxFromStdString(change.text));
                    continue;
                }

                const auto label = std::find_if(
                    duplicateLabels.begin(), duplicateLabels.end(),
                    [&](const auto& item) { return item.label == change.label; });
                if (label == duplicateLabels.end()) {
                    return fail("The plugin produced an invalid label change");
                }
                if (change.type == AudacityPluginLabelChangeType::Delete) {
                    duplicate->DeleteLabelById(label->nativeId);
                    continue;
                }

                const auto index = duplicate->GetLabelIndex(label->nativeId);
                if (index < 0) {
                    return fail("The plugin produced an invalid label change");
                }
                auto updated = *duplicate->GetLabel(index);
                updated.selectedRegion.setTimes(
                    selectionStart + change.startSeconds,
                    selectionStart + change.endSeconds);
                updated.title = au3::wxFromStdString(change.text);
                duplicate->SetLabel(static_cast<size_t>(index), updated);
            }
            labelReplacementTracks->Add(duplicate);
            labelReplacementDestinations.push_back(selected.destinationTrack);
        }

        for (const auto& labels : offlineHost.labelTracks()) {
            auto labelTrack = LabelTrack::CreatePtr(tracks);
            labelTrack->SetName(au3::wxFromStdString(labels.name));
            for (const auto& label : labels.labels) {
                labelTrack->AddLabel(
                    SelectedRegion(selectionStart + label.startSeconds,
                                   selectionStart + label.endSeconds),
                    au3::wxFromStdString(label.text));
            }
            addedTracks->Add(labelTrack);
        }

        if (cancelled.load(std::memory_order_relaxed) || TotalProgress(1.0)) {
            return cancel();
        }

        for (const auto& destination : replacementDestinations) {
            tracks.ReplaceOne(*destination, std::move(*replacementTracks));
        }
        for (const auto& destination : labelReplacementDestinations) {
            tracks.ReplaceOne(*destination, std::move(*labelReplacementTracks));
        }
        tracks.Append(std::move(*addedTracks));

        if (isGenerator) {
            mT1 = mT0 + generatorDuration;
        }
        instance.setLastError({});
        return true;
    } catch (const UserException&) {
        return cancel();
    } catch (const AudacityException&) {
        throw;
    } catch (const std::bad_alloc&) {
        return fail("The plugin ran out of memory");
    } catch (const std::exception& error) {
        return fail(error.what());
    } catch (...) {
        return fail("The plugin could not complete the operation");
    }
}
} // namespace au::effects
