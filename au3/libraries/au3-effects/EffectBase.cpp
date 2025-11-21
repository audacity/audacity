/**********************************************************************

  Audacity: A Digital Audio Editor

  EffectBase.cpp

  Dominic Mazzoni
  Vaughan Johnson
  Martyn Shaw

  Paul Licameli split from Effect.cpp

*******************************************************************//**

\class EffectBase
\brief Base class for many of the effects in Audacity.

*//*******************************************************************/
#include "EffectBase.h"

#include <thread>
#include "BasicUI.h"
#include "ConfigInterface.h"
#include "EffectOutputTracks.h"
#include "PluginManager.h"
#include "QualitySettings.h"
#include "TransactionScope.h"
#include "ViewInfo.h"
#include "WaveTrack.h"
#include "NumericConverterFormats.h"

EffectBase::EffectBase()
{
    // PRL:  I think this initialization of mProjectRate doesn't matter
    // because it is always reassigned in DoEffect before it is used
    // STF: but can't call AudioIOBase::GetOptimalSupportedSampleRate() here.
    // (Which is called to compute the default-default value.)  (Bug 2280)
    mProjectRate = QualitySettings::DefaultSampleRate.ReadWithDefault(44100);
}

EffectBase::~EffectBase() = default;

void EffectBase::SetTracks(TrackList* pTracks)
{
    mTracks = pTracks ? pTracks->shared_from_this() : nullptr;
}

double EffectBase::GetDefaultDuration()
{
    return 30.0;
}

// TODO:  Lift the possible user-prompting part out of this function, so that
// the recursive paths into this function via Effect::Delegate are simplified,
// and we don't have both EffectSettings and EffectSettingsAccessPtr
// If pAccess is not null, settings should have come from its Get()
bool EffectBase::DoEffect(EffectSettings& settings,
                          const InstanceFinder& finder,
                          double projectRate,
                          TrackList* list,
                          WaveTrackFactory* factory,
                          NotifyingSelectedRegion& selectedRegion,
                          unsigned flags,
                          const EffectSettingsAccessPtr& pAccess)
{
    auto cleanup0 = valueRestorer(mUIFlags, flags);
    wxASSERT(selectedRegion.duration() >= 0.0);

    mFactory = factory;
    mProjectRate = projectRate;

    SetTracks(list);
    // Don't hold a dangling pointer when done
    Finally Do([&]{ SetTracks(nullptr); });

    // This is for performance purposes only, no additional recovery implied
    auto& pProject = *const_cast<AudacityProject*>(FindProject()); // how to remove this const_cast?
    TransactionScope trans(pProject, "Effect");

    // Update track/group counts
    CountWaveTracks();

    bool isSelection = false;

    auto duration = 0.0;
    if (GetType() == EffectTypeGenerate) {
        GetConfig(GetDefinition(), PluginSettings::Private,
                  CurrentSettingsGroup(),
                  EffectSettingsExtra::DurationKey(), duration, GetDefaultDuration());
    }

    WaveTrack* newTrack{};
    bool success = false;
    auto oldDuration = duration;

    auto cleanup = finally([&] {
        if (!success) {
            if (newTrack) {
                mTracks->Remove(*newTrack);
            }
            // On failure, restore the old duration setting
            settings.extra.SetDuration(oldDuration);
        } else {
            trans.Commit();
        }

        mPresetNames.clear();
    });

    // We don't yet know the effect type for code in the Nyquist Prompt, so
    // assume it requires a track and handle errors when the effect runs.
    if ((GetType() == EffectTypeGenerate || GetPath() == NYQUIST_PROMPT_ID) && (mNumTracks == 0)) {
        auto track = mFactory->Create();
        track->SetName(mTracks->MakeUniqueTrackName(WaveTrack::GetDefaultAudioTrackNamePreference()));
        newTrack = mTracks->Add(track);
        newTrack->SetSelected(true);
    }

    mT0 = selectedRegion.t0();
    mT1 = selectedRegion.t1();
    if (mT1 > mT0) {
        // there is a selection: let's fit in there...
        // MJS: note that this is just for the TTC and is independent of the track rate
        // but we do need to make sure we have the right number of samples at the project rate
        double quantMT0 = QUANTIZED_TIME(mT0, mProjectRate);
        double quantMT1 = QUANTIZED_TIME(mT1, mProjectRate);
        duration = quantMT1 - quantMT0;
        isSelection = true;
        mT1 = mT0 + duration;
    }

    // This is happening inside EffectSettingsAccess::ModifySettings
    auto newFormat = (isSelection
                      ? NumericConverterFormats::TimeAndSampleFormat()
                      : NumericConverterFormats::DefaultSelectionFormat()
                      ).Internal();
    auto updater = [&](EffectSettings& settings) {
        settings.extra.SetDuration(duration);
        settings.extra.SetDurationFormat(newFormat);
        return nullptr;
    };
    // Update our copy of settings; update the EffectSettingsAccess too,
    // if we are going to show a dialog
    updater(settings);
    if (pAccess) {
        pAccess->ModifySettings(updater);
    }

    mF0 = selectedRegion.f0();
    mF1 = selectedRegion.f1();
    if (mF0 != SelectedRegion::UndefinedFrequency) {
        mPresetNames.push_back(L"control-f0");
    }
    if (mF1 != SelectedRegion::UndefinedFrequency) {
        mPresetNames.push_back(L"control-f1");
    }

    CountWaveTracks();

    // Allow the dialog factory to fill this in, but it might not
    std::shared_ptr<EffectInstance> pInstance;

    if (IsInteractive()) {
        if (!finder) {
            return false;
        } else if (auto result = finder(settings)) {
            pInstance = *result;
        } else {
            return false;
        }
    }

    auto pInstanceEx = std::dynamic_pointer_cast<EffectInstanceEx>(pInstance);
    if (!pInstanceEx) {
        // Path that skipped the dialog factory -- effect may be non-interactive
        // or this is batch mode processing or repeat of last effect with stored
        // settings.
        pInstanceEx = std::dynamic_pointer_cast<EffectInstanceEx>(MakeInstance());
        // Note: Init may read parameters from preferences
        if (!pInstanceEx || !pInstanceEx->Init()) {
            return false;
        }
    }

    // If the dialog was shown, then it has been closed without errors or
    // cancellation, and any change of duration has been saved in the config file

    bool returnVal = true;
    bool skipFlag = CheckWhetherSkipEffect(settings);
    if (skipFlag == false) {
        using namespace BasicUI;
        auto name = GetName();
        auto progress = MakeProgress(
            name,
            XO("Applying %s...").Format(name),
            ProgressShowCancel
            );
        auto vr = valueRestorer(mProgress, progress.get());

        assert(pInstanceEx); // null check above
        returnVal = pInstanceEx->Process(settings);
    }

    if (returnVal && (mT1 >= mT0)) {
        selectedRegion.setTimes(mT0, mT1);
    }

    success = returnVal;
    return returnVal;
}

void EffectBase::SetLinearEffectFlag(bool linearEffectFlag)
{
    mIsLinearEffect = linearEffectFlag;
}

void EffectBase::SetPreviewFullSelectionFlag(bool previewDurationFlag)
{
    mPreviewFullSelection = previewDurationFlag;
}

const AudacityProject* EffectBase::FindProject() const
{
    if (!inputTracks()) {
        return nullptr;
    }
    return inputTracks()->GetOwner();
}

void EffectBase::CountWaveTracks()
{
    const auto range = mTracks->Selected<const WaveTrack>();
    mNumTracks = range.sum(&WaveTrack::NChannels);
    mNumGroups = range.size();
}

std::any EffectBase::BeginPreview(const EffectSettings&)
{
    return {};
}

auto EffectBase::FindInstance(EffectPlugin& plugin)
-> std::optional<InstancePointer>
{
    auto result = plugin.MakeInstance();
    if (auto pInstanceEx = std::dynamic_pointer_cast<EffectInstanceEx>(result)
        ; pInstanceEx && pInstanceEx->Init()) {
        return { pInstanceEx };
    }
    return {};
}

auto EffectBase::DefaultInstanceFinder(EffectPlugin& plugin) -> InstanceFinder
{
    return [&plugin](auto&) { return FindInstance(plugin); };
}
