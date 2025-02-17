/**********************************************************************

  Audacity: A Digital Audio Editor

  ContrastBase.cpp

*//*******************************************************************/
#include "ContrastBase.h"
#include "BasicUI.h"
#include "Prefs.h"
#include "Project.h"
#include "ViewInfo.h"
#include "WaveChannelUtilities.h"
#include "WaveTrack.h"

bool ContrastBase::GetDB(float& dB)
{
    float rms = float(0.0);

    // For stereo tracks: sqrt((mean(L)+mean(R))/2)
    double meanSq = 0.0;

    auto& p = GetProject();
    auto range = TrackList::Get(p).Selected<const WaveTrack>();
    auto numberSelectedTracks = range.size();
    using namespace BasicUI;
    if (numberSelectedTracks > 1) {
        ShowMessageBox(
            XO("You can only measure one track at a time."),
            MessageBoxOptions {}.IconStyle(Icon::Error));
        return false;
    }
    if (numberSelectedTracks == 0) {
        ShowMessageBox(
            XO("Please select an audio track."),
            MessageBoxOptions {}.IconStyle(Icon::Error));
        return false;
    }

    const auto first = *range.begin();
    const auto channels = first->Channels();
    assert(mT0 <= mT1);
    // Ignore whitespace beyond ends of track.
    mT0 = std::max(mT0, first->GetStartTime());
    mT1 = std::min(mT1, first->GetEndTime());
    for (auto t : channels) {
        auto SelT0 = t->TimeToLongSamples(mT0);
        auto SelT1 = t->TimeToLongSamples(mT1);

        if (SelT0 > SelT1) {
            ShowMessageBox(
                XO("Invalid audio selection.\nPlease ensure that audio is selected."),
                MessageBoxOptions {}.IconStyle(Icon::Error));
            return false;
        }

        if (SelT0 == SelT1) {
            ShowMessageBox(
                XO("Nothing to measure.\nPlease select a section of a track."),
                MessageBoxOptions {}.IconStyle(Icon::Error));
            return false;
        }

        // Don't throw in this analysis dialog
        rms = WaveChannelUtilities::GetRMS(*t, mT0, mT1, false);
        meanSq += rms * rms;
    }
    // TODO: This works for stereo, provided the audio clips are in both
    // channels. We should really count gaps between clips as silence.
    rms = (meanSq > 0.0) ? sqrt(meanSq / static_cast<double>(channels.size()))
          : 0.0;

    // Gives warning C4056, Overflow in floating-point constant arithmetic
    // -INFINITY is intentional here.
    // Looks like we are stuck with this warning, as
    // #pragma warning( disable : 4056)
    // even around the whole function does not disable it successfully.

    dB = (rms == 0.0) ? -INFINITY : LINEAR_TO_DB(rms);
    return true;
}

void ContrastBase::SetStartAndEndTime()
{
    auto& p = GetProject();
    auto& selectedRegion = ViewInfo::Get(p).selectedRegion;
    mT0 = selectedRegion.t0();
    mT1 = selectedRegion.t1();
}
