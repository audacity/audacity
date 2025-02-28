/**********************************************************************

  Audacity: A Digital Audio Editor

  PlotSpectrumBase.cpp

  Dominic Mazzoni
  Matthieu Hodgkinson split from FreqWindow.cpp

**********************************************************************/
#include "PlotSpectrumBase.h"
#include "BasicUI.h"
#include "Prefs.h"
#include "SampleFormat.h"
#include "ViewInfo.h"
#include "WaveTrack.h"

PlotSpectrumBase::PlotSpectrumBase(AudacityProject& project)
    : mProject{&project}
    , mAnalyst(std::make_unique<SpectrumAnalyst>())
{
    mRate = 0;
    mDataLen = 0;

    gPrefs->Read(wxT("/FrequencyPlotDialog/DrawGrid"), &mDrawGrid, true);
    gPrefs->Read(wxT("/FrequencyPlotDialog/SizeChoice"), &mSize, 3);

    int alg;
    gPrefs->Read(wxT("/FrequencyPlotDialog/AlgChoice"), &alg, 0);
    mAlg = static_cast<SpectrumAnalyst::Algorithm>(alg);

    gPrefs->Read(wxT("/FrequencyPlotDialog/FuncChoice"), &mFunc, 3);
    gPrefs->Read(wxT("/FrequencyPlotDialog/AxisChoice"), &mAxis, 1);
}

bool PlotSpectrumBase::GetAudio()
{
    mData.reset();
    mDataLen = 0;

    int selcount = 0;
    bool warning = false;
    for (auto track : TrackList::Get(*mProject).Selected<const WaveTrack>()) {
        auto& selectedRegion = ViewInfo::Get(*mProject).selectedRegion;
        auto start = track->TimeToLongSamples(selectedRegion.t0());
        if (selcount == 0) {
            mRate = track->GetRate();
            auto end = track->TimeToLongSamples(selectedRegion.t1());
            auto dataLen = end - start;
            // Permit approximately 46.60 minutes of selected samples at
            // a sampling frequency of 48 kHz (11.65 minutes at 192 kHz).
            auto maxDataLen = size_t(2) << 26;
            if (dataLen > maxDataLen) {
                warning = true;
                mDataLen = maxDataLen;
            } else {
                mDataLen = dataLen.as_size_t();
            }
            mData = Floats { mDataLen };
        }
        const auto nChannels = track->NChannels();
        if (track->GetRate() != mRate) {
            using namespace BasicUI;
            ShowMessageBox(
                XO("To plot the spectrum, all selected tracks must have the same sample rate."),
                MessageBoxOptions {}.Caption(XO("Error")).IconStyle(Icon::Error));
            mData.reset();
            mDataLen = 0;
            return false;
        }
        Floats buffer1 { mDataLen };
        Floats buffer2 { mDataLen };
        float* const buffers[] { buffer1.get(), buffer2.get() };
        // Don't allow throw for bad reads
        if (!track->GetFloats(
                0, nChannels, buffers, start, mDataLen, false,
                FillFormat::fillZero, false)) {
            using namespace BasicUI;
            ShowMessageBox(
                XO(
                    "Audio could not be analyzed. This may be due to a stretched or pitch-shifted clip.\nTry resetting any stretched clips, or mixing and rendering the tracks before analyzing"),
                MessageBoxOptions {}.Caption(XO("Error")).IconStyle(Icon::Error));
            mData.reset();
            mDataLen = 0;
            return false;
        }
        size_t iChannel = 0;
        if (selcount == 0) {
            // First channel -- assign into mData
            for (size_t i = 0; i < mDataLen; i++) {
                mData[i] = buffers[0][i];
            }
            ++iChannel;
        }
        // Later channels -- accumulate
        for (; iChannel < nChannels; ++iChannel) {
            const auto buffer = buffers[iChannel];
            for (size_t i = 0; i < mDataLen; i++) {
                mData[i] += buffer[i];
            }
        }
        ++selcount;
    }

    if (selcount == 0) {
        return false;
    }

    if (warning) {
        auto msg
            =XO("Too much audio was selected. Only the first %.1f seconds of audio will be analyzed.")
              .Format(mDataLen / mRate);
        BasicUI::ShowMessageBox(msg);
    }
    return true;
}
