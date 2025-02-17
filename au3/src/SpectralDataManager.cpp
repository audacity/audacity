/**********************************************************************

  Audacity: A Digital Audio Editor

  SpectralDataManager.cpp

  Edward Hui

*******************************************************************//*!

\class SpectralDataManager
\brief Performs the calculation for spectral editing

*//*******************************************************************/

#include <iostream>
#include "FFT.h"
#include "ProjectHistory.h"
#include "SpectralDataManager.h"
#include "WaveTrack.h"

SpectralDataManager::SpectralDataManager() = default;

SpectralDataManager::~SpectralDataManager() = default;

struct SpectralDataManager::Setting {
    eWindowFunctions mInWindowType = eWinFuncHann;
    eWindowFunctions mOutWindowType = eWinFuncHann;
    size_t mWindowSize = 2048;
    unsigned mStepsPerWindow = 4;
    bool mLeadingPadding = true;
    bool mTrailingPadding = true;
    bool mNeedOutput = true;
};

namespace {
const std::shared_ptr<SpectralData> FindSpectralData(Channel* pChannel)
{
    auto& view = ChannelView::Get(*pChannel);
    if (auto waveChannelViewPtr = dynamic_cast<WaveChannelView*>(&view)) {
        for (const auto& subViewPtr : waveChannelViewPtr->GetAllSubViews()) {
            if (subViewPtr->IsSpectral()) {
                auto sView
                    =std::static_pointer_cast<SpectrumView>(subViewPtr).get();
                const auto pData = sView->GetSpectralData();
                if (!pData->dataHistory.empty()) {
                    return pData;
                }
            }
        }
    }
    return {};
}
}

bool SpectralDataManager::ProcessTracks(AudacityProject& project)
{
    auto& tracks = TrackList::Get(project);
    int applyCount = 0;
    Setting setting;
    for (auto wt : tracks.Any<WaveTrack>()) {
        using Type = long long;
        Type startSample{ std::numeric_limits<Type>::max() };
        Type endSample{ std::numeric_limits<Type>::min() };
        for (auto pChannel : wt->Channels()) {
            if (const auto pData = FindSpectralData(pChannel.get())) {
                const auto& hopSize = pData->GetHopSize();
                auto start = pData->GetStartSample();
                endSample = std::max(endSample, pData->GetEndSample());

                // Correct the start of range so that the first full window is
                // centered at that position
                start = std::max(static_cast<long long>(0), start - 2 * hopSize);
                startSample = std::min(startSample, start);
            }
        }
        if (startSample >= endSample) {
            continue;
        }
        const auto t0 = wt->LongSamplesToTime(startSample);
        const auto len = endSample - startSample;
        const auto tLen = wt->LongSamplesToTime(len);
        auto tempTrack = wt->EmptyCopy();
        auto iter = tempTrack->Channels().begin();
        long long processed{};
        for (auto pChannel : wt->Channels()) {
            Worker worker{ (*iter++).get(), setting };
            auto& view = ChannelView::Get(*pChannel);

            if (auto waveChannelViewPtr = dynamic_cast<WaveChannelView*>(&view)) {
                for (const auto& subViewPtr : waveChannelViewPtr->GetAllSubViews()) {
                    if (!subViewPtr->IsSpectral()) {
                        continue;
                    }
                    auto sView = std::static_pointer_cast<SpectrumView>(subViewPtr).get();
                    auto pSpectralData = sView->GetSpectralData();

                    if (!pSpectralData->dataHistory.empty()) {
                        // TODO make this correct in case start or end of spectral data in
                        // the channels differs
                        processed = std::max(processed, pSpectralData->GetLength());
                        worker.Process(*pChannel, pSpectralData);
                        applyCount += static_cast<int>(pSpectralData->dataHistory.size());
                        pSpectralData->clearAllData();
                    }
                }
            }
        }
        if (tempTrack) {
            TrackSpectrumTransformer::PostProcess(*tempTrack, processed);
            // Take the output track and insert it in place of the original
            // sample data
            // TODO make this correct in case start or end of spectral data in
            // the channels differs
            wt->ClearAndPaste(t0, t0 + tLen, *tempTrack, true, false);
        }
    }

    if (applyCount) {
        ProjectHistory::Get(project).PushState(
            XO("Applied effect to selection"),
            XO("Applied effect to selection"));
        ProjectHistory::Get(project).ModifyState(true);
    }

    return applyCount > 0;
}

int SpectralDataManager::FindFrequencySnappingBin(const WaveChannel& channel,
                                                  long long int startSC, int hopSize, double threshold, int targetFreqBin)
{
    Setting setting;
    setting.mNeedOutput = false;
    Worker worker{ nullptr, setting };

    return worker.ProcessSnapping(
        channel, startSC, hopSize, setting.mWindowSize, threshold, targetFreqBin);
}

std::vector<int> SpectralDataManager::FindHighestFrequencyBins(WaveChannel& wc,
                                                               long long int startSC,
                                                               int hopSize,
                                                               double threshold,
                                                               int targetFreqBin)
{
    Setting setting;
    setting.mNeedOutput = false;
    Worker worker{ nullptr, setting };

    return worker.ProcessOvertones(wc, startSC, hopSize, setting.mWindowSize, threshold, targetFreqBin);
}

SpectralDataManager::Worker::Worker(
    WaveChannel* pChannel, const Setting& setting)
    : TrackSpectrumTransformer{pChannel,
                               setting.mNeedOutput, setting.mInWindowType, setting.mOutWindowType,
                               setting.mWindowSize, setting.mStepsPerWindow,
                               setting.mLeadingPadding, setting.mTrailingPadding
                               }
// Work members
{
}

SpectralDataManager::Worker::~Worker() = default;

bool SpectralDataManager::Worker::DoStart()
{
    return TrackSpectrumTransformer::DoStart();
}

bool SpectralDataManager::Worker::DoFinish()
{
    return TrackSpectrumTransformer::DoFinish();
}

bool SpectralDataManager::Worker::Process(const WaveChannel& channel,
                                          const std::shared_ptr<SpectralData>& pSpectralData)
{
    mpSpectralData = pSpectralData;
    const auto hopSize = mpSpectralData->GetHopSize();
    const auto startSample = mpSpectralData->GetStartSample();
    // Correct the first hop num, because SpectrumTransformer will send
    // a few initial windows that overlay the range only partially
    mStartHopNum = startSample / hopSize - (mStepsPerWindow - 1);
    mWindowCount = 0;
    return TrackSpectrumTransformer::Process(Processor, channel, 1,
                                             mpSpectralData->GetCorrectedStartSample(), mpSpectralData->GetLength());
}

int SpectralDataManager::Worker::ProcessSnapping(const WaveChannel& channel,
                                                 long long startSC, int hopSize, size_t winSize, double threshold,
                                                 int targetFreqBin)
{
    mSnapThreshold = threshold;
    mSnapTargetFreqBin = targetFreqBin;
    mSnapSamplingRate = channel.GetTrack().GetRate();

    startSC = std::max(static_cast<long long>(0), startSC - 2 * hopSize);
    // The calculated frequency peak will be stored in mReturnFreq
    if (!TrackSpectrumTransformer::Process(SnappingProcessor, channel,
                                           1, startSC, winSize)) {
        return 0;
    }

    return mSnapReturnFreqBin;
}

std::vector<int> SpectralDataManager::Worker::ProcessOvertones(
    const WaveChannel& channel, long long startSC, int hopSize, size_t winSize,
    double threshold, int targetFreqBin)
{
    mOvertonesThreshold = threshold;
    mSnapTargetFreqBin = targetFreqBin;
    mSnapSamplingRate = channel.GetTrack().GetRate();

    startSC = std::max(static_cast<long long>(0), startSC - 2 * hopSize);
    // The calculated multiple frequency peaks will be stored in mOvertonesTargetFreqBin
    TrackSpectrumTransformer::Process(
        OvertonesProcessor, channel, 1, startSC, winSize);
    return move(mOvertonesTargetFreqBin);
}

bool SpectralDataManager::Worker::SnappingProcessor(SpectrumTransformer& transformer)
{
    auto& worker = static_cast<Worker&>(transformer);
    // Compute power spectrum in the newest window
    {
        MyWindow& record = worker.NthWindow(0);
        float* pSpectrum = &record.mSpectrums[0];
        const double dc = record.mRealFFTs[0];
        *pSpectrum++ = dc * dc;
        float* pReal = &record.mRealFFTs[1], * pImag = &record.mImagFFTs[1];
        for (size_t nn = worker.mSpectrumSize - 2; nn--;) {
            const double re = *pReal++, im = *pImag++;
            *pSpectrum++ = re * re + im * im;
        }
        const double nyquist = record.mImagFFTs[0];
        *pSpectrum = nyquist * nyquist;

        const double& sr = worker.mSnapSamplingRate;
        const double nyquistRate = sr / 2;
        const double& threshold = worker.mSnapThreshold;
        const double& spectrumSize = worker.mSpectrumSize;
        const int& targetBin = worker.mSnapTargetFreqBin;

        int binBound = spectrumSize * threshold;
        float maxValue = std::numeric_limits<float>::min();

        // Skip the first and last bin
        for (int i = -binBound; i < binBound; i++) {
            int idx = std::clamp(targetBin + i, 0, static_cast<int>(spectrumSize - 1));
            if (record.mSpectrums[idx] > maxValue) {
                maxValue = record.mSpectrums[idx];
                // Update the return frequency
                worker.mSnapReturnFreqBin = idx;
            }
        }
    }

    return true;
}

bool SpectralDataManager::Worker::OvertonesProcessor(SpectrumTransformer& transformer)
{
    auto& worker = static_cast<Worker&>(transformer);
    // Compute power spectrum in the newest window
    {
        MyWindow& record = worker.NthWindow(0);
        float* pSpectrum = &record.mSpectrums[0];
        const double dc = record.mRealFFTs[0];
        *pSpectrum++ = dc * dc;
        float* pReal = &record.mRealFFTs[1], * pImag = &record.mImagFFTs[1];
        for (size_t nn = worker.mSpectrumSize - 2; nn--;) {
            const double re = *pReal++, im = *pImag++;
            *pSpectrum++ = re * re + im * im;
        }
        const double nyquist = record.mImagFFTs[0];
        *pSpectrum = nyquist * nyquist;

        const double& spectrumSize = worker.mSpectrumSize;
        const int& targetBin = worker.mSnapTargetFreqBin;

        float targetValue = record.mSpectrums[targetBin];

        double fundamental = targetBin;
        int overtone = 2, binNum = 0;
        pSpectrum = &record.mSpectrums[0];
        while (fundamental >= 1
               && (binNum = lrint(fundamental * overtone)) < spectrumSize) {
            // Examine a few bins each way up and down
            constexpr int tolerance = 3;
            auto begin = pSpectrum + std::max(0, binNum - (tolerance + 1));
            auto end = pSpectrum
                       + std::min<size_t>(spectrumSize, binNum + (tolerance + 1) + 1);
            auto peak = std::max_element(begin, end);

            // Abandon if the peak is too far up or down
            if (peak == begin || peak == end - 1) {
                break;
            }

            int newBin = peak - pSpectrum;
            worker.mOvertonesTargetFreqBin.push_back(newBin);
            // Correct the estimate of the fundamental
            fundamental = double(newBin) / overtone++;
        }
    }
    return true;
}

bool SpectralDataManager::Worker::Processor(SpectrumTransformer& transformer)
{
    auto& worker = static_cast<Worker&>(transformer);
    // Compute power spectrum in the newest window
    {
        MyWindow& record = worker.NthWindow(0);
        float* pSpectrum = &record.mSpectrums[0];
        const double dc = record.mRealFFTs[0];
        *pSpectrum++ = dc * dc;
        float* pReal = &record.mRealFFTs[1], * pImag = &record.mImagFFTs[1];
        for (size_t nn = worker.mSpectrumSize - 2; nn--;) {
            const double re = *pReal++, im = *pImag++;
            *pSpectrum++ = re * re + im * im;
        }
        const double nyquist = record.mImagFFTs[0];
        *pSpectrum = nyquist * nyquist;
    }

    worker.ApplyEffectToSelection();
    return true;
}

bool SpectralDataManager::Worker::ApplyEffectToSelection()
{
    auto& record = NthWindow(0);

    for (auto& spectralDataMap: mpSpectralData->dataHistory) {
        // For all added frequency
        for (const int& freqBin: spectralDataMap[mStartHopNum]) {
            record.mRealFFTs[freqBin] = 0;
            record.mImagFFTs[freqBin] = 0;
        }
    }

    mWindowCount++;
    mStartHopNum++;
    return true;
}

auto SpectralDataManager::Worker::NewWindow(size_t windowSize)
-> std::unique_ptr<Window>
{
    return std::make_unique<MyWindow>(windowSize);
}

SpectralDataManager::Worker::MyWindow::~MyWindow()
{
}
