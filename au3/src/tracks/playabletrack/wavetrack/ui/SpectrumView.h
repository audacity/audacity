/**********************************************************************

Audacity: A Digital Audio Editor

SpectrumView.h

Paul Licameli split from WaveChannelView.h

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_VIEW__
#define __AUDACITY_SPECTRUM_VIEW__

#include <functional>
#include <map>
#include <set>
#include "WaveChannelView.h" // to inherit

class WaveTrack;
class BrushHandle;
using HopsAndBinsMap = std::map<long long, std::set<int> >;

class SpectralData
{
private:
    double mSampleRate;
    int mWindowSize;
    int mHopSize;
    long long mStartSample;
    long long mEndSample;

public:
    SpectralData(double sr)
        : mSampleRate(sr)
        , mWindowSize(2048)
        , mHopSize(mWindowSize / 4)
        // Set start and end in reverse for comparison during data addition
        , mStartSample(std::numeric_limits<long long>::max())
        , mEndSample(0)
    {}
    SpectralData(const SpectralData& src) = delete;

    HopsAndBinsMap dataBuffer;
    std::vector<HopsAndBinsMap> dataHistory;
    // TODO: replace with two pairs to save space
    std::vector<std::pair<int, int> > coordHistory;

    // Abstracted the copy method for future extension
    void CopyFrom(const SpectralData& src)
    {
        mStartSample = src.GetStartSample();
        mEndSample = src.GetEndSample();

        // std containers will perform deepcopy automatically
        dataHistory = src.dataHistory;
        dataBuffer = src.dataBuffer;
        coordHistory = src.coordHistory;
    }

    int GetHopSize() const
    {
        return mHopSize;
    }

    int GetWindowSize() const
    {
        return mWindowSize;
    }

    double GetSR() const
    {
        return mSampleRate;
    }

    long long GetStartSample() const
    {
        return mStartSample;
    }

    long long GetEndSample() const
    {
        return mEndSample;
    }

    long long GetCorrectedStartSample() const
    {
        // Correct the start of range so that the first full window is
        // centered at that position
        return std::max<long long>(0, GetStartSample() - 2 * GetHopSize());
    }

    long long GetLength() const
    {
        return GetEndSample() - GetCorrectedStartSample();
    }

    // The double time points is quantized into long long
    void addHopBinData(int hopNum, int freqBin)
    {
        // Update the start and end sampleCount of current selection
        if (hopNum * mHopSize > mEndSample) {
            mEndSample = hopNum * mHopSize;
        }
        if (hopNum * mHopSize < mStartSample) {
            mStartSample = hopNum * mHopSize;
        }

        dataBuffer[hopNum].insert(freqBin);
    }

    void removeHopBinData(int hopNum, int freqBin)
    {
        // TODO: Recalculate the start and end in case hop falls in 0 || end
        for (auto& dataBuf: dataHistory) {
            if (dataBuf.find(hopNum) != dataBuf.end()) {
                dataBuf[hopNum].erase(freqBin);
            }
        }
    }

    void clearAllData()
    {
        // DataBuffer should be clear when the user release cursor
        dataHistory.clear();
        mStartSample = std::numeric_limits<long long>::max();
        mEndSample = 0;
    }

    void saveAndClearBuffer()
    {
        dataHistory.emplace_back(dataBuffer);
        dataBuffer.clear();
        coordHistory.clear();
    }
};

class SpectrumView final : public WaveChannelSubView
{
    SpectrumView& operator=(const SpectrumView&) = delete;
public:
    SpectrumView(WaveChannelView& waveChannelView, const SpectrumView& src)
    = delete;
    explicit SpectrumView(WaveChannelView& waveChannelView);
    ~SpectrumView() override;

    const Type& SubViewType() const override;

    std::shared_ptr<ChannelVRulerControls> DoGetVRulerControls() override;

    std::shared_ptr<SpectralData> GetSpectralData();

    bool IsSpectral() const override;

    static int mBrushRadius;

    void CopyToSubView(WaveChannelSubView* destSubView) const override;

    class SpectralDataSaver;

private:
    std::weak_ptr<BrushHandle> mBrushHandle;
    std::shared_ptr<SpectralData> mpSpectralData, mpBackupSpectralData;
    bool mOnBrushTool;

    // TrackPanelDrawable implementation
    void Draw(
        TrackPanelDrawingContext& context, const wxRect& rect, unsigned iPass) override;

    void DoDraw(TrackPanelDrawingContext& context, const WaveChannel& channel, const WaveTrack::Interval* selectedClip, const wxRect& rect);

    std::vector<UIHandlePtr> DetailedHitTest(
        const TrackPanelMouseState& state, const AudacityProject* pProject, int currentTool, bool bMultiTool)
    override;

    unsigned CaptureKey(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project) override;

    unsigned KeyDown(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project) override;

    unsigned Char(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow* pParent, AudacityProject* project) override;

    static void ForAll(AudacityProject& project, std::function<void(SpectrumView& view)> fn);

    void DoSetMinimized(bool minimized) override;
};

#endif
