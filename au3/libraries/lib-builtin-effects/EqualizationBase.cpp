#include "EqualizationBase.h"
#include "BasicUI.h"
#include "EffectOutputTracks.h"
#include "WaveClip.h"
#include "WaveTrack.h"

const EffectParameterMethods& EqualizationBase::Parameters() const
{
    static CapturedParameters<
        EqualizationBase, EqualizationParameters::FilterLength,
        // CurveName,
        EqualizationParameters::InterpLin,
        // Pretty sure the interpolation name shouldn't have been interpreted when
        // specified in chains, but must keep it that way for compatibility.
        EqualizationParameters::InterpMeth>
    parameters { [](EqualizationBase& effect, EffectSettings&,
                    EqualizationParameters& params, bool updating) {
            constexpr auto nInterpolations
                =EqualizationParameters::nInterpolations;
            if (updating) {
                if (params.mInterp >= nInterpolations) {
                    params.mInterp -= nInterpolations;
                }
            }
            return true;
        } };
    return parameters;
}

///----------------------------------------------------------------------------
// EqualizationBase
//----------------------------------------------------------------------------

const ComponentInterfaceSymbol EqualizationBase::Symbol { XO("Equalization") };

EqualizationBase::EqualizationBase(int Options)
    : mParameters{GetDefinition()}
    , mOptions{Options}
{
    auto& hiFreq = mParameters.mHiFreq;
    auto& curves = mCurvesList.mCurves;

    Parameters().Reset(*this);

    SetLinearEffectFlag(true);

    // Load the EQ curves
    EQCurveReader { curves, GetName(), mOptions }.LoadCurves();

    // Note: initial curve is set in TransferDataToWindow

    // double loLog = log10(mLoFreq);
    // double stepLog = (log10(hiFreq) - loLog)/((double)NUM_PTS-1.);

    // We expect these Hi and Lo frequencies to be overridden by Init().
    // Don't use inputTracks().  See bug 2321.
#if 0
    auto trackList = inputTracks();
    const auto t = trackList
                   ? *trackList->Any<const WaveTrack>().first
                   : nullptr
    ;
    hiFreq
        =(t
          ? t->GetRate()
          : mProjectRate)
          / 2.0;
#endif
    hiFreq = mProjectRate / 2.0;
}

EqualizationBase::~EqualizationBase()
{
}

// ComponentInterface implementation

TranslatableString EqualizationBase::GetDescription() const
{
    return XO("Adjusts the volume levels of particular frequencies");
}

ManualPageID EqualizationBase::ManualPage() const
{
    // Bug 2509: Must use _ and not space in names.
    if (mOptions == kEqOptionGraphic) {
        return L"Graphic_EQ";
    }
    if (mOptions == kEqOptionCurve) {
        return L"Filter_Curve_EQ";
    }
    return L"Equalization";
}

// EffectDefinitionInterface implementation

EffectType EqualizationBase::GetType() const
{
    return EffectTypeProcess;
}

bool EqualizationBase::VisitSettings(
    ConstSettingsVisitor& visitor, const EffectSettings& settings) const
{
    const auto& curves = mCurvesList.mCurves;
    Effect::VisitSettings(visitor, settings);

    // Curve point parameters -- how many isn't known statically
    if (dynamic_cast<ShuttleGetAutomation*>(&visitor)) {
        int numPoints = curves[0].points.size();
        int point;
        for (point = 0; point < numPoints; point++) {
            const wxString nameFreq = wxString::Format("f%i", point);
            const wxString nameVal = wxString::Format("v%i", point);
            visitor.Define(
                curves[0].points[point].Freq, nameFreq, 0.0, 0.0, 0.0, 0.0);
            visitor.Define(
                curves[0].points[point].dB, nameVal, 0.0, 0.0, 0.0, 0.0);
        }
    }
    return true;
}

bool EqualizationBase::VisitSettings(
    SettingsVisitor& visitor, EffectSettings& settings)
{
    auto& curves = mCurvesList.mCurves;
    Effect::VisitSettings(visitor, settings);

    // Curve point parameters -- how many isn't known statically
    {
        curves[0].points.clear();

        for (int i = 0; i < 200; i++) {
            const wxString nameFreq = wxString::Format("f%i", i);
            const wxString nameVal = wxString::Format("v%i", i);
            double f = -1000.0;
            double d = 0.0;
            visitor.Define(f, nameFreq, 0.0, -10000.0, 1000000.0, 0.0);
            visitor.Define(d, nameVal, 0.0, -10000.0, 10000.0, 0.0);
            if (f <= 0.0) {
                break;
            }
            curves[0].points.push_back(EQPoint(f, d));
        }
        mCurvesList.setCurve(0);
    }
    return true;
}

OptionalMessage
EqualizationBase::LoadFactoryDefaults(EffectSettings& settings) const
{
    // To do: externalize state so const_cast isn't needed
    if (!const_cast<EqualizationBase&>(*this).DoLoadFactoryDefaults(settings)) {
        return {}
    }
    return { nullptr };
}

OptionalMessage
EqualizationBase::DoLoadFactoryDefaults(EffectSettings& settings)
{
    mParameters.LoadDefaults(mOptions);
    return Effect::LoadFactoryDefaults(settings);
}

// Constants determining who the prests are for.
const bool kCURVE = false;
const bool kBOTH = true;

static const struct
{
    const bool bForBoth; // more extended set is used for Filter EQ
    // See Bug 2254 for rationale.
    const TranslatableString name;
    const wxChar* values;
} FactoryPresets
[] = {
    { kCURVE, XO("100Hz Rumble"),
      wxT(
          "f0=\"20.0\" v0=\"-80.0\" f1=\"49.237316986327\" v1=\"-33.107692718506\" f2=\"54.196034330446\" v2=\"-29.553844451904\" f3=\"88.033573501041\" v3=\"-6.923076629639\" f4=\"95.871851182279\" v4=\"-4.523078918457\" f5=\"108.957037410504\" v5=\"-1.938461303711\" f6=\"123.828171198057\" v6=\"-0.73846244812\" f7=\"149.228077614658\" v7=\"-0.092308044434\"") },
    { kCURVE, XO("AM Radio"),
      wxT(
          "f0=\"20.0\" v0=\"-63.67\" f1=\"31.0\" v1=\"-33.219\" f2=\"50.0\" v2=\"-3.01\" f3=\"63.0\" v3=\"-0.106\" f4=\"100.0\" v4=\"0.0\" f5=\"2500.0\" v5=\"0.0\" f6=\"4000.0\" v6=\"-0.614\" f7=\"5000.0\" v7=\"-8.059\" f8=\"8000.0\" v8=\"-39.981\" f9=\"20000.0\" v9=\"-103.651\" f10=\"48000.0\" v10=\"-164.485\"") },
    { kBOTH, XO("Bass Boost"),
      wxT("f0=\"100.0\" v0=\"9.0\" f1=\"500.0\" v1=\"0.0\"") },
    { kBOTH, XO("Bass Cut"),
      wxT("f0=\"150.0\" v0=\"-50.0\" f1=\"300.0\" v1=\"0.0\"") },
    { kCURVE, XO("Low rolloff for speech"),
      wxT(
          "f0=\"50.0\" v0=\"-120.0\" f1=\"60.0\" v1=\"-50.0\" f2=\"65.0\" v2=\"-24.0\" f3=\"70.0\" v3=\"-12.0\" f4=\"80.0\" v4=\"-4.0\" f5=\"90.0\" v5=\"-1.0\" f6=\"100.0\" v6=\"0.0\"") },
    { kBOTH, XO("RIAA"), wxT(
          "f0=\"20.0\" v0=\"19.274\" f1=\"25.0\" v1=\"18.954\" f2=\"31.0\" v2=\"18.516\" f3=\"40.0\" v3=\"17.792\" f4=\"50.0\" v4=\"16.946\" f5=\"63.0\" v5=\"15.852\" f6=\"80.0\" v6=\"14.506\" f7=\"100.0\" v7=\"13.088\" f8=\"125.0\" v8=\"11.563\" f9=\"160.0\" v9=\"9.809\" f10=\"200.0\" v10=\"8.219\" f11=\"250.0\" v11=\"6.677\" f12=\"315.0\" v12=\"5.179\" f13=\"400.0\" v13=\"3.784\" f14=\"500.0\" v14=\"2.648\" f15=\"630.0\" v15=\"1.642\" f16=\"800.0\" v16=\"0.751\" f17=\"1000.0\" v17=\"0.0\" f18=\"1250.0\" v18=\"-0.744\" f19=\"1600.0\" v19=\"-1.643\" f20=\"2000.0\" v20=\"-2.589\" f21=\"2500.0\" v21=\"-3.7\" f22=\"3150.0\" v22=\"-5.038\" f23=\"4000.0\" v23=\"-6.605\" f24=\"5000.0\" v24=\"-8.21\" f25=\"6300.0\" v25=\"-9.98\" f26=\"8000.0\" v26=\"-11.894\" f27=\"10000.0\" v27=\"-13.734\" f28=\"12500.0\" v28=\"-15.609\" f29=\"16000.0\" v29=\"-17.708\" f30=\"20000.0\" v30=\"-19.62\" f31=\"25000.0\" v31=\"-21.542\" f32=\"48000.0\" v32=\"-27.187\"") },
    { kCURVE, XO("Telephone"),
      wxT(
          "f0=\"20.0\" v0=\"-94.087\" f1=\"200.0\" v1=\"-14.254\" f2=\"250.0\" v2=\"-7.243\" f3=\"315.0\" v3=\"-2.245\" f4=\"400.0\" v4=\"-0.414\" f5=\"500.0\" v5=\"0.0\" f6=\"2500.0\" v6=\"0.0\" f7=\"3150.0\" v7=\"-0.874\" f8=\"4000.0\" v8=\"-3.992\" f9=\"5000.0\" v9=\"-9.993\" f10=\"48000.0\" v10=\"-88.117\"") },
    { kBOTH, XO("Treble Boost"),
      wxT("f0=\"4000.0\" v0=\"0.0\" f1=\"5000.0\" v1=\"9.0\"") },
    { kBOTH, XO("Treble Cut"),
      wxT("f0=\"6000.0\" v0=\"0.0\" f1=\"10000.0\" v1=\"-110.0\"") },
    { kCURVE, XO("Walkie-talkie"),
      wxT(
          "f0=\"100.0\" v0=\"-120.0\" f1=\"101.0\" v1=\"0.0\" f2=\"2000.0\" v2=\"0.0\" f3=\"2001.0\" v3=\"-120.0\"") },
};

RegistryPaths EqualizationBase::GetFactoryPresets() const
{
    RegistryPaths names;

    for (size_t i = 0; i < WXSIZEOF(FactoryPresets); i++) {
        if (
            (mOptions == kEqOptionGraphic)
            && (FactoryPresets[i].bForBoth == false)) {
            continue;
        }
        names.push_back(FactoryPresets[i].name.Translation());
    }

    return names;
}

OptionalMessage
EqualizationBase::LoadFactoryPreset(int id, EffectSettings& settings) const
{
    int index = -1;
    for (size_t i = 0; i < WXSIZEOF(FactoryPresets); i++) {
        if (
            (mOptions == kEqOptionGraphic)
            && (FactoryPresets[i].bForBoth == false)) {
            continue;
        }
        if (id-- == 0) {
            index = i;
            break;
        }
    }
    if (index < 0) {
        return {}
    }

    // mParams =
    wxString params = FactoryPresets[index].values;

    CommandParameters eap(params);
    ShuttleSetAutomation S;
    S.SetForWriting(&eap);
    // To do: externalize state so const_cast isn't needed
    if (!const_cast<EqualizationBase*>(this)->VisitSettings(S, settings)) {
        return {}
    }
    return { nullptr };
}

// Effect implementation

bool EqualizationBase::Init()
{
    constexpr auto loFreqI = EqualizationFilter::loFreqI;

    const auto& lin = mParameters.mLin;
    const auto& curveName = mParameters.mCurveName;
    auto& loFreq = mParameters.mLoFreq;
    auto& hiFreq = mParameters.mHiFreq;

    int selcount = 0;
    double rate = 0.0;

    if (const auto project = FindProject()) {
        auto trackRange = TrackList::Get(*project).Selected<const WaveTrack>();
        if (trackRange) {
            rate = (*(trackRange.first++))->GetRate();
            ++selcount;

            for (auto track : trackRange) {
                if (track->GetRate() != rate) {
                    BasicUI::ShowMessageBox(XO(
                                                "To apply Equalization, all selected tracks must have the same sample rate."));
                    return false;
                }
                ++selcount;
            }
        }
    } else {
        // Editing macro parameters, use this default
        rate = 44100.0;
    }

    hiFreq = rate / 2.0;
    // Unlikely, but better than crashing.
    if (hiFreq <= loFreqI) {
        BasicUI::ShowMessageBox(
            XO("Track sample rate is too low for this effect."));
        return false;
    }

    loFreq = loFreqI;

    mCurvesList.setCurve(curveName);

    mParameters.CalcFilter();

    return true;
}

bool EqualizationBase::Process(EffectInstance&, EffectSettings&)
{
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } } };
    mParameters.CalcFilter();
    bool bGoodResult = true;

    int count = 0;
    for (auto track : outputs.Get().Selected<WaveTrack>()) {
        double trackStart = track->GetStartTime();
        double trackEnd = track->GetEndTime();
        double t0 = mT0 < trackStart ? trackStart : mT0;
        double t1 = mT1 > trackEnd ? trackEnd : mT1;

        if (t1 > t0) {
            auto start = track->TimeToLongSamples(t0);
            auto end = track->TimeToLongSamples(t1);
            auto len = end - start;

            auto pTempTrack = track->EmptyCopy();
            pTempTrack->ConvertToSampleFormat(floatSample);
            auto iter0 = pTempTrack->Channels().begin();

            for (const auto pChannel : track->Channels()) {
                constexpr auto windowSize = EqualizationFilter::windowSize;
                const auto& M = mParameters.mM;

                wxASSERT(M - 1 < windowSize);
                size_t L = windowSize - (M - 1); // Process L samples at a go
                auto s = start;
                auto idealBlockLen = pChannel->GetMaxBlockSize() * 4;
                if (idealBlockLen % L != 0) {
                    idealBlockLen += (L - (idealBlockLen % L));
                }
                auto pNewChannel = *iter0++;
                Task task { M, idealBlockLen, *pNewChannel };
                bGoodResult = ProcessOne(task, count, *pChannel, start, len);
                if (!bGoodResult) {
                    goto done;
                }
            }
            pTempTrack->Flush();
            // Remove trailing data from the temp track
            pTempTrack->Clear(t1 - t0, pTempTrack->GetEndTime());
            track->ClearAndPaste(t0, t1, *pTempTrack, true, true);
        }

        count++;
    }
done:

    if (bGoodResult) {
        outputs.Commit();
    }

    return bGoodResult;
}

// EqualizationBase implementation

bool EqualizationBase::ProcessOne(
    Task& task, int count, const WaveChannel& t, sampleCount start,
    sampleCount len)
{
    constexpr auto windowSize = EqualizationFilter::windowSize;

    const auto& M = mParameters.mM;

    wxASSERT(M - 1 < windowSize);
    size_t L = windowSize - (M - 1); // Process L samples at a go
    auto s = start;

    auto& buffer = task.buffer;
    auto& window1 = task.window1;
    auto& window2 = task.window2;
    auto& thisWindow = task.thisWindow;
    auto& lastWindow = task.lastWindow;

    auto originalLen = len;

    auto& output = task.output;

    TrackProgress(count, 0.);
    bool bLoopSuccess = true;
    size_t wcopy = 0;

    while (len != 0)
    {
        auto block = limitSampleBufferSize(task.idealBlockLen, len);

        t.GetFloats(buffer.get(), s, block);

        for (size_t i = 0; i < block;
             i += L) { // go through block in lumps of length L
            wcopy = std::min<size_t>(L, block - i);
            for (size_t j = 0; j < wcopy; j++) {
                thisWindow[j] = buffer[i + j]; // copy the L (or remaining) samples
            }
            for (auto j = wcopy; j < windowSize; j++) {
                thisWindow[j] = 0; // this includes the padding
            }
            mParameters.Filter(windowSize, thisWindow);

            // Overlap - Add
            for (size_t j = 0; (j < M - 1) && (j < wcopy); j++) {
                buffer[i + j] = thisWindow[j] + lastWindow[L + j];
            }
            for (size_t j = M - 1; j < wcopy; j++) {
                buffer[i + j] = thisWindow[j];
            }

            std::swap(thisWindow, lastWindow);
        } // next i, lump of this block

        task.AccumulateSamples((samplePtr)buffer.get(), block);
        len -= block;
        s += block;

        if (TrackProgress(
                count, (s - start).as_double() / originalLen.as_double())) {
            bLoopSuccess = false;
            break;
        }
    }

    if (bLoopSuccess) {
        // M-1 samples of 'tail' left in lastWindow, get them now
        if (wcopy < (M - 1)) {
            // Still have some overlap left to process
            // (note that lastWindow and thisWindow have been exchanged at this
            // point
            //  so that 'thisWindow' is really the window prior to 'lastWindow')
            size_t j = 0;
            for (; j < M - 1 - wcopy; j++) {
                buffer[j] = lastWindow[wcopy + j] + thisWindow[L + wcopy + j];
            }
            // And fill in the remainder after the overlap
            for (; j < M - 1; j++) {
                buffer[j] = lastWindow[wcopy + j];
            }
        } else {
            for (size_t j = 0; j < M - 1; j++) {
                buffer[j] = lastWindow[wcopy + j];
            }
        }
        task.AccumulateSamples((samplePtr)buffer.get(), M - 1);
    }
    return bLoopSuccess;
}
