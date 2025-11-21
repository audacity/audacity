/**********************************************************************

  Audacity: A Digital Audio Editor

  PaulstretchBase.cpp

  Nasca Octavian Paul (Paul Nasca)

*******************************************************************//**

\class PaulstretchBase
\brief An Extreme Time Stretch and Time Smear effect

*//*******************************************************************/
#include "PaulstretchBase.h"
#include "BasicUI.h"
#include "EffectOutputTracks.h"
#include "FFT.h"
#include "Prefs.h"
#include "SyncLock.h"
#include "TimeWarper.h"
#include "WaveTrack.h"
#include <algorithm>
#include <cfloat> // FLT_MAX
#include <cmath>

const ComponentInterfaceSymbol PaulstretchBase::Symbol { XO("Paulstretch") };

const EffectParameterMethods& PaulstretchBase::Parameters() const
{
    static CapturedParameters<PaulstretchBase, Amount, Time> parameters;
    return parameters;
}

/// \brief Class that helps EffectPaulStretch.  It does the FFTs and inner loop
/// of the effect.
class PaulStretch
{
public:
    PaulStretch(float rap_, size_t in_bufsize_, float samplerate_);
    // in_bufsize is also a half of a FFT buffer (in samples)
    virtual ~PaulStretch();

    void process(float* smps, size_t nsmps);

    size_t get_nsamples(); // how many samples are required to be added in the
                           // pool next time
    size_t get_nsamples_for_fill(); // how many samples are required to be added
                                    // for a complete buffer refill (at start of
                                    // the song or after seek)

private:
    void process_spectrum(float* WXUNUSED(freq)) {}

    const float samplerate;
    const float rap;
    const size_t in_bufsize;

public:
    const size_t out_bufsize;
    const Floats out_buf;

private:
    const Floats old_out_smp_buf;

public:
    const size_t
        poolsize; // how many samples are inside the input_pool size
                  // (need to know how many samples to fill when seeking)

private:
    const Floats in_pool; // de marimea in_bufsize

    double remained_samples; // how many fraction of samples has remained (0..1)

    const Floats fft_smps, fft_c, fft_s, fft_freq, fft_tmp;
};

PaulstretchBase::PaulstretchBase()
{
    Parameters().Reset(*this);

    SetLinearEffectFlag(true);
}

PaulstretchBase::~PaulstretchBase()
{
}

// ComponentInterface implementation

ComponentInterfaceSymbol PaulstretchBase::GetSymbol() const
{
    return Symbol;
}

TranslatableString PaulstretchBase::GetDescription() const
{
    return XO(
        "Paulstretch is only for an extreme time-stretch or \"stasis\" effect");
}

ManualPageID PaulstretchBase::ManualPage() const
{
    return L"Paulstretch";
}

// EffectDefinitionInterface implementation

EffectType PaulstretchBase::GetType() const
{
    return EffectTypeProcess;
}

// Effect implementation

double PaulstretchBase::CalcPreviewInputLength(
    const EffectSettings&, double previewLength) const
{
    // FIXME: Preview is currently at the project rate, but should really be
    // at the track rate (bugs 1284 and 852).
    auto minDuration = GetBufferSize(mProjectRate) * 2 + 1;

    // Preview playback may need to be trimmed but this is the smallest selection
    // that we can use.
    double minLength
        =std::max<double>(minDuration / mProjectRate, previewLength / mAmount);

    return minLength;
}

bool PaulstretchBase::Process(EffectInstance&, EffectSettings&)
{
    // Pass true because sync lock adjustment is needed
    EffectOutputTracks outputs { *mTracks, GetType(), { { mT0, mT1 } }, true };
    auto newT1 = mT1;
    int count = 0;
    // Process selected wave tracks first, to find the new t1 value
    for (const auto track : outputs.Get().Selected<WaveTrack>()) {
        double trackStart = track->GetStartTime();
        double trackEnd = track->GetEndTime();
        double t0 = mT0 < trackStart ? trackStart : mT0;
        double t1 = mT1 > trackEnd ? trackEnd : mT1;
        if (t1 > t0) {
            auto tempTrack = track->EmptyCopy();
            const auto channels = track->Channels();
            auto iter = tempTrack->Channels().begin();
            for (const auto pChannel : channels) {
                if (!ProcessOne(*pChannel, **iter++, t0, t1, count++)) {
                    return false;
                }
            }
            tempTrack->Flush();
            newT1 = std::max(newT1, mT0 + tempTrack->GetEndTime());
            PasteTimeWarper warper { t1, t0 + tempTrack->GetEndTime() };
            constexpr auto preserve = false;
            constexpr auto merge = true;
            track->ClearAndPaste(t0, t1, *tempTrack, preserve, merge, &warper);
        } else {
            count += track->NChannels();
        }
    }

    // Sync lock adjustment of other tracks
    outputs.Get().Any().Visit(
        [&](auto&& fallthrough) {
        return [&](WaveTrack& track) {
            if (!track.IsSelected()) {
                fallthrough();
            }
        };
    },
        [&](Track& track) {
        if (SyncLock::IsSyncLockSelected(track)) {
            track.SyncLockAdjust(mT1, newT1);
        }
    });
    mT1 = newT1;

    outputs.Commit();

    return true;
}

bool PaulstretchBase::ProcessOne(
    const WaveChannel& track, WaveChannel& outputTrack, double t0, double t1,
    int count)
{
    const auto badAllocMessage = XO("Requested value exceeds memory capacity.");

    const auto rate = track.GetTrack().GetRate();
    const auto stretch_buf_size = GetBufferSize(rate);
    if (stretch_buf_size == 0) {
        BasicUI::ShowMessageBox(badAllocMessage);
        return {};
    }

    double amount = this->mAmount;

    auto start = track.TimeToLongSamples(t0);
    auto end = track.TimeToLongSamples(t1);
    auto len = end - start;

    const auto minDuration = stretch_buf_size * 2 + 1;
    if (minDuration < stretch_buf_size) {
        // overflow!
        BasicUI::ShowMessageBox(badAllocMessage);
        return {};
    }

    if (len < minDuration) { // error because the selection is too short
        float maxTimeRes = log(len.as_double()) / log(2.0);
        maxTimeRes = pow(2.0, floor(maxTimeRes) + 0.5);
        maxTimeRes = maxTimeRes / rate;

        using namespace BasicUI;
        if (this->IsPreviewing()) {
            double defaultPreviewLen;
            gPrefs->Read(
                wxT("/AudioIO/EffectsPreviewLen"), &defaultPreviewLen, 6.0);

            if ((minDuration / mProjectRate) < defaultPreviewLen) {
                ShowMessageBox(
                    /* i18n-hint: 'Time Resolution' is the name of a control in the
                       Paulstretch effect.*/
                    XO("Audio selection too short to preview.\n\n"
                       "Try increasing the audio selection to at least %.1f seconds,\n"
                       "or reducing the 'Time Resolution' to less than %.1f seconds.")
                    .Format(
                        (minDuration / rate) + 0.05, // round up to 1/10 s.
                        floor(maxTimeRes * 10.0) / 10.0),
                    MessageBoxOptions {}.IconStyle(Icon::Warning));
            } else {
                ShowMessageBox(
                    /* i18n-hint: 'Time Resolution' is the name of a control in the
                       Paulstretch effect.*/
                    XO("Unable to Preview.\n\n"
                       "For the current audio selection, the maximum\n"
                       "'Time Resolution' is %.1f seconds.")
                    .Format(floor(maxTimeRes * 10.0) / 10.0),
                    MessageBoxOptions {}.IconStyle(Icon::Warning));
            }
        } else {
            ShowMessageBox(
                /* i18n-hint: 'Time Resolution' is the name of a control in the
                   Paulstretch effect.*/
                XO("The 'Time Resolution' is too long for the selection.\n\n"
                   "Try increasing the audio selection to at least %.1f seconds,\n"
                   "or reducing the 'Time Resolution' to less than %.1f seconds.")
                .Format(
                    (minDuration / rate) + 0.05, // round up to 1/10 s.
                    floor(maxTimeRes * 10.0) / 10.0),
                MessageBoxOptions {}.IconStyle(Icon::Warning));
        }

        return {};
    }

    auto dlen = len.as_double();
    double adjust_amount = dlen / (dlen - ((double)stretch_buf_size * 2.0));
    amount = 1.0 + (amount - 1.0) * adjust_amount;

    try
    {
        // This encloses all the allocations of buffers, including those in
        // the constructor of the PaulStretch object

        PaulStretch stretch(amount, stretch_buf_size, rate);

        auto nget = stretch.get_nsamples_for_fill();

        auto bufsize = stretch.poolsize;
        Floats buffer0 { bufsize };
        float* bufferptr0 = buffer0.get();
        bool first_time = true;

        const auto fade_len = std::min<size_t>(100, bufsize / 2 - 1);
        bool cancelled = false;

        {
            Floats fade_track_smps { fade_len };
            decltype(len) s = 0;

            while (s < len)
            {
                track.GetFloats(bufferptr0, start + s, nget);
                stretch.process(buffer0.get(), nget);

                if (first_time) {
                    stretch.process(buffer0.get(), 0);
                }

                s += nget;

                if (first_time) { // blend the start of the selection
                    track.GetFloats(fade_track_smps.get(), start, fade_len);
                    first_time = false;
                    for (size_t i = 0; i < fade_len; i++) {
                        float fi = (float)i / (float)fade_len;
                        stretch.out_buf[i]
                            =stretch.out_buf[i] * fi + (1.0 - fi) * fade_track_smps[i];
                    }
                }
                if (s >= len) { // blend the end of the selection
                    track.GetFloats(fade_track_smps.get(), end - fade_len, fade_len);
                    for (size_t i = 0; i < fade_len; i++) {
                        float fi = (float)i / (float)fade_len;
                        auto i2 = bufsize / 2 - 1 - i;
                        stretch.out_buf[i2]
                            =stretch.out_buf[i2] * fi
                              + (1.0 - fi) * fade_track_smps[fade_len - 1 - i];
                    }
                }

                outputTrack.Append(
                    (samplePtr)stretch.out_buf.get(), floatSample,
                    stretch.out_bufsize);

                nget = stretch.get_nsamples();
                if (TrackProgress(count, s.as_double() / len.as_double())) {
                    cancelled = true;
                    break;
                }
            }
        }

        if (!cancelled) {
            return true;
        }
    }
    catch (const std::bad_alloc&)
    {
        BasicUI::ShowMessageBox(badAllocMessage);
    }
    return false;
}

size_t PaulstretchBase::GetBufferSize(double rate) const
{
    // Audacity's fft requires a power of 2
    float tmp = rate * mTime_resolution / 2.0;
    tmp = log(tmp) / log(2.0);
    tmp = pow(2.0, floor(tmp + 0.5));

    auto stmp = size_t(tmp);
    if (stmp != tmp) {
        // overflow
        return 0;
    }
    if (stmp >= 2 * stmp) {
        // overflow
        return 0;
    }

    return std::max<size_t>(stmp, 128);
}

/*************************************************************/

PaulStretch::PaulStretch(float rap_, size_t in_bufsize_, float samplerate_)
    : samplerate{samplerate_}
    , rap{std::max(1.0f, rap_)}
    , in_bufsize{in_bufsize_}
    , out_bufsize{std::max(size_t { 8 }, in_bufsize)}
    , out_buf{out_bufsize}
    , old_out_smp_buf{out_bufsize * 2, true}
    , poolsize{in_bufsize_ * 2}
    , in_pool{poolsize, true}
    , remained_samples{0.0}
    , fft_smps{poolsize, true}
    , fft_c{poolsize, true}
    , fft_s{poolsize, true}
    , fft_freq{poolsize, true}
    , fft_tmp{poolsize}
{
}

PaulStretch::~PaulStretch()
{
}

void PaulStretch::process(float* smps, size_t nsmps)
{
    // add NEW samples to the pool
    if ((smps != NULL) && (nsmps != 0)) {
        if (nsmps > poolsize) {
            nsmps = poolsize;
        }
        int nleft = poolsize - nsmps;

        // move left the samples from the pool to make room for NEW samples
        for (int i = 0; i < nleft; i++) {
            in_pool[i] = in_pool[i + nsmps];
        }

        // add NEW samples to the pool
        for (size_t i = 0; i < nsmps; i++) {
            in_pool[i + nleft] = smps[i];
        }
    }

    // get the samples from the pool
    for (size_t i = 0; i < poolsize; i++) {
        fft_smps[i] = in_pool[i];
    }
    WindowFunc(eWinFuncHann, poolsize, fft_smps.get());

    RealFFT(poolsize, fft_smps.get(), fft_c.get(), fft_s.get());

    for (size_t i = 0; i < poolsize / 2; i++) {
        fft_freq[i] = sqrt(fft_c[i] * fft_c[i] + fft_s[i] * fft_s[i]);
    }
    process_spectrum(fft_freq.get());

    // put randomize phases to frequencies and do a IFFT
    float inv_2p15_2pi = 1.0 / 16384.0 * (float)M_PI;
    for (size_t i = 1; i < poolsize / 2; i++) {
        unsigned int random = (rand()) & 0x7fff;
        float phase = random * inv_2p15_2pi;
        float s = fft_freq[i] * sin(phase);
        float c = fft_freq[i] * cos(phase);

        fft_c[i] = fft_c[poolsize - i] = c;

        fft_s[i] = s;
        fft_s[poolsize - i] = -s;
    }
    fft_c[0] = fft_s[0] = 0.0;
    fft_c[poolsize / 2] = fft_s[poolsize / 2] = 0.0;

    FFT(poolsize, true, fft_c.get(), fft_s.get(), fft_smps.get(), fft_tmp.get());

    float max = 0.0, max2 = 0.0;
    for (size_t i = 0; i < poolsize; i++) {
        max = std::max(max, fabsf(fft_tmp[i]));
        max2 = std::max(max2, fabsf(fft_smps[i]));
    }

    // make the output buffer
    float tmp = 1.0 / (float)out_bufsize * M_PI;
    float hinv_sqrt2 = 0.853553390593f; //(1.0+1.0/sqrt(2))*0.5;

    float ampfactor = 1.0;
    if (rap < 1.0) {
        ampfactor = rap * 0.707;
    } else {
        ampfactor = (out_bufsize / (float)poolsize) * 4.0;
    }

    for (size_t i = 0; i < out_bufsize; i++) {
        float a = (0.5 + 0.5 * cos(i * tmp));
        float out
            =fft_smps[i + out_bufsize] * (1.0 - a) + old_out_smp_buf[i] * a;
        out_buf[i] = out
                     * (hinv_sqrt2 - (1.0 - hinv_sqrt2) * cos(i * 2.0 * tmp))
                     * ampfactor;
    }

    // copy the current output buffer to old buffer
    for (size_t i = 0; i < out_bufsize * 2; i++) {
        old_out_smp_buf[i] = fft_smps[i];
    }
}

size_t PaulStretch::get_nsamples()
{
    double r = out_bufsize / rap;
    auto ri = (size_t)floor(r);
    double rf = r - floor(r);

    remained_samples += rf;
    if (remained_samples >= 1.0) {
        ri += (size_t)floor(remained_samples);
        remained_samples = remained_samples - floor(remained_samples);
    }

    if (ri > poolsize) {
        ri = poolsize;
    }

    return ri;
}

size_t PaulStretch::get_nsamples_for_fill()
{
    return poolsize;
}
