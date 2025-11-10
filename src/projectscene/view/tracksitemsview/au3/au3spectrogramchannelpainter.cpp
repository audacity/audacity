/*
 * Audacity: A Digital Audio Editor
 */
#include "au3spectrogramchannelpainter.h"

#include "./ClipParameters.h"
#include "./SpectrumCache.h"
#include "./wavepainterutils.h" // TODO generalize
#include "../../../internal/au3/viewinfo.h"

#include "framework/global/log.h"

#include "libraries/lib-time-frequency-selection/SelectedRegion.h"

namespace au::projectscene {
using Au3SelectedRegion = ::SelectedRegion;

namespace {
std::pair<sampleCount, sampleCount> GetSelectedSampleIndices(
    const Au3SelectedRegion& selectedRegion, const WaveChannelInterval& clip,
    bool trackIsSelected)
{
    if (!trackIsSelected) {
        return { 0, 0 };
    }
    const double t0 = selectedRegion.t0(); // left selection bound
    const double t1 = selectedRegion.t1(); // right selection bound
    const auto startTime = clip.GetPlayStartTime();
    const auto s0 = std::max(sampleCount(0), clip.TimeToSamples(t0 - startTime));
    auto s1 = std::clamp(
        clip.TimeToSamples(t1 - startTime), sampleCount { 0 },
        clip.GetVisibleSampleCount());
    return { s0, s1 };
}
}

Au3SpectrogramChannelPainter::Params::Params(SpectrogramSettings& settings,
                                             const SelectedRegion& selectedRegion,
                                             const ZoomInfo& zoomInfo,
                                             bool trackIsSelected)
    : settings{settings}
    , zoomInfo{zoomInfo}
    , selectedRegion{selectedRegion}
    , trackIsSelected{trackIsSelected}
{
}

Au3SpectrogramChannelPainter::Au3SpectrogramChannelPainter(std::weak_ptr<au3::Au3Project> au3Project)
    : m_au3Project{std::move(au3Project)}
{
}

void Au3SpectrogramChannelPainter::paint(QPainter& painter, WaveClipChannel& clipChannel, const WaveMetrics& metrics, const Params& params)
{
    SpectrogramSettings& settings = params.settings;
    Au3SelectedRegion selectedRegion;
    selectedRegion.setT0(params.selectedRegion.t0);
    selectedRegion.setT1(params.selectedRegion.t1);
    selectedRegion.setF0(params.selectedRegion.f0);
    selectedRegion.setF1(params.selectedRegion.f1);

    const QRect rect{ static_cast<int>(metrics.left), 0, static_cast<int>(metrics.width), static_cast<int>(metrics.height) };
    const ClipParameters clipParams { clipChannel, rect, params.zoomInfo };

    const QRect& hiddenMid = clipParams.hiddenMid;
    // The "hiddenMid" rect contains the part of the display actually
    // containing the waveform, as it appears without the fisheye.  If it's empty, we're done.
    if (hiddenMid.width() <= 0) {
        return;
    }

    const double& t0 = clipParams.t0;
    const double playStartTime = clipChannel.GetPlayStartTime();

    const auto [ssel0, ssel1] = GetSelectedSampleIndices(selectedRegion, clipChannel, params.trackIsSelected);
    const double& averagePixelsPerSecond = clipParams.averagePixelsPerSecond;
    const double sampleRate = clipChannel.GetRate();
    const double stretchRatio = clipChannel.GetStretchRatio();
    const double& hiddenLeftOffset = clipParams.hiddenLeftOffset;
    const double& leftOffset = clipParams.leftOffset;
    const QRect& mid = clipParams.mid;

    double freqLo = SelectedRegion::UndefinedFrequency;
    double freqHi = SelectedRegion::UndefinedFrequency;
    freqLo = selectedRegion.f0();
    freqHi = selectedRegion.f1();

    const int& colorScheme = settings.colorScheme;
    const int& range = settings.range;
    const int& gain = settings.gain;

    const float* spectrogram = nullptr;
    const sampleCount* where = nullptr;
    WaveClipSpectrumCache::Get(clipChannel).GetSpectrogram(clipChannel, spectrogram, settings, where,
                                                           hiddenMid.width(), t0, averagePixelsPerSecond);

    auto nBins = settings.NBins();

    float minFreq, maxFreq;
    SpectrogramBounds::Get(channel).GetBounds(channel, minFreq, maxFreq);

    const SpectrogramSettings::ScaleType scaleType = settings.scaleType;

    // nearest frequency to each pixel row from number scale, for selecting
    // the desired fft bin(s) for display on that row
    float* bins = (float*)alloca(sizeof(*bins) * (hiddenMid.height + 1));
    {
        const NumberScale numberScale(settings.GetScale(minFreq, maxFreq));

        NumberScale::Iterator it = numberScale.begin(mid.height);
        float nextBin = std::max(0.0f, std::min(float(nBins - 1),
                                                settings.findBin(*it, binUnit)));

        int yy;
        for (yy = 0; yy < hiddenMid.height; ++yy) {
            bins[yy] = nextBin;
            nextBin = std::max(0.0f, std::min(float(nBins - 1),
                                              settings.findBin(*++it, binUnit)));
        }
        bins[yy] = nextBin;
    }

#ifdef EXPERIMENTAL_FFT_Y_GRID
    const float
        log2 = logf(2.0f),
        scale2 = (lmax - lmin) / log2,
        lmin2 = lmin / log2;

    ArrayOf<bool> yGrid{ size_t(mid.height) };
    for (int yy = 0; yy < mid.height; ++yy) {
        float n = (float(yy) / mid.height * scale2 - lmin2) * 12;
        float n2 = (float(yy + 1) / mid.height * scale2 - lmin2) * 12;
        float f = float(minFreq) / (fftSkipPoints + 1) * powf(2.0f, n / 12.0f + lmin2);
        float f2 = float(minFreq) / (fftSkipPoints + 1) * powf(2.0f, n2 / 12.0f + lmin2);
        n = logf(f / 440) / log2 * 12;
        n2 = logf(f2 / 440) / log2 * 12;
        if (floor(n) < floor(n2)) {
            yGrid[yy] = true;
        } else {
            yGrid[yy] = false;
        }
    }
#endif //EXPERIMENTAL_FFT_Y_GRID

    auto& clipCache = WaveClipSpectrumCache::Get(clip);
    auto& specPxCache = clipCache.mSpecPxCaches[clip.GetChannelIndex()];
    if (!updated && specPxCache
        && ((int)specPxCache->len == hiddenMid.height * hiddenMid.width)
        && scaleType == specPxCache->scaleType
        && gain == specPxCache->gain
        && range == specPxCache->range
        && minFreq == specPxCache->minFreq
        && maxFreq == specPxCache->maxFreq
#ifdef EXPERIMENTAL_FFT_Y_GRID
        && fftYGrid == fftYGridOld
#endif //EXPERIMENTAL_FFT_Y_GRID
#ifdef EXPERIMENTAL_FIND_NOTES
        && fftFindNotes == artist->fftFindNotesOld
        && findNotesMinA == artist->findNotesMinAOld
        && numberOfMaxima == artist->findNotesNOld
        && findNotesQuantize == artist->findNotesQuantizeOld
#endif
        ) {
        // Wave clip's spectrum cache is up to date,
        // and so is the spectrum pixel cache
    } else {
        // Update the spectrum pixel cache
        specPxCache = std::make_unique<SpecPxCache>(hiddenMid.width * hiddenMid.height);
        specPxCache->scaleType = scaleType;
        specPxCache->gain = gain;
        specPxCache->range = range;
        specPxCache->minFreq = minFreq;
        specPxCache->maxFreq = maxFreq;
#ifdef EXPERIMENTAL_FIND_NOTES
        artist->fftFindNotesOld = fftFindNotes;
        artist->findNotesMinAOld = findNotesMinA;
        artist->findNotesNOld = numberOfMaxima;
        artist->findNotesQuantizeOld = findNotesQuantize;
#endif

#ifdef EXPERIMENTAL_FIND_NOTES
        float log2 = logf(2.0f),
              lmin = logf(minFreq), lmax = logf(maxFreq), scale = lmax - lmin,
              lmins = lmin,
              lmaxs = lmax
        ;
#endif //EXPERIMENTAL_FIND_NOTES

#ifdef EXPERIMENTAL_FIND_NOTES
        int maxima[128];
        float maxima0[128], maxima1[128];
        const float
            f2bin = half / (sampleRate / 2.0f),
            bin2f = 1.0f / f2bin,
            minDistance = powf(2.0f, 2.0f / 12.0f),
            i0 = expf(lmin) / binUnit,
            i1 = expf(scale + lmin) / binUnit,
            minColor = 0.0f;
        const size_t maxTableSize = 1024;
        ArrayOf<int> indexes{ maxTableSize };
#endif //EXPERIMENTAL_FIND_NOTES

#ifdef _OPENMP
#pragma omp parallel for
#endif
        for (int xx = 0; xx < hiddenMid.width; ++xx) {
#ifdef EXPERIMENTAL_FIND_NOTES
            int maximas = 0;
            const int x0 = nBins * xx;
            if (fftFindNotes) {
                for (int i = maxTableSize - 1; i >= 0; i--) {
                    indexes[i] = -1;
                }

                // Build a table of (most) values, put the index in it.
                for (int i = (int)(i0); i < (int)(i1); i++) {
                    float freqi = freq[x0 + (int)(i)];
                    int value = (int)((freqi + gain + range) / range * (maxTableSize - 1));
                    if (value < 0) {
                        value = 0;
                    }
                    if (value >= maxTableSize) {
                        value = maxTableSize - 1;
                    }
                    indexes[value] = i;
                }
                // Build from the indices an array of maxima.
                for (int i = maxTableSize - 1; i >= 0; i--) {
                    int index = indexes[i];
                    if (index >= 0) {
                        float freqi = freq[x0 + index];
                        if (freqi < findNotesMinA) {
                            break;
                        }

                        bool ok = true;
                        for (int m = 0; m < maximas; m++) {
                            // Avoid to store very close maxima.
                            float maxm = maxima[m];
                            if (maxm / index < minDistance && index / maxm < minDistance) {
                                ok = false;
                                break;
                            }
                        }
                        if (ok) {
                            maxima[maximas++] = index;
                            if (maximas >= numberOfMaxima) {
                                break;
                            }
                        }
                    }
                }

// The f2pix helper macro converts a frequency into a pixel coordinate.
#define f2pix(f) (logf(f) - lmins) / (lmaxs - lmins) * hiddenMid.height

                // Possibly quantize the maxima frequencies and create the pixel block limits.
                for (int i = 0; i < maximas; i++) {
                    int index = maxima[i];
                    float f = float(index) * bin2f;
                    if (findNotesQuantize) {
                        f = expf((int)(log(f / 440) / log2 * 12 - 0.5) / 12.0f * log2) * 440;
                        maxima[i] = f * f2bin;
                    }
                    float f0 = expf((log(f / 440) / log2 * 24 - 1) / 24.0f * log2) * 440;
                    maxima0[i] = f2pix(f0);
                    float f1 = expf((log(f / 440) / log2 * 24 + 1) / 24.0f * log2) * 440;
                    maxima1[i] = f2pix(f1);
                }
            }

            int it = 0;
            bool inMaximum = false;
#endif //EXPERIMENTAL_FIND_NOTES

            for (int yy = 0; yy < hiddenMid.height; ++yy) {
                const float bin     = bins[yy];
                const float nextBin = bins[yy + 1];

                if (settings.scaleType != SpectrogramSettings::stLogarithmic) {
                    const float value = findValue
                                            (freq + nBins * xx, bin, nextBin, nBins, autocorrelation, gain, range);
                    specPxCache->values[xx * hiddenMid.height + yy] = value;
                } else {
                    float value;

#ifdef EXPERIMENTAL_FIND_NOTES
                    if (fftFindNotes) {
                        if (it < maximas) {
                            float i0 = maxima0[it];
                            if (yy >= i0) {
                                inMaximum = true;
                            }

                            if (inMaximum) {
                                float i1 = maxima1[it];
                                if (yy + 1 <= i1) {
                                    value = findValue(freq + x0, bin, nextBin, nBins, autocorrelation, gain, range);
                                    if (value < findNotesMinA) {
                                        value = minColor;
                                    }
                                } else {
                                    it++;
                                    inMaximum = false;
                                    value = minColor;
                                }
                            } else {
                                value = minColor;
                            }
                        } else {
                            value = minColor;
                        }
                    } else
#endif //EXPERIMENTAL_FIND_NOTES
                    {
                        value = findValue
                                    (freq + nBins * xx, bin, nextBin, nBins, autocorrelation, gain, range);
                    }
                    specPxCache->values[xx * hiddenMid.height + yy] = value;
                } // logF
            } // each yy
        } // each xx
    } // updating cache

    float selBinLo = settings.findBin(freqLo, binUnit);
    float selBinHi = settings.findBin(freqHi, binUnit);
    float selBinCenter = (freqLo < 0 || freqHi < 0)
                         ? -1
                         : settings.findBin(sqrt(freqLo * freqHi), binUnit);

    const bool isSpectral = settings.SpectralSelectionEnabled();
    const bool hidden = (ZoomInfo::HIDDEN == zoomInfo.GetFisheyeState());
    const int begin = hidden
                      ? 0
                      : std::max(0, (int)(zoomInfo.GetFisheyeLeftBoundary(-leftOffset)));
    const int end = hidden
                    ? 0
                    : std::min(mid.width, (int)(zoomInfo.GetFisheyeRightBoundary(-leftOffset)));
    const size_t numPixels = std::max(0, end - begin);

    SpecCache specCache;

    // need explicit resize since specCache.where[] accessed before Populate()
    specCache.Grow(numPixels, settings, -1, t0);

    if (numPixels > 0) {
        for (int ii = begin; ii < end; ++ii) {
            const double time = zoomInfo.PositionToTime(ii, -leftOffset) - playStartTime;
            specCache.where[ii - begin]
                =sampleCount(0.5 + sampleRate / stretchRatio * time);
        }
        specCache.Populate(
            settings, clip, 0, 0, numPixels,
            0 // FIXME: PRL -- make reassignment work with fisheye
            );
    }

    // build color gradient tables (not thread safe)
    if (!AColor::gradient_inited) {
        AColor::PreComputeGradient();
    }

    // left pixel column of the fisheye
    int fisheyeLeft = zoomInfo.GetFisheyeLeftBoundary(-leftOffset);

    // Bug 2389 - always draw at least one pixel of selection.
    int selectedX = zoomInfo.TimeToPosition(selectedRegion.t0(), -leftOffset);

#ifdef _OPENMP
#pragma omp parallel for
#endif

    const NumberScale numberScale(settings.GetScale(minFreq, maxFreq));
    int windowSize = mpSpectralData->GetWindowSize();
    int hopSize = mpSpectralData->GetHopSize();
    double sr = mpSpectralData->GetSR();
    auto& dataHistory = mpSpectralData->dataHistory;

    // Lazy way to add all hops and bins required for rendering
    dataHistory.push_back(mpSpectralData->dataBuffer);

    // Generate combined hops and bins map for rendering
    std::map<long long, std::set<int> > hopBinMap;
    for (auto vecIter = dataHistory.begin(); vecIter != dataHistory.end(); ++vecIter) {
        for (const auto& hopMap: *vecIter) {
            for (const auto& binNum: hopMap.second) {
                hopBinMap[hopMap.first].insert(binNum);
            }
        }
    }

    // Lambda for converting yy (not mouse coord!) to respective freq. bins
    auto yyToFreqBin = [&](int yy){
        const double p = double(yy) / hiddenMid.height;
        float convertedFreq = numberScale.PositionToValue(p);
        float convertedFreqBinNum = convertedFreq / (sr / windowSize);

        // By default lrintf will round to nearest by default, rounding to even on tie.
        // std::round that was used here before rounds halfway cases away from zero.
        // However, we can probably tolerate rounding issues here, as this will only slightly affect
        // the visuals.
        return static_cast<int>(lrintf(convertedFreqBinNum));
    };

    for (int xx = 0; xx < mid.width; ++xx) {
        int correctedX = xx + leftOffset - hiddenLeftOffset;

        // in fisheye mode the time scale has changed, so the row values aren't cached
        // in the loop above, and must be fetched from fft cache
        float* uncached;
        if (!zoomInfo.InFisheye(xx, -leftOffset)) {
            uncached = 0;
        } else {
            int specIndex = (xx - fisheyeLeft) * nBins;
            wxASSERT(specIndex >= 0 && specIndex < (int)specCache.freq.size());
            uncached = &specCache.freq[specIndex];
        }

        // zoomInfo must be queried for each column since with fisheye enabled
        // time between columns is variable
        const auto w0 = sampleCount(
            0.5 + sampleRate / stretchRatio
            * (zoomInfo.PositionToTime(xx, -leftOffset) - playStartTime));

        const auto w1 = sampleCount(
            0.5 + sampleRate / stretchRatio
            * (zoomInfo.PositionToTime(xx + 1, -leftOffset) - playStartTime));

        bool maybeSelected = ssel0 <= w0 && w1 < ssel1;
        maybeSelected = maybeSelected || (xx == selectedX);

        // In case the xx matches the hop number, it will be used as iterator for frequency bins
        std::set<int>* pSelectedBins = nullptr;
        std::set<int>::iterator freqBinIter;
        auto advanceFreqBinIter = [&](int nextBinRounded){
            while (freqBinIter != pSelectedBins->end()
                   && *freqBinIter < nextBinRounded) {
                ++freqBinIter;
            }
        };

        bool hitHopNum = false;
        if (onBrushTool) {
            int convertedHopNum = (w0.as_long_long() + hopSize / 2) / hopSize;
            hitHopNum = (hopBinMap.find(convertedHopNum) != hopBinMap.end());
            if (hitHopNum) {
                pSelectedBins = &hopBinMap[convertedHopNum];
                freqBinIter = pSelectedBins->begin();
                advanceFreqBinIter(yyToFreqBin(0));
            }
        }

        for (int yy = 0; yy < hiddenMid.height; ++yy) {
            if (onBrushTool) {
                maybeSelected = false;
            }
            const float bin     = bins[yy];
            const float nextBin = bins[yy + 1];
            auto binRounded = yyToFreqBin(yy);
            auto nextBinRounded = yyToFreqBin(yy + 1);

            if (hitHopNum
                && freqBinIter != pSelectedBins->end()
                && binRounded == *freqBinIter) {
                maybeSelected = true;
            }

            if (hitHopNum) {
                advanceFreqBinIter(nextBinRounded);
            }

            // For spectral selection, determine what colour
            // set to use.  We use a darker selection if
            // in both spectral range and time range.

            AColor::ColorGradientChoice selected = AColor::ColorGradientUnselected;

            // If we are in the time selected range, then we may use a different color set.
            if (maybeSelected) {
                selected
                    =ChooseColorSet(bin, nextBin, selBinLo, selBinCenter, selBinHi,
                                    (xx + leftOffset - hiddenLeftOffset) / DASH_LENGTH, isSpectral);
                if (onBrushTool && selected != AColor::ColorGradientUnselected) {
                    // use only two sets of colors
                    selected = AColor::ColorGradientTimeAndFrequencySelected;
                }
            }

            const float value = uncached
                                ? findValue(uncached, bin, nextBin, nBins, autocorrelation, gain, range)
                                : specPxCache->values[correctedX * hiddenMid.height + yy];

            unsigned char rv, gv, bv;
            GetColorGradient(value, selected, colorScheme, &rv, &gv, &bv);

#ifdef EXPERIMENTAL_FFT_Y_GRID
            if (fftYGrid && yGrid[yy]) {
                rv /= 1.1f;
                gv /= 1.1f;
                bv /= 1.1f;
            }
#endif //EXPERIMENTAL_FFT_Y_GRID
            int px = ((mid.height - 1 - yy) * mid.width + xx);
#ifdef EXPERIMENTAL_SPECTROGRAM_OVERLAY
            // More transparent the closer to zero intensity.
            alpha[px]= wxMin(200, (value + 0.3) * 500);
#endif
            px *=3;
            data[px++] = rv;
            data[px++] = gv;
            data[px] = bv;
        } // each yy
    } // each xx

    dataHistory.pop_back();
    wxBitmap converted = wxBitmap(image);

    wxMemoryDC memDC;

    memDC.SelectObject(converted);

    dc.Blit(mid.x, mid.y, mid.width, mid.height, &memDC, 0, 0, wxCOPY, FALSE);

    // Draw clip edges, as also in waveform view, which improves the appearance
    // of split views
    {
        auto clipRect = ClipParameters::GetClipRect(clip, zoomInfo, rect);
        TrackArt::DrawClipEdges(dc, clipRect, selected);
    }
}
}
