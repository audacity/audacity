/**********************************************************************

Audacity: A Digital Audio Editor

SpectrumView.cpp

Paul Licameli split from WaveChannelView.cpp

**********************************************************************/


#include "SpectrumView.h"

#include "SpectralDataManager.h" // Cycle :-(
#include "SpectrumCache.h"

#include "Sequence.h"
#include "Spectrum.h"

#include "ClipParameters.h"
#include "SpectrumVRulerControls.h"
#include "WaveChannelViewConstants.h"

#include "../../../ui/BrushHandle.h"

#include "AColor.h"
#include "PendingTracks.h"
#include "Prefs.h"
#include "NumberScale.h"
#include "../../../../TrackArt.h"
#include "../../../../TrackArtist.h"
#include "../../../../TrackPanelDrawingContext.h"
#include "ViewInfo.h"
#include "WaveClip.h"
#include "WaveTrack.h"
#include "WaveTrackLocation.h"
#include "WaveTrackUtilities.h"
#include "SpectrogramSettings.h"

#include <wx/dcmemory.h>
#include <wx/graphics.h>

#include "float_cast.h"

class BrushHandle;
class SpectralData;

static_assert(
   SpectrogramSettings::csNumColorScheme == AColor::colorSchemes,
   "Broken correspondence");

static WaveChannelSubView::Type sType{
   WaveChannelViewConstants::Spectrum,
   { wxT("Spectrogram"), XXO("&Spectrogram") }
};

static WaveChannelSubViewType::RegisteredType reg{ sType };

SpectrumView::SpectrumView(WaveChannelView &waveChannelView)
   : WaveChannelSubView(waveChannelView)
{
   const auto wt = FindWaveChannel();
   mpSpectralData = std::make_shared<SpectralData>(wt->GetRate());
   mOnBrushTool = false;
}

SpectrumView::~SpectrumView() = default;

bool SpectrumView::IsSpectral() const
{
   return true;
}

class SpectrumView::SpectralDataSaver : public BrushHandle::StateSaver {
public:
   explicit SpectralDataSaver( SpectrumView &view )
      : mView{ view }
   {}

   void Init( AudacityProject &project, bool clearAll ) override
   {
      mpProject = &project;
      ForAll( project, [this, clearAll](SpectrumView &view){
         auto pOldData = view.mpSpectralData;
         if (clearAll) {
            auto &pNewData = view.mpBackupSpectralData =
               std::make_shared<SpectralData>(pOldData->GetSR());
            pNewData->CopyFrom(*pOldData);
            pOldData->clearAllData();
         }
         else {
            // Back up one view only
            if (&mView == &view) {
               auto &pNewData = view.mpBackupSpectralData =
                  std::make_shared<SpectralData>(pOldData->GetSR());
               pNewData->CopyFrom( *pOldData );
            }
            else
               view.mpBackupSpectralData = {};
         }
      });
   }

   ~SpectralDataSaver() override
   {
      if (mpProject)
         ForAll( *mpProject, [this](SpectrumView &view){
            if (mCommitted) {
               // Discard all backups
               view.mpBackupSpectralData = {};
            }
            else {
               // Restore all
               if (auto &pBackupData = view.mpBackupSpectralData) {
                  view.mpSpectralData->CopyFrom(*pBackupData);
                  pBackupData.reset();
               }
            }
         });
   }

private:
   SpectrumView &mView;
   AudacityProject *mpProject = nullptr;
};

// This always hits, but details of the hit vary with mouse position and
// key state.
static UIHandlePtr BrushHandleHitTest(
   std::weak_ptr<BrushHandle> &holder,
   const TrackPanelMouseState &st, const AudacityProject *pProject,
   const std::shared_ptr<SpectrumView> &pChannelView,
   const std::shared_ptr<SpectralData> &mpData)
{
   const auto &viewInfo = ViewInfo::Get( *pProject );
   auto &projectSettings = ProjectSettings::Get( *pProject );
   auto result = std::make_shared<BrushHandle>(
      std::make_shared<SpectrumView::SpectralDataSaver>(*pChannelView),
      pChannelView, TrackList::Get(*pProject),
      st, viewInfo, mpData, projectSettings);

   result = AssignUIHandlePtr(holder, result);

   //Make sure we are within the selected track
   const auto pChannel = pChannelView->FindWaveChannel();
   if (!pChannel ||
       !pChannel->GetTrack().GetSelected())
   {
      return result;
   }

   return result;
}

void SpectrumView::ForAll( AudacityProject &project,
   std::function<void(SpectrumView &view)> fn )
{
   if (!fn)
      return;
   for (const auto wt : TrackList::Get(project).Any<WaveTrack>()) {
      for (auto pChannel : wt->Channels()) {
         if (auto pWaveChannelView =
             dynamic_cast<WaveChannelView*>(&ChannelView::Get(*pChannel))) {
            for (const auto &pSubView : pWaveChannelView->GetAllSubViews()) {
               if (const auto sView = dynamic_cast<SpectrumView*>(pSubView.get()))
                  fn( *sView );
            }
         }
      }
   }
}

std::vector<UIHandlePtr> SpectrumView::DetailedHitTest(
   const TrackPanelMouseState &state,
   const AudacityProject *pProject, int currentTool, bool bMultiTool )
{
   const auto wt = FindWaveChannel();
   std::vector<UIHandlePtr> results;

#ifdef EXPERIMENTAL_BRUSH_TOOL
   mOnBrushTool = (currentTool == ToolCodes::brushTool);
   if(mOnBrushTool){
      const auto result = BrushHandleHitTest(
         mBrushHandle, state,
         pProject, std::static_pointer_cast<SpectrumView>(shared_from_this()),
         mpSpectralData);
      results.push_back(result);
      return results;
   }
#endif

   return WaveChannelSubView::DoDetailedHitTest(
      state, pProject, currentTool, bMultiTool, wt
   ).second;
}

void SpectrumView::DoSetMinimized( bool minimized )
{
   const auto wt = FindWaveChannel();
   if (!wt)
      return;

   bool bHalfWave;
   gPrefs->Read(wxT("/GUI/CollapseToHalfWave"), &bHalfWave, false);
   if( bHalfWave && minimized)
   {
      // It is all right to set the top of scale to a huge number,
      // not knowing the track sampleRate here -- because when retrieving the
      // value, then we pass in a sample rate and clamp it above to the
      // Nyquist frequency.
      constexpr auto max = std::numeric_limits<float>::max();
      const bool spectrumLinear =
         (SpectrogramSettings::Get(*wt).scaleType ==
            SpectrogramSettings::stLinear);
      // Zoom out full
      SpectrogramBounds::Get(*wt)
         .SetBounds( spectrumLinear ? 0.0f : 1.0f, max );
   }

   ChannelView::DoSetMinimized(minimized);
}

auto SpectrumView::SubViewType() const -> const Type &
{
   return sType;
}

std::shared_ptr<ChannelVRulerControls> SpectrumView::DoGetVRulerControls()
{
   return std::make_shared<SpectrumVRulerControls>(shared_from_this());
}

std::shared_ptr<SpectralData> SpectrumView::GetSpectralData(){
   return mpSpectralData;
}

void SpectrumView::CopyToSubView(WaveChannelSubView *destSubView) const
{
   if (const auto pDest = dynamic_cast< SpectrumView* >(destSubView)) {
      pDest->mpSpectralData =
         std::make_shared<SpectralData>(mpSpectralData->GetSR());
      pDest->mpSpectralData->CopyFrom(*mpSpectralData);
   }
}

namespace
{

static inline float findValue
(const float *spectrum, float bin0, float bin1, unsigned nBins,
 bool autocorrelation, int gain, int range)
{
   float value;


#if 0
   // Averaging method
   if ((int)(bin1) == (int)(bin0)) {
      value = spectrum[(int)(bin0)];
   } else {
      float binwidth= bin1 - bin0;
      value = spectrum[(int)(bin0)] * (1.f - bin0 + (int)bin0);

      bin0 = 1 + (int)(bin0);
      while (bin0 < (int)(bin1)) {
         value += spectrum[(int)(bin0)];
         bin0 += 1.0;
      }
      // Do not reference past end of freq array.
      if ((int)(bin1) >= (int)nBins) {
         bin1 -= 1.0;
      }

      value += spectrum[(int)(bin1)] * (bin1 - (int)(bin1));
      value /= binwidth;
   }
#else
   // Maximum method, and no apportionment of any single bins over multiple pixel rows
   // See Bug971
   int index, limitIndex;
   if (autocorrelation) {
      // bin = 2 * nBins / (nBins - 1 - array_index);
      // Solve for index
      index = std::max(0.0f, std::min(float(nBins - 1),
         (nBins - 1) - (2 * nBins) / (std::max(1.0f, bin0))
      ));
      limitIndex = std::max(0.0f, std::min(float(nBins - 1),
         (nBins - 1) - (2 * nBins) / (std::max(1.0f, bin1))
      ));
   }
   else {
      index = std::min<int>(nBins - 1, (int)(floor(0.5 + bin0)));
      limitIndex = std::min<int>(nBins, (int)(floor(0.5 + bin1)));
   }
   value = spectrum[index];
   while (++index < limitIndex)
      value = std::max(value, spectrum[index]);
#endif
   if (!autocorrelation) {
      // Last step converts dB to a 0.0-1.0 range
      value = (value + range + gain) / (double)range;
   }
   value = std::min(1.0f, std::max(0.0f, value));
   return value;
}

// dashCount counts both dashes and the spaces between them.
inline AColor::ColorGradientChoice
ChooseColorSet( float bin0, float bin1, float selBinLo,
   float selBinCenter, float selBinHi, int dashCount, bool isSpectral )
{
   if (!isSpectral)
      return  AColor::ColorGradientTimeSelected;
   if ((selBinCenter >= 0) && (bin0 <= selBinCenter) &&
       (selBinCenter < bin1))
      return AColor::ColorGradientEdge;
   if ((0 == dashCount % 2) &&
       (((selBinLo >= 0) && (bin0 <= selBinLo) && ( selBinLo < bin1))  ||
        ((selBinHi >= 0) && (bin0 <= selBinHi) && ( selBinHi < bin1))))
      return AColor::ColorGradientEdge;
   if ((selBinLo < 0 || selBinLo < bin1) && (selBinHi < 0 || selBinHi > bin0))
      return  AColor::ColorGradientTimeAndFrequencySelected;

   return  AColor::ColorGradientTimeSelected;
}

std::pair<sampleCount, sampleCount> GetSelectedSampleIndices(
   const SelectedRegion& selectedRegion, const WaveChannelInterval& clip,
   bool trackIsSelected)
{
   if (!trackIsSelected)
      return { 0, 0 };
   const double t0 = selectedRegion.t0(); // left selection bound
   const double t1 = selectedRegion.t1(); // right selection bound
   const auto startTime = clip.GetPlayStartTime();
   const auto s0 = std::max(sampleCount(0), clip.TimeToSamples(t0 - startTime));
   auto s1 = std::clamp(
      clip.TimeToSamples(t1 - startTime), sampleCount { 0 },
      clip.GetVisibleSampleCount());
   return { s0, s1 };
}

void DrawClipSpectrum(TrackPanelDrawingContext &context,
   const WaveChannel &channel,
   const WaveChannelInterval &clip, const wxRect &rect,
   const std::shared_ptr<SpectralData> &mpSpectralData,
   bool selected)
{
  auto &dc = context.dc;
  const auto artist = TrackArtist::Get(context);
  bool onBrushTool = artist->onBrushTool;
  const auto &selectedRegion = *artist->pSelectedRegion;
  const auto &zoomInfo = *artist->pZoomInfo;

#ifdef PROFILE_WAVEFORM
   Profiler profiler;
#endif

   //If clip is "too small" draw a placeholder instead of
   //attempting to fit the contents into a few pixels
   if (!WaveChannelView::ClipDetailsVisible(clip, zoomInfo, rect))
   {
      auto clipRect = ClipParameters::GetClipRect(clip, zoomInfo, rect);
      TrackArt::DrawClipFolded(dc, clipRect);
      return;
   }

   auto &settings = SpectrogramSettings::Get(channel);
   const bool autocorrelation = (settings.algorithm == SpectrogramSettings::algPitchEAC);

   enum { DASH_LENGTH = 10 /* pixels */ };

   const ClipParameters params { clip, rect, zoomInfo };
   const wxRect &hiddenMid = params.hiddenMid;
   // The "hiddenMid" rect contains the part of the display actually
   // containing the waveform, as it appears without the fisheye.  If it's empty, we're done.
   if (hiddenMid.width <= 0) {
      return;
   }

   const double &t0 = params.t0;
   const double playStartTime = clip.GetPlayStartTime();

   const auto [ssel0, ssel1] = GetSelectedSampleIndices(selectedRegion, clip,
      channel.GetTrack().GetSelected());
   const double &averagePixelsPerSecond = params.averagePixelsPerSecond;
   const double sampleRate = clip.GetRate();
   const double stretchRatio = clip.GetStretchRatio();
   const double &hiddenLeftOffset = params.hiddenLeftOffset;
   const double &leftOffset = params.leftOffset;
   const wxRect &mid = params.mid;

   double freqLo = SelectedRegion::UndefinedFrequency;
   double freqHi = SelectedRegion::UndefinedFrequency;
   freqLo = selectedRegion.f0();
   freqHi = selectedRegion.f1();

   const int &colorScheme = settings.colorScheme;
   const int &range = settings.range;
   const int &gain = settings.gain;

#ifdef EXPERIMENTAL_FIND_NOTES
   const bool &fftFindNotes = settings.fftFindNotes;
   const double &findNotesMinA = settings.findNotesMinA;
   const int &numberOfMaxima = settings.numberOfMaxima;
   const bool &findNotesQuantize = settings.findNotesQuantize;
#endif
#ifdef EXPERIMENTAL_FFT_Y_GRID
   const bool &fftYGrid = settings.fftYGrid;
#endif

   dc.SetPen(*wxTRANSPARENT_PEN);

   // We draw directly to a bit image in memory,
   // and then paint this directly to our offscreen
   // bitmap.  Note that this could be optimized even
   // more, but for now this is not bad.  -dmazzoni
   wxImage image((int)mid.width, (int)mid.height);
   if (!image.IsOk())
      return;
#ifdef EXPERIMENTAL_SPECTROGRAM_OVERLAY
   image.SetAlpha();
   unsigned char *alpha = image.GetAlpha();
#endif
   unsigned char *data = image.GetData();

   const auto half = settings.GetFFTLength() / 2;
   const double binUnit = sampleRate / (2 * half);
   const float *freq = 0;
   const sampleCount *where = 0;
   bool updated = WaveClipSpectrumCache::Get(clip).GetSpectrogram(
      clip, freq, settings, where, (size_t)hiddenMid.width, t0,
      averagePixelsPerSecond);
   auto nBins = settings.NBins();

   float minFreq, maxFreq;
   SpectrogramBounds::Get(channel).GetBounds(channel, minFreq, maxFreq);

   const SpectrogramSettings::ScaleType scaleType = settings.scaleType;

   // nearest frequency to each pixel row from number scale, for selecting
   // the desired fft bin(s) for display on that row
   float *bins = (float*)alloca(sizeof(*bins)*(hiddenMid.height + 1));
   {
      const NumberScale numberScale( settings.GetScale( minFreq, maxFreq ) );

      NumberScale::Iterator it = numberScale.begin(mid.height);
      float nextBin = std::max( 0.0f, std::min( float(nBins - 1),
         settings.findBin( *it, binUnit ) ) );

      int yy;
      for (yy = 0; yy < hiddenMid.height; ++yy) {
         bins[yy] = nextBin;
         nextBin = std::max( 0.0f, std::min( float(nBins - 1),
            settings.findBin( *++it, binUnit ) ) );
      }
      bins[yy] = nextBin;
   }

#ifdef EXPERIMENTAL_FFT_Y_GRID
   const float
      log2 = logf(2.0f),
      scale2 = (lmax - lmin) / log2,
      lmin2 = lmin / log2;

   ArrayOf<bool> yGrid{size_t(mid.height)};
   for (int yy = 0; yy < mid.height; ++yy) {
      float n = (float(yy) / mid.height*scale2 - lmin2) * 12;
      float n2 = (float(yy + 1) / mid.height*scale2 - lmin2) * 12;
      float f = float(minFreq) / (fftSkipPoints + 1)*powf(2.0f, n / 12.0f + lmin2);
      float f2 = float(minFreq) / (fftSkipPoints + 1)*powf(2.0f, n2 / 12.0f + lmin2);
      n = logf(f / 440) / log2 * 12;
      n2 = logf(f2 / 440) / log2 * 12;
      if (floor(n) < floor(n2))
         yGrid[yy] = true;
      else
         yGrid[yy] = false;
   }
#endif //EXPERIMENTAL_FFT_Y_GRID

   auto &clipCache = WaveClipSpectrumCache::Get(clip);
   auto &specPxCache = clipCache.mSpecPxCaches[clip.GetChannelIndex()];
   if (!updated && specPxCache &&
      ((int)specPxCache->len == hiddenMid.height * hiddenMid.width)
      && scaleType == specPxCache->scaleType
      && gain == specPxCache->gain
      && range == specPxCache->range
      && minFreq == specPxCache->minFreq
      && maxFreq == specPxCache->maxFreq
#ifdef EXPERIMENTAL_FFT_Y_GRID
   && fftYGrid==fftYGridOld
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
   }
   else {
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
      float log2 = logf( 2.0f ),
         lmin = logf( minFreq ), lmax = logf( maxFreq ), scale = lmax - lmin,
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
            for (int i = maxTableSize - 1; i >= 0; i--)
               indexes[i] = -1;

            // Build a table of (most) values, put the index in it.
            for (int i = (int)(i0); i < (int)(i1); i++) {
               float freqi = freq[x0 + (int)(i)];
               int value = (int)((freqi + gain + range) / range*(maxTableSize - 1));
               if (value < 0)
                  value = 0;
               if (value >= maxTableSize)
                  value = maxTableSize - 1;
               indexes[value] = i;
            }
            // Build from the indices an array of maxima.
            for (int i = maxTableSize - 1; i >= 0; i--) {
               int index = indexes[i];
               if (index >= 0) {
                  float freqi = freq[x0 + index];
                  if (freqi < findNotesMinA)
                     break;

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
                     if (maximas >= numberOfMaxima)
                        break;
                  }
               }
            }

// The f2pix helper macro converts a frequency into a pixel coordinate.
#define f2pix(f) (logf(f)-lmins)/(lmaxs-lmins)*hiddenMid.height

            // Possibly quantize the maxima frequencies and create the pixel block limits.
            for (int i = 0; i < maximas; i++) {
               int index = maxima[i];
               float f = float(index)*bin2f;
               if (findNotesQuantize)
               {
                  f = expf((int)(log(f / 440) / log2 * 12 - 0.5) / 12.0f*log2) * 440;
                  maxima[i] = f*f2bin;
               }
               float f0 = expf((log(f / 440) / log2 * 24 - 1) / 24.0f*log2) * 440;
               maxima0[i] = f2pix(f0);
               float f1 = expf((log(f / 440) / log2 * 24 + 1) / 24.0f*log2) * 440;
               maxima1[i] = f2pix(f1);
            }
         }

         int it = 0;
         bool inMaximum = false;
#endif //EXPERIMENTAL_FIND_NOTES

         for (int yy = 0; yy < hiddenMid.height; ++yy) {
            const float bin     = bins[yy];
            const float nextBin = bins[yy+1];

            if (settings.scaleType != SpectrogramSettings::stLogarithmic) {
               const float value = findValue
                  (freq + nBins * xx, bin, nextBin, nBins, autocorrelation, gain, range);
               specPxCache->values[xx * hiddenMid.height + yy] = value;
            }
            else {
               float value;

#ifdef EXPERIMENTAL_FIND_NOTES
               if (fftFindNotes) {
                  if (it < maximas) {
                     float i0 = maxima0[it];
                     if (yy >= i0)
                        inMaximum = true;

                     if (inMaximum) {
                        float i1 = maxima1[it];
                        if (yy + 1 <= i1) {
                           value = findValue(freq + x0, bin, nextBin, nBins, autocorrelation, gain, range);
                           if (value < findNotesMinA)
                              value = minColor;
                        }
                        else {
                           it++;
                           inMaximum = false;
                           value = minColor;
                        }
                     }
                     else {
                        value = minColor;
                     }
                  }
                  else
                     value = minColor;
               }
               else
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

   float selBinLo = settings.findBin( freqLo, binUnit);
   float selBinHi = settings.findBin( freqHi, binUnit);
   float selBinCenter = (freqLo < 0 || freqHi < 0)
      ? -1
      : settings.findBin( sqrt(freqLo * freqHi), binUnit );

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
         specCache.where[ii - begin] =
            sampleCount(0.5 + sampleRate / stretchRatio * time);
      }
      specCache.Populate(
         settings, clip, 0, 0, numPixels,
         0 // FIXME: PRL -- make reassignment work with fisheye
      );
   }

   // build color gradient tables (not thread safe)
   if (!AColor::gradient_inited)
      AColor::PreComputeGradient();

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
   auto &dataHistory = mpSpectralData->dataHistory;

   // Lazy way to add all hops and bins required for rendering
   dataHistory.push_back(mpSpectralData->dataBuffer);

   // Generate combined hops and bins map for rendering
   std::map<long long, std::set<int>> hopBinMap;
   for(auto vecIter = dataHistory.begin(); vecIter != dataHistory.end(); ++vecIter){
      for(const auto &hopMap: *vecIter){
         for(const auto &binNum: hopMap.second)
            hopBinMap[hopMap.first].insert(binNum);
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
      }
      else {
          int specIndex = (xx - fisheyeLeft) * nBins;
          wxASSERT(specIndex >= 0 && specIndex < (int)specCache.freq.size());
          uncached = &specCache.freq[specIndex];
      }

      // zoomInfo must be queried for each column since with fisheye enabled
      // time between columns is variable
      const auto w0 = sampleCount(
         0.5 + sampleRate / stretchRatio *
                  (zoomInfo.PositionToTime(xx, -leftOffset) - playStartTime));

      const auto w1 = sampleCount(
         0.5 + sampleRate / stretchRatio *
                  (zoomInfo.PositionToTime(xx + 1, -leftOffset) - playStartTime));

      bool maybeSelected = ssel0 <= w0 && w1 < ssel1;
      maybeSelected = maybeSelected || (xx == selectedX);

      // In case the xx matches the hop number, it will be used as iterator for frequency bins
      std::set<int> *pSelectedBins = nullptr;
      std::set<int>::iterator freqBinIter;
      auto advanceFreqBinIter = [&](int nextBinRounded){
         while (freqBinIter != pSelectedBins->end() &&
         *freqBinIter < nextBinRounded)
            ++freqBinIter;
      };

      bool hitHopNum = false;
      if (onBrushTool) {
         int convertedHopNum = (w0.as_long_long() + hopSize / 2) / hopSize;
         hitHopNum = (hopBinMap.find(convertedHopNum) != hopBinMap.end());
         if(hitHopNum) {
            pSelectedBins = &hopBinMap[convertedHopNum];
            freqBinIter = pSelectedBins->begin();
            advanceFreqBinIter(yyToFreqBin(0));
         }
      }

      for (int yy = 0; yy < hiddenMid.height; ++yy) {
         if(onBrushTool)
            maybeSelected = false;
         const float bin     = bins[yy];
         const float nextBin = bins[yy+1];
         auto binRounded = yyToFreqBin(yy);
         auto nextBinRounded = yyToFreqBin(yy + 1);

         if(hitHopNum
            && freqBinIter != pSelectedBins->end()
            && binRounded == *freqBinIter)
            maybeSelected = true;

         if (hitHopNum)
            advanceFreqBinIter(nextBinRounded);

         // For spectral selection, determine what colour
         // set to use.  We use a darker selection if
         // in both spectral range and time range.

         AColor::ColorGradientChoice selected = AColor::ColorGradientUnselected;

         // If we are in the time selected range, then we may use a different color set.
         if (maybeSelected) {
            selected =
               ChooseColorSet(bin, nextBin, selBinLo, selBinCenter, selBinHi,
                  (xx + leftOffset - hiddenLeftOffset) / DASH_LENGTH, isSpectral);
            if ( onBrushTool && selected != AColor::ColorGradientUnselected )
               // use only two sets of colors
               selected = AColor::ColorGradientTimeAndFrequencySelected;
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
         alpha[px]= wxMin( 200, (value+0.3) * 500) ;
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

void SpectrumView::DoDraw(TrackPanelDrawingContext& context,
   const WaveChannel &channel, const WaveTrack::Interval* selectedClip,
   const wxRect & rect)
{
   const auto artist = TrackArtist::Get( context );
   const auto &blankSelectedBrush = artist->blankSelectedBrush;
   const auto &blankBrush = artist->blankBrush;
   TrackArt::DrawBackgroundWithSelection(
      context, rect, channel, blankSelectedBrush, blankBrush );

   for (const auto &pInterval : channel.Intervals()) {
      bool selected = selectedClip &&
         selectedClip == &pInterval->GetClip();
      DrawClipSpectrum(context, channel, *pInterval, rect, mpSpectralData,
         selected);
   }

   DrawBoldBoundaries(context, channel, rect);
}

void SpectrumView::Draw(
   TrackPanelDrawingContext &context, const wxRect &rect, unsigned iPass )
{
   if ( iPass == TrackArtist::PassTracks ) {
      const auto artist = TrackArtist::Get(context);
      const auto &pendingTracks = *artist->pPendingTracks;

      auto &dc = context.dc;

      const auto pChannel = FindChannel();
      if (!pChannel)
         return;
      const auto &wt = static_cast<const WaveChannel&>(
         pendingTracks.SubstitutePendingChangedChannel(*pChannel));

#if defined(__WXMAC__)
      wxAntialiasMode aamode = dc.GetGraphicsContext()->GetAntialiasMode();
      dc.GetGraphicsContext()->SetAntialiasMode(wxANTIALIAS_NONE);
#endif

      auto waveChannelView = GetWaveChannelView().lock();
      wxASSERT(waveChannelView.use_count());

      auto selectedClip = waveChannelView->GetSelectedClip();
      DoDraw(context, wt, selectedClip.get(), rect);

#if defined(__WXMAC__)
      dc.GetGraphicsContext()->SetAntialiasMode(aamode);
#endif
   }
   WaveChannelSubView::Draw(context, rect, iPass);
}

static const WaveChannelSubViews::RegisteredFactory key{
   [](WaveChannelView &view){
      return std::make_shared<SpectrumView>(view);
   }
};

// The following attaches the spectrogram settings item to the wave track popup
// menu.  It is appropriate only to spectrum view and so is kept in this
// source file with the rest of the spectrum view implementation.
#include "WaveTrackControls.h"
#include "AudioIOBase.h"
#include "../../../../MenuCreator.h"
#include "ProjectHistory.h"
#include "../../../../RefreshCode.h"
#include "../../../../prefs/PrefsDialog.h"
#include "../../../../prefs/SpectrumPrefs.h"
#include "AudacityMessageBox.h"
#include "../../../../widgets/PopupMenuTable.h"

namespace {
struct SpectrogramSettingsHandler : PopupMenuHandler {

   PlayableTrackControls::InitMenuData *mpData{};
   static SpectrogramSettingsHandler &Instance()
   {
      static SpectrogramSettingsHandler instance;
      return instance;
   }

   void OnSpectrogramSettings(wxCommandEvent &);

   void InitUserData(void *pUserData) override
   {
      mpData = static_cast< PlayableTrackControls::InitMenuData* >(pUserData);
   }
};

void SpectrogramSettingsHandler::OnSpectrogramSettings(wxCommandEvent &)
{
   class ViewSettingsDialog final : public PrefsDialog
   {
   public:
      ViewSettingsDialog(wxWindow *parent, AudacityProject &project,
         const TranslatableString &title, PrefsPanel::Factories &factories,
         int page)
         : PrefsDialog(parent, &project, title, factories)
         , mPage(page)
      {
      }

      long GetPreferredPage() override
      {
         return mPage;
      }

      void SavePreferredPage() override
      {
      }

   private:
      const int mPage;
   };

   auto gAudioIO = AudioIOBase::Get();
   if (gAudioIO->IsBusy()){
      AudacityMessageBox(
         XO(
"To change Spectrogram Settings, stop any\n playing or recording first."),
         XO("Stop the Audio First"),
         wxOK | wxICON_EXCLAMATION | wxCENTRE);
      return;
   }

   auto &wc = **static_cast<WaveTrack&>(mpData->track).Channels().begin();

   PrefsPanel::Factories factories;
   // factories.push_back(WaveformPrefsFactory(&track));
   factories.push_back(SpectrumPrefsFactory(&wc));
   const int page =
      // (pTrack->GetDisplay() == WaveChannelViewConstants::Spectrum) ? 1 :
      0;

   auto title = XO("%s:").Format(wc.GetTrack().GetName());
   ViewSettingsDialog dialog(
      mpData->pParent, mpData->project, title, factories, page);

   if (0 != dialog.ShowModal()) {
      // Redraw
      AudacityProject *const project = &mpData->project;
      ProjectHistory::Get( *project ).ModifyState(true);
      //Bug 1725 Toolbar was left greyed out.
      //This solution is overkill, but does fix the problem and is what the
      //prefs dialog normally does.
      MenuCreator::RebuildAllMenuBars();
      mpData->result = RefreshCode::RefreshAll;
   }
}

PopupMenuTable::AttachedItem sAttachment{
   GetWaveTrackMenuTable(),
   { "SubViews/Extra" },
   std::make_unique<PopupMenuSection>( "SpectrogramSettings",
      // Conditionally add menu item for settings, if showing spectrum
      PopupMenuTable::Adapt< WaveTrackPopupMenuTable >(
         [](WaveTrackPopupMenuTable &table)
         {
            using Entry = PopupMenuTable::Entry;
            static const int OnSpectrogramSettingsID =
            GetWaveTrackMenuTable().ReserveId();

            const auto pTrack = &table.FindWaveTrack();
            const auto &view = WaveChannelView::GetFirst(*pTrack);
            const auto displays = view.GetDisplays();
            bool hasSpectrum = (displays.end() != std::find(
               displays.begin(), displays.end(),
               WaveChannelSubView::Type{
                  WaveChannelViewConstants::Spectrum, {} }
            ) );
            return hasSpectrum
               // In future, we might move this to the context menu of the
               // Spectrum vertical ruler.
               // (But the latter won't be satisfactory without a means to
               // open that other context menu with keystrokes only, and that
               // would require some notion of a focused sub-view.)
               ? std::make_unique<Entry>("SpectrogramSettings",
                  Entry::Item,
                  OnSpectrogramSettingsID,
                  XXO("S&pectrogram Settings..."),
                  (wxCommandEventFunction)
                     (&SpectrogramSettingsHandler::OnSpectrogramSettings),
                  SpectrogramSettingsHandler::Instance(),
                  []( PopupMenuHandler &handler, wxMenu &menu, int id ){
                     // Bug 1253.  Shouldn't open preferences if audio is busy.
                     // We can't change them on the fly yet anyway.
                     auto gAudioIO = AudioIOBase::Get();
                     menu.Enable(id, !gAudioIO->IsBusy());
                  } )
               : nullptr;
         } ) )
};
}

static bool ShouldCaptureEvent(wxKeyEvent& event, SpectralData *pData)
{
   const auto keyCode = event.GetKeyCode();
   return
      (keyCode == WXK_BACK || keyCode == WXK_DELETE ||
       keyCode == WXK_NUMPAD_DELETE)
      && pData && !pData->dataHistory.empty();
}

unsigned SpectrumView::CaptureKey(
   wxKeyEvent& event, ViewInfo&, wxWindow*, AudacityProject*)
{
   bool capture = ShouldCaptureEvent(event, mpSpectralData.get());
   event.Skip(!capture);
   return RefreshCode::RefreshNone;
}

unsigned SpectrumView::KeyDown(wxKeyEvent& event, ViewInfo& viewInfo, wxWindow*, AudacityProject* project)
{
   bool capture = ShouldCaptureEvent(event, mpSpectralData.get());
   event.Skip(!capture);
   if (capture && SpectralDataManager::ProcessTracks(*project))
      // Not RefreshCell, because there might be effects in multiple tracks
      return RefreshCode::RefreshAll;
   return RefreshCode::RefreshNone;
}

unsigned SpectrumView::Char(
   wxKeyEvent &event, ViewInfo&, wxWindow*, AudacityProject* )
{
   bool capture = ShouldCaptureEvent(event, mpSpectralData.get());
   event.Skip(!capture);
   return RefreshCode::RefreshNone;
}

// Attach some related menu items
#include "../../../ui/SelectHandle.h"
#include "../../../../CommonCommandFlags.h"
#include "Project.h"
#include "SpectrumAnalyst.h"
#include "CommandContext.h"

namespace {
void DoNextPeakFrequency(AudacityProject &project, bool up)
{
   // This only ever considered the left member of a stereo pair!
   // TODO:  account for the right hand channel too.
   // (How?  Average corresponding bin power?)

   auto &tracks = TrackList::Get( project );
   auto &viewInfo = ViewInfo::Get( project );

   // Find the first selected wave track that is in a spectrogram view.
   const auto hasSpectrum = [](const WaveTrack *wt){
      const auto displays = WaveChannelView::GetFirst(*wt).GetDisplays();
      return displays.end() != std::find(
         displays.begin(), displays.end(),
         WaveChannelSubView::Type{ WaveChannelViewConstants::Spectrum, {} });
   };
   const auto range = tracks.Selected<const WaveTrack>();
   const auto iter = find_if(begin(range), end(range), hasSpectrum);
   if (iter != end(range)) {
      SpectrumAnalyst analyst;
      auto &wt = **iter;
      SelectHandle::SnapCenterOnce(analyst,
         viewInfo, **wt.Channels().first, up);
      ProjectHistory::Get( project ).ModifyState(false);
   }
}

struct Handler : CommandHandlerObject, ClientData::Base {

// Handler state:
double mLastF0{ SelectedRegion::UndefinedFrequency };
double mLastF1{ SelectedRegion::UndefinedFrequency };

void OnToggleSpectralSelection(const CommandContext &context)
{
   auto &project = context.project;
   auto &selectedRegion = ViewInfo::Get( project ).selectedRegion;

   const double f0 = selectedRegion.f0();
   const double f1 = selectedRegion.f1();
   const bool haveSpectralSelection =
   !(f0 == SelectedRegion::UndefinedFrequency &&
     f1 == SelectedRegion::UndefinedFrequency);
   if (haveSpectralSelection)
   {
      mLastF0 = f0;
      mLastF1 = f1;
      selectedRegion.setFrequencies
      (SelectedRegion::UndefinedFrequency, SelectedRegion::UndefinedFrequency);
   }
   else
      selectedRegion.setFrequencies(mLastF0, mLastF1);

   ProjectHistory::Get( project ).ModifyState(false);
}

void OnNextHigherPeakFrequency(const CommandContext &context)
{
   auto &project = context.project;
   DoNextPeakFrequency(project, true);
}

void OnNextLowerPeakFrequency(const CommandContext &context)
{
   auto &project = context.project;
   DoNextPeakFrequency(project, false);
}
};

// Handler is stateful.  Needs a factory registered with
// AudacityProject.
static const AttachedProjectObjects::RegisteredFactory key{
   [](AudacityProject&) {
      return std::make_unique< Handler >(); } };

static CommandHandlerObject &findCommandHandler(AudacityProject &project) {
   return project.AttachedObjects::Get< Handler >( key );
};

using namespace MenuRegistry;
#define FN(X) (& Handler :: X)

auto SpectralSelectionMenu()
{
   static auto menu = std::shared_ptr{
   ( FinderScope{ findCommandHandler },
   Menu( wxT("Spectral"), XXO("S&pectral"),
      Command( wxT("ToggleSpectralSelection"),
         XXO("To&ggle Spectral Selection"), FN(OnToggleSpectralSelection),
         TracksExistFlag(), wxT("Q") ),
      Command( wxT("NextHigherPeakFrequency"),
         XXO("Next &Higher Peak Frequency"), FN(OnNextHigherPeakFrequency),
         TracksExistFlag() ),
      Command( wxT("NextLowerPeakFrequency"),
         XXO("Next &Lower Peak Frequency"), FN(OnNextLowerPeakFrequency),
         TracksExistFlag() )
   ) ) };
   return menu;
}

#undef FN

AttachedItem sAttachment2{ Indirect(SpectralSelectionMenu()),
   Placement{ wxT("Select/Basic"), { OrderingHint::After, wxT("Region") } }
};

}
