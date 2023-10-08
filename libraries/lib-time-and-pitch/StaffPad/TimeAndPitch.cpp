#include "TimeAndPitch.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <utility>

#include "CircularSampleBuffer.h"
#include "FourierTransform_pffft.h"
#include "SamplesFloat.h"
#include "SimdTypes.h"

using namespace staffpad::audio;

namespace staffpad {

namespace {
constexpr double twoPi = 6.28318530717958647692f;

/**
 * 6-point lagrange interpolator SNR ~ -35.2db
 * interpolates between samples 2 and 3 using t.
 *
 * smp              pointer to a memory region containing 6 samples
 * t                fractional sample to interpolate t >= 0 && t < 1
 * return value     the interpolated sample
 */
inline float lagrange6(const float (&smp)[6], float t)
{
  auto* y = &smp[2];

  auto ym1_y1 = y[-1] + y[1];
  auto twentyfourth_ym2_y2 = 1.f / 24.f * (y[-2] + y[2]);
  auto c0 = y[0];
  auto c1 = 0.05f * y[-2] - 0.5f * y[-1] - 1.f / 3.f * y[0] + y[1] - 0.25f * y[2] + 1.f / 30.f * y[3];
  auto c2 = 2.f / 3.f * ym1_y1 - 1.25f * y[0] - twentyfourth_ym2_y2;
  auto c3 = 5.f / 12.f * y[0] - 7.f / 12.f * y[1] + 7.f / 24.f * y[2] - 1.f / 24.f * (y[-2] + y[-1] + y[3]);
  auto c4 = 0.25f * y[0] - 1.f / 6.f * ym1_y1 + twentyfourth_ym2_y2;
  auto c5 = 1.f / 120.f * (y[3] - y[-2]) + 1.f / 24.f * (y[-1] - y[2]) + 1.f / 12.f * (y[1] - y[0]);

  // Estrin's scheme
  auto t2 = t * t;
  return (c0 + c1 * t) + (c2 + c3 * t) * t2 + (c4 + c5 * t) * t2 * t2;
}

inline int getFftSize(int sampleRate)
{
  // 44.1kHz maps to 4096 samples (i.e., 93ms).
  // We grow the FFT size proportionally with the sample rate to keep the
  // window duration roughly constant, with quantization due to the
  // power-of-two constraint.
  // If needed some time in the future, we can decouple analysis window and
  // FFT sizes by zero-padding, allowing for very fine-grained window duration
  // without compromising performance.
  return 1 << (12 + (int)std::round(std::log2(sampleRate / 44100.)));
}
} // namespace

struct TimeAndPitch::impl
{
  impl(int fft_size) : fft(fft_size)
  {
  }

  FourierTransform fft;
  CircularSampleBuffer<float> inResampleInputBuffer[2];
  CircularSampleBuffer<float> inCircularBuffer[2];
  CircularSampleBuffer<float> outCircularBuffer[2];
  CircularSampleBuffer<float> normalizationBuffer;

  SamplesReal fft_timeseries;
  SamplesComplex spectrum;

  static constexpr size_t historyLength = 2;
  SamplesReal norm[historyLength];
  SamplesReal phase[historyLength];
  
  SamplesReal phase_accum;
  SamplesReal cosWindow;
  SamplesReal sqWindow;

  double exact_hop_a = 512.0, hop_a_err = 0.0;
  double exact_hop_s = 0.0;
  double next_exact_hop_s = 512.0;
  double hop_s_err = 0.0;

  std::vector<int> peak_index, trough_index;
  std::uint8_t index = 0;
};

TimeAndPitch::TimeAndPitch(int sampleRate)
    : fftSize(getFftSize(sampleRate))
{
}

TimeAndPitch::~TimeAndPitch()
{
  // Here for ~std::shared_ptr<impl>() to know ~impl()
}

void TimeAndPitch::setup(int numChannels, int maxBlockSize)
{
  assert(numChannels == 1 || numChannels == 2);
  _numChannels = numChannels;

  d = std::make_unique<impl>(fftSize);
  _maxBlockSize = maxBlockSize;
  _numBins = fftSize / 2 + 1;

  // audio sample buffers
  d->fft_timeseries.setSize(_numChannels, fftSize);
  int outBufferSize = fftSize + 2 * _maxBlockSize; // 1 block extra for safety
  for (int ch = 0; ch < _numChannels; ++ch)
  {
    d->inResampleInputBuffer[ch].setSize(_maxBlockSize +
                                         6); // needs additional history for resampling. more in the future
    d->inCircularBuffer[ch].setSize(fftSize);
    d->outCircularBuffer[ch].setSize(outBufferSize);
  }
  d->normalizationBuffer.setSize(outBufferSize);

  // fft coefficient buffers
  d->spectrum.setSize(_numChannels, _numBins);
  d->index = 0;
  for (size_t ii = 0; ii < d->historyLength; ++ii)
  {
    d->norm[ii].setSize(1, _numBins);
    d->phase[ii].setSize(_numChannels, _numBins);
  }
  d->phase_accum.setSize(_numChannels, _numBins);

  _expectedPhaseChangePerBinPerSample = twoPi / double(fftSize);

  // Cos window
  // For perfect int-factor overlaps the first sample needs to be 0 and the last non-zero.
  d->cosWindow.setSize(1, fftSize);
  d->sqWindow.setSize(1, fftSize);
  auto* w = d->cosWindow.getPtr(0);
  auto* sqw = d->sqWindow.getPtr(0);
  for (int i = 0; i < fftSize; ++i)
  {
    w[i] = -0.5f * std::cos(float(twoPi * (float)i / (float)fftSize)) + 0.5f;
    sqw[i] = w[i] * w[i];
  }

  d->peak_index.reserve(_numBins);
  d->trough_index.reserve(_numBins);

  // force fft to allocate all buffers now. Currently there is no other way in the fft class
  d->fft.forwardReal(d->fft_timeseries, d->spectrum);

  reset();
}

int TimeAndPitch::getSamplesToNextHop() const
{
  return std::max(0, int(std::ceil(d->exact_hop_a)) - _analysis_hop_counter +
                         1); // 1 extra to counter pitch factor error accumulation
}

int TimeAndPitch::getNumAvailableOutputSamples() const
{
  return _availableOutputSamples;
}

void TimeAndPitch::reset()
{
  _analysis_hop_counter = 0;
  _availableOutputSamples = 0;
  for (int ch = 0; ch < _numChannels; ++ch)
  {
    d->inResampleInputBuffer[ch].reset();
    d->inCircularBuffer[ch].reset();
    d->outCircularBuffer[ch].reset();
  }
  d->normalizationBuffer.reset();
  for (size_t ii = 0; ii < d->historyLength; ++ii)
  {
    d->norm[ii].zeroOut();
    d->phase[ii].zeroOut();
  }
  d->phase_accum.zeroOut();
  _outBufferWriteOffset = 0;
  d->hop_a_err = 0.0;
  d->hop_s_err = 0.0;
  d->exact_hop_s = 0.0;
  _resampleReadPos = 0.0;
}

namespace {

// wrap a phase value into -PI..PI
inline float _unwrapPhase(float arg)
{
  using namespace audio::simd;
  return arg - rint(arg * 0.15915494309f) * 6.283185307f;
}

void _unwrapPhaseVec(float* v, int n)
{
  audio::simd::perform_parallel_simd_aligned(v, n, [](auto& a) { a = a - rint(a * 0.15915494309f) * 6.283185307f; });
}

/// rotate even-sized array by half its size to align fft phase at the center
void _fft_shift(float* v, int n)
{
  assert((n & 1) == 0);
  int n2 = n >> 1;
  audio::simd::perform_parallel_simd_aligned(v, v + n2, n2, [](auto& a, auto& b) {
    auto tmp = a;
    a = b;
    b = tmp;
  });
}

void _lr_to_ms(float* ch1, float* ch2, int n)
{
  audio::simd::perform_parallel_simd_aligned(ch1, ch2, n, [](auto& a, auto& b) {
    auto l = a, r = b;
    a = 0.5f * (l + r);
    b = 0.5f * (l - r);
  });
}

void _ms_to_lr(float* ch1, float* ch2, int n)
{
  audio::simd::perform_parallel_simd_aligned(ch1, ch2, n, [](auto& a, auto& b) {
    auto m = a, s = b;
    a = m + s;
    b = m - s;
  });
}

} // namespace

// ----------------------------------------------------------------------------

template <int num_channels>
void TimeAndPitch::_time_stretch(float a_a, float a_s)
{
  auto alpha = a_s / a_a; // this is the real stretch factor based on integer hop sizes

  // Create a norm array
  const auto index = d->index;
  const auto last_index = (d->index + d->historyLength - 1) % d->historyLength;
  auto* norms = d->norm[index].getPtr(0); // for stereo, just use the mid-channel
  const auto* norms_last = d->norm[last_index].getPtr(0);

  d->peak_index.clear();
  d->trough_index.clear();
  float lowest = norms[0];
  int trough = 0;
  if (norms_last[0] >= norms[1])
  {
    d->peak_index.emplace_back(0);
    d->trough_index.emplace_back(0);
  }
  for (int i = 1; i < _numBins - 1; ++i)
  {
    if (norms_last[i] >= norms[i - 1] && norms_last[i] >= norms[i + 1])
    {
      d->peak_index.emplace_back(i);
      d->trough_index.emplace_back(trough);
      trough = i;
      lowest = norms[i];
    }
    else if (norms[i] < lowest)
    {
      lowest = norms[i];
      trough = i;
    }
  }
  if (norms_last[_numBins - 1] > norms[_numBins - 2])
  {
    d->peak_index.emplace_back(_numBins - 1);
    d->trough_index.emplace_back(trough);
  }

  if (d->peak_index.size() == 0)
  {
    // use max norm of last frame
    int max_idx = 0;
    float max_norm = 0.f;
    vo::findMaxElement(norms_last, _numBins, max_idx, max_norm);
    d->peak_index.emplace_back(max_idx);
  }

  const float** p = const_cast<const float**>(d->phase[index].getPtrs());
  const float** p_l = const_cast<const float**>(d->phase[last_index].getPtrs());
  float** acc = d->phase_accum.getPtrs();

  float expChange_a = a_a * float(_expectedPhaseChangePerBinPerSample);
  float expChange_s = a_s * float(_expectedPhaseChangePerBinPerSample);

  int num_peaks = (int)d->peak_index.size();
  // time integration for all peaks
  for (int i = 0; i < num_peaks; ++i)
  {
    const int n = d->peak_index[i];
    auto fn = float(n);

    // determine phase from last frame
    float fn_expChange_a = fn * expChange_a;
    float fn_expChange_s = fn * expChange_s;

    for (int ch = 0; ch < num_channels; ++ch)
      acc[ch][n] = acc[ch][n] + alpha * _unwrapPhase(p[ch][n] - p_l[ch][n] - fn_expChange_a) + fn_expChange_s;
  }

  // go from first peak to 0
  for (int n = d->peak_index[0]; n > 0; --n)
  {
    for (int ch = 0; ch < num_channels; ++ch)
      acc[ch][n - 1] = acc[ch][n] - alpha * _unwrapPhase(p[ch][n] - p[ch][n - 1]);
  }

  // 'grow' from pairs of peaks to the lowest norm in between
  for (int i = 0; i < num_peaks - 1; ++i)
  {
    const int mid = d->trough_index[i + 1];
    for (int n = d->peak_index[i]; n < mid; ++n)
    {
      for (int ch = 0; ch < num_channels; ++ch)
        acc[ch][n + 1] = acc[ch][n] + alpha * _unwrapPhase(p[ch][n + 1] - p[ch][n]);
    }
    for (int n = d->peak_index[i + 1]; n > mid + 1; --n)
    {
      for (int ch = 0; ch < num_channels; ++ch)
        acc[ch][n - 1] = acc[ch][n] - alpha * _unwrapPhase(p[ch][n] - p[ch][n - 1]);
    }
  }

  // last peak to the end
  for (int n = d->peak_index[num_peaks - 1]; n < _numBins - 1; ++n)
  {
    for (int ch = 0; ch < num_channels; ++ch)
      acc[ch][n + 1] = acc[ch][n] + alpha * _unwrapPhase(p[ch][n + 1] - p[ch][n]);
  }

  d->index = (d->index + 1) % d->historyLength;
}

/// process one hop/chunk in _fft_timeSeries and add the result to output circular buffer
void TimeAndPitch::_process_hop(int hop_a, int hop_s)
{
  if (d->exact_hop_a != d->exact_hop_s)
  {
    if (_numChannels == 2)
      _lr_to_ms(d->fft_timeseries.getPtr(0), d->fft_timeseries.getPtr(1), fftSize);

    for (int ch = 0; ch < _numChannels; ++ch)
    {
      vo::multiply(d->fft_timeseries.getPtr(ch), d->cosWindow.getPtr(0), d->fft_timeseries.getPtr(ch), fftSize);
      _fft_shift(d->fft_timeseries.getPtr(ch), fftSize);
    }

    // determine norm/phase
    d->fft.forwardReal(d->fft_timeseries, d->spectrum);
    // norms of the mid channel only (or sole channel) are needed in
    // _time_stretch
    const auto index = d->index;
    vo::calcNorms(d->spectrum.getPtr(0), d->norm[index].getPtr(0), d->spectrum.getNumSamples());
    for (int ch = 0; ch < _numChannels; ++ch)
      vo::calcPhases(d->spectrum.getPtr(ch), d->phase[index].getPtr(ch), d->spectrum.getNumSamples());

    if (_numChannels == 1)
      _time_stretch<1>((float)hop_a, (float)hop_s);
    else if (_numChannels == 2)
      _time_stretch<2>((float)hop_a, (float)hop_s);

    for (int ch = 0; ch < _numChannels; ++ch)
      _unwrapPhaseVec(d->phase_accum.getPtr(ch), _numBins);

    for (int ch = 0; ch < _numChannels; ++ch)
      vo::rotate(d->phase[index].getPtr(ch), d->phase_accum.getPtr(ch), d->spectrum.getPtr(ch),
                                  d->spectrum.getNumSamples());
    d->fft.inverseReal(d->spectrum, d->fft_timeseries);

    for (int ch = 0; ch < _numChannels; ++ch)
      vo::constantMultiply(d->fft_timeseries.getPtr(ch), 1.f / fftSize, d->fft_timeseries.getPtr(ch),
                           d->fft_timeseries.getNumSamples());

    if (_numChannels == 2)
      _ms_to_lr(d->fft_timeseries.getPtr(0), d->fft_timeseries.getPtr(1), fftSize);

    for (int ch = 0; ch < _numChannels; ++ch)
    {
      _fft_shift(d->fft_timeseries.getPtr(ch), fftSize);
      vo::multiply(d->fft_timeseries.getPtr(ch), d->cosWindow.getPtr(0), d->fft_timeseries.getPtr(ch), fftSize);
    }
  }
  else
  { // stretch factor == 1.0 => just apply window
    for (int ch = 0; ch < _numChannels; ++ch)
      vo::multiply(d->fft_timeseries.getPtr(ch), d->sqWindow.getPtr(0), d->fft_timeseries.getPtr(ch), fftSize);
  }

  {
    float gainFact = float(_timeStretch * ((8.f / 3.f) / _overlap_a)); // overlap add normalization factor
    for (int ch = 0; ch < _numChannels; ++ch)
      d->outCircularBuffer[ch].writeAddBlockWithGain(_outBufferWriteOffset, fftSize, d->fft_timeseries.getPtr(ch),
                                                     gainFact);

    if (normalize_window)
      d->normalizationBuffer.writeAddBlockWithGain(_outBufferWriteOffset, fftSize, d->sqWindow.getPtr(0), gainFact);
  }
  _outBufferWriteOffset += hop_s;
  _availableOutputSamples += hop_s;
}

void TimeAndPitch::setTimeStretchAndPitchFactor(double timeScale, double pitchFactor)
{
  assert(timeScale > 0.0);
  assert(pitchFactor > 0.0);
  _pitchFactor = pitchFactor;
  _timeStretch = timeScale * pitchFactor;

  _overlap_a = overlap;
  double overlap_s = overlap;
  if (_timeStretch > 1.0 || !modulate_synthesis_hop)
    _overlap_a *= _timeStretch;
  else
    overlap_s /= _timeStretch;

  d->exact_hop_a = double(fftSize) / _overlap_a;
  if (!modulate_synthesis_hop)
  {
    d->exact_hop_s = double(fftSize) / overlap_s;
  }
  else
  {
    // switch after processing next block, unless it's the first one
    d->next_exact_hop_s = double(fftSize) / overlap_s;
    if (d->exact_hop_s == 0.0) // on first chunk set it immediately
      d->exact_hop_s = d->next_exact_hop_s;
  }
}

void TimeAndPitch::feedAudio(const float* const* input_smp, int numSamples)
{
  assert(numSamples <= _maxBlockSize);
  for (int ch = 0; ch < _numChannels; ++ch)
  {
    d->inResampleInputBuffer[ch].writeBlock(0, numSamples, input_smp[ch]);
    d->inResampleInputBuffer[ch].advance(numSamples);
  }
  _resampleReadPos -= numSamples;

  // determine integer hop size for the next hop.
  // The error accumulators will make the value fluctuate by one to reach arbitrary scale
  // ratios
  if (d->exact_hop_s == 0.0) // this happens if feedAudio is called without setting up stretch factors
    d->exact_hop_s = d->next_exact_hop_s;

  const int hop_s = int(d->exact_hop_s + d->hop_s_err);
  const int hop_a = int(d->exact_hop_a + d->hop_a_err);

  double step = 0.0;
  double read_pos = _resampleReadPos;
  while (read_pos < 0.0)
  {
    auto int_pos = int(std::floor(read_pos));
    float frac_pos = float(read_pos - int_pos);
    for (int ch = 0; ch < _numChannels; ++ch)
    {
      float smp[6];
      d->inResampleInputBuffer[ch].readBlock(int_pos - 6, 6, smp);
      float s = (frac_pos == 0) ? smp[2] : lagrange6(smp, frac_pos);
      d->inCircularBuffer[ch].writeOffset0(s);
      d->inCircularBuffer[ch].advance(1);
    }
    _analysis_hop_counter++;
    step++;

    if (_analysis_hop_counter >= hop_a)
    {
      _analysis_hop_counter -= hop_a;
      d->hop_s_err += d->exact_hop_s - hop_s;
      d->hop_a_err += d->exact_hop_a - hop_a;
      for (int ch = 0; ch < _numChannels; ++ch)
        d->inCircularBuffer[ch].readBlock(-fftSize, fftSize, d->fft_timeseries.getPtr(ch));
      _process_hop(hop_a, hop_s);
    }
    read_pos = _resampleReadPos + step * _pitchFactor;
  }
  _resampleReadPos = read_pos;
}

void TimeAndPitch::retrieveAudio(float* const* out_smp, int numSamples)
{
  assert(numSamples <= _maxBlockSize);
  for (int ch = 0; ch < _numChannels; ++ch)
  {
    d->outCircularBuffer[ch].readAndClearBlock(0, numSamples, out_smp[ch]);
    if (normalize_window)
    {
      constexpr float curve =
          4.f * 4.f; // the curve approximates 1/x over 1 but fades to 0 near 0 to avoid fade-in clicks
      for (int i = 0; i < numSamples; ++i)
      {
        float x = d->normalizationBuffer.read(i);
        out_smp[ch][i] *= x / (x * x + 1 / curve);
      }
    }
    d->outCircularBuffer[ch].advance(numSamples);
  }

  d->normalizationBuffer.clearBlock(0, numSamples);
  d->normalizationBuffer.advance(numSamples);

  _outBufferWriteOffset -= numSamples;
  _availableOutputSamples -= numSamples;

  if (modulate_synthesis_hop)
    d->exact_hop_s = d->next_exact_hop_s;
}

void TimeAndPitch::processPitchShift(float* const* smp, int numSamples, double pitchFactor)
{
  setTimeStretchAndPitchFactor(1.0, pitchFactor);
  feedAudio(smp, numSamples);
  retrieveAudio(smp, numSamples);
}

int TimeAndPitch::getLatencySamples() const
{
  return fftSize - fftSize / overlap + 3; // 3 for resampling
}


int TimeAndPitch::getLatencySamplesForStretchRatio(float timeStretch) const
{
  const float coeff = (timeStretch < 1.f) ? (1.f / 3.f) : (2.f / 3.f);
  return int(getLatencySamples() * (timeStretch * coeff + (1 - coeff)));
}

} // namespace staffpad
