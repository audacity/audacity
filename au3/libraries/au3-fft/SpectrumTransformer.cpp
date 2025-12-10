/**********************************************************************

Audacity: A Digital Audio Editor

SpectrumTransformer.cpp

Edward Hui

**********************************************************************/

#include "SpectrumTransformer.h"

#include <algorithm>
#include "FFT.h"

SpectrumTransformer::SpectrumTransformer(bool needsOutput,
                                         eWindowFunctions inWindowType,
                                         eWindowFunctions outWindowType,
                                         size_t windowSize, unsigned stepsPerWindow,
                                         bool leadingPadding, bool trailingPadding)
    : mWindowSize{windowSize}
    , mSpectrumSize{1 + mWindowSize / 2}
    , mStepsPerWindow{stepsPerWindow}
    , mStepSize{mWindowSize / mStepsPerWindow}
    , mLeadingPadding{leadingPadding}
    , mTrailingPadding{trailingPadding}
    , hFFT{GetFFT(mWindowSize)}
    , mFFTBuffer(mWindowSize)
    , mInWaveBuffer(mWindowSize)
    , mOutOverlapBuffer(mWindowSize)
    , mNeedsOutput{needsOutput}
{
    // Check preconditions

    // Powers of 2 only!
    wxASSERT(mWindowSize > 0
             && 0 == (mWindowSize & (mWindowSize - 1)));

    wxASSERT(mWindowSize % mStepsPerWindow == 0);

    wxASSERT(!(inWindowType == eWinFuncRectangular && outWindowType == eWinFuncRectangular));

    // To do:  check that inWindowType, outWindowType, and mStepsPerWindow
    // are compatible for correct overlap-add reconstruction.

    // Create windows as needed
    if (inWindowType != eWinFuncRectangular) {
        mInWindow.resize(mWindowSize);
        std::fill(mInWindow.begin(), mInWindow.end(), 1.0f);
        NewWindowFunc(inWindowType, mWindowSize, false, mInWindow.data());
    }
    if (outWindowType != eWinFuncRectangular) {
        mOutWindow.resize(mWindowSize);
        std::fill(mOutWindow.begin(), mOutWindow.end(), 1.0f);
        NewWindowFunc(outWindowType, mWindowSize, false, mOutWindow.data());
    }

    // Must scale one or the other window so overlap-add
    // comes out right
    double denom = 0;
    for (size_t ii = 0; ii < mWindowSize; ii += mStepSize) {
        denom
            +=(mInWindow.empty() ? 1.0 : mInWindow[ii])
               *
               (mOutWindow.empty() ? 1.0 : mOutWindow[ii]);
    }
    // It is ASSUMED that you have chosen window types and
    // steps per window, so that this sum denom would be the
    // same, starting the march anywhere from 0 to mStepSize - 1.
    // Else, your overlap-add won't be right, and the transformer
    // might not be an identity even when you do nothing to the
    // spectra.

    float* pWindow = 0;
    if (!mInWindow.empty()) {
        pWindow = mInWindow.data();
    } else if (!mOutWindow.empty()) {
        pWindow = mOutWindow.data();
    } else {
        // Can only happen if both window types were rectangular
        wxASSERT(false);
    }
    for (size_t ii = 0; ii < mWindowSize; ++ii) {
        *pWindow++ /= denom;
    }
}

auto SpectrumTransformer::NewWindow(size_t windowSize)
-> std::unique_ptr<Window>
{
    return std::make_unique<Window>(windowSize);
}

bool SpectrumTransformer::DoStart()
{
    return true;
}

bool SpectrumTransformer::DoFinish()
{
    return true;
}

bool SpectrumTransformer::Start(size_t queueLength)
{
    // Prepare clean queue
    ResizeQueue(queueLength);
    for (auto& pWindow : mQueue) {
        pWindow->Zero();
    }

    // invoke derived method
    if (!DoStart()) {
        return false;
    }

    // Clean input and output buffers
    {
        float* pFill;
        pFill = mInWaveBuffer.data();
        std::fill(pFill, pFill + mWindowSize, 0.0f);
        pFill = mOutOverlapBuffer.data();
        std::fill(pFill, pFill + mWindowSize, 0.0f);
    }

    if (!mLeadingPadding) {
        // We do not want leading zero padded windows
        mInWavePos = 0;
        mOutStepCount = -static_cast<int>(queueLength - 1);
    } else {
        // So that the queue gets primed with some windows,
        // zero-padded in front, the first having mStepSize
        // samples of wave data:
        mInWavePos = mWindowSize - mStepSize;
        // This starts negative, to count up until the queue fills:
        mOutStepCount = -static_cast<int>(queueLength - 1)
                        // ... and then must pass over the padded windows,
                        // before the first full window:
                        - static_cast<int>(mStepsPerWindow - 1);
    }

    mInSampleCount = 0;

    return true;
}

bool SpectrumTransformer::ProcessSamples(const WindowProcessor& processor,
                                         const float* buffer, size_t len)
{
    if (buffer) {
        mInSampleCount += len;
    }
    bool success = true;
    while (success && len
           && mOutStepCount * static_cast<int>(mStepSize) < mInSampleCount) {
        auto avail = std::min(len, mWindowSize - mInWavePos);
        if (buffer) {
            memmove(&mInWaveBuffer[mInWavePos], buffer, avail * sizeof(float));
        } else {
            memset(&mInWaveBuffer[mInWavePos], 0, avail * sizeof(float));
        }
        if (buffer) {
            buffer += avail;
        }
        len -= avail;
        mInWavePos += avail;

        if (mInWavePos == mWindowSize) {
            FillFirstWindow();

            // invoke derived method
            if ((success = processor(*this)), success) {
                OutputStep();
            }

            ++mOutStepCount;
            RotateWindows();

            // Shift input.
            memmove(mInWaveBuffer.data(), &mInWaveBuffer[mStepSize],
                    (mWindowSize - mStepSize) * sizeof(float));
            mInWavePos -= mStepSize;
        }
    }

    return success;
}

void SpectrumTransformer::ResizeQueue(size_t queueLength)
{
    int oldLen = mQueue.size();
    mQueue.resize(queueLength);
    for (size_t ii = oldLen; ii < queueLength; ++ii) {
        // invoke derived method to get a queue element
        // with appropriate extra fields
        mQueue[ii] = NewWindow(mWindowSize);
    }
}

void SpectrumTransformer::FillFirstWindow()
{
    // Transform samples to frequency domain, windowed as needed
    {
        auto pFFTBuffer = mFFTBuffer.data(), pInWaveBuffer = mInWaveBuffer.data();
        if (mInWindow.size() > 0) {
            auto pInWindow = mInWindow.data();
            for (size_t ii = 0; ii < mWindowSize; ++ii) {
                *pFFTBuffer++ = *pInWaveBuffer++ **pInWindow++;
            }
        } else {
            memmove(pFFTBuffer, pInWaveBuffer, mWindowSize * sizeof(float));
        }
    }
    RealFFTf(mFFTBuffer.data(), hFFT.get());

    auto& record = Nth(0);

    // Store real and imaginary parts for later inverse FFT
    {
        float* pReal = &record.mRealFFTs[1];
        float* pImag = &record.mImagFFTs[1];
        int* pBitReversed = &hFFT->BitReversed[1];
        const auto last = mSpectrumSize - 1;
        for (size_t ii = 1; ii < last; ++ii) {
            const int kk = *pBitReversed++;
            *pReal++ = mFFTBuffer[kk];
            *pImag++ = mFFTBuffer[kk + 1];
        }
        // DC and Fs/2 bins need to be handled specially
        const float dc = mFFTBuffer[0];
        record.mRealFFTs[0] = dc;

        const float nyquist = mFFTBuffer[1];
        record.mImagFFTs[0] = nyquist; // For Fs/2, not really imaginary
    }
}

void SpectrumTransformer::RotateWindows()
{
    std::rotate(mQueue.begin(), mQueue.end() - 1, mQueue.end());
}

bool SpectrumTransformer::Finish(const WindowProcessor& processor)
{
    bool bLoopSuccess = true;
    if (mTrailingPadding) {
        // Keep flushing empty input buffers through the history
        // windows until we've output exactly as many samples as
        // were input.
        // Well, not exactly, but not more than one step-size of extra samples
        // at the end.

        while (bLoopSuccess
               && mOutStepCount * static_cast<int>(mStepSize) < mInSampleCount) {
            bLoopSuccess = ProcessSamples(processor, nullptr, mStepSize);
        }
    }

    if (bLoopSuccess) {
        // invoke derived method
        bLoopSuccess = DoFinish();
    }

    return bLoopSuccess;
}

size_t SpectrumTransformer::CurrentQueueSize() const
{
    auto allocSize = mQueue.size();
    auto size = mOutStepCount + allocSize;
    if (mLeadingPadding) {
        size += mStepsPerWindow - 1;
    }

    if (size < allocSize) {
        return size.as_size_t();
    } else {
        return allocSize;
    }
}

// Formerly part of EffectNoiseReduction::Worker::ReduceNoise()
void SpectrumTransformer::OutputStep()
{
    if (!mNeedsOutput) {
        return;
    }
    if (QueueIsFull()) {
        const auto last = mSpectrumSize - 1;
        Window& record = **mQueue.rbegin();

        const float* pReal = &record.mRealFFTs[1];
        const float* pImag = &record.mImagFFTs[1];
        float* pBuffer = &mFFTBuffer[2];
        auto nn = mSpectrumSize - 2;
        for (; nn--;) {
            *pBuffer++ = *pReal++;
            *pBuffer++ = *pImag++;
        }
        mFFTBuffer[0] = record.mRealFFTs[0];
        // The Fs/2 component is stored as the imaginary part of the DC component
        mFFTBuffer[1] = record.mImagFFTs[0];

        // Invert the FFT into the output buffer
        InverseRealFFTf(mFFTBuffer.data(), hFFT.get());

        // Overlap-add
        if (mOutWindow.size() > 0) {
            auto pOut = mOutOverlapBuffer.data();
            auto pWindow = mOutWindow.data();
            auto pBitReversed = &hFFT->BitReversed[0];
            for (size_t jj = 0; jj < last; ++jj) {
                auto kk = *pBitReversed++;
                *pOut++ += mFFTBuffer[kk] * (*pWindow++);
                *pOut++ += mFFTBuffer[kk + 1] * (*pWindow++);
            }
        } else {
            auto pOut = mOutOverlapBuffer.data();
            auto pBitReversed = &hFFT->BitReversed[0];
            for (size_t jj = 0; jj < last; ++jj) {
                auto kk = *pBitReversed++;
                *pOut++ += mFFTBuffer[kk];
                *pOut++ += mFFTBuffer[kk + 1];
            }
        }
        auto buffer = mOutOverlapBuffer.data();
        if (mOutStepCount >= 0) {
            // Output the first portion of the overlap buffer, they're done
            DoOutput(buffer, mStepSize);
        }
        // Shift the remainder over.
        memmove(buffer, buffer + mStepSize, sizeof(float) * (mWindowSize - mStepSize));
        std::fill(buffer + mWindowSize - mStepSize, buffer + mWindowSize, 0.0f);
    }
}

bool SpectrumTransformer::QueueIsFull() const
{
    if (mLeadingPadding) {
        return mOutStepCount >= -static_cast<int>(mStepsPerWindow - 1);
    } else {
        return mOutStepCount >= 0;
    }
}

SpectrumTransformer::~SpectrumTransformer() = default;

SpectrumTransformer::Window::~Window() = default;
