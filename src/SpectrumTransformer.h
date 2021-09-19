/*!********************************************************************

Audacity: A Digital Audio Editor

SpectrumTransformer.h
@brief Transformer of sample sequences by FFT, coefficient changes, inverse FFT, overlap-add

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_TRANSFORMER__
#define __AUDACITY_SPECTRUM_TRANSFORMER__
 
#include <functional>
#include <memory>
#include <vector>
#include "audacity/Types.h"
#include "RealFFTf.h"
#include "SampleCount.h"

enum eWindowFunctions : int;

/*!
 @brief A class that transforms a portion of a wave track (preserving duration)
 by applying Fourier transform, then modifying coefficients, then inverse
 Fourier transform and overlap-add to reconstruct.
 
 @par The procedure that modifies coefficients can be varied, and can employ lookahead
 and -behind to nearby windows.  May also be used just to gather information
 without producing output.
*/
class SpectrumTransformer /* not final */
{
public:
   // Public interface
   using FloatVector = std::vector<float>;

   //! Type of function that transforms windows in the queue
   /*! Called repeatedly, with the newest window in the queue taken from
      input, and the last window of the queue about to be inverse-transformed for output.
      @return false to abort processing. */
   using WindowProcessor = std::function< bool(SpectrumTransformer&) >;

   /*!
    @pre `!(inWindowType == eWinFuncRectangular && outWindowType eWinFuncRectangular)`
    @pre `windowSize % stepsPerWindow == 0`
    @pre `windowSize` is a power of 2
    */
   SpectrumTransformer(
      bool needsOutput, //!< Whether to do the inverse FFT
      eWindowFunctions inWindowType, //!< Used in FFT transform
      eWindowFunctions outWindowType, //!< Used in inverse FFT transform
      size_t windowSize,     //!< must be a power of 2
      unsigned stepsPerWindow, //!< determines the overlap
      bool leadingPadding, /*!<
         Whether to start the queue with windows that partially overlap
         the first full window of input samples */
      bool trailingPadding /*!<
         Whether to stop the procedure after the last complete window of input
         is added to the queue */
   );
   virtual ~SpectrumTransformer();

   bool NeedsOutput() const { return mNeedsOutput; }

   //! Call once before a sequence of calls to ProcessSamples; Invokes DoStart
   /*! @return success */
   bool Start(size_t queueLength);

   //! Call multiple times
   /*!
    @param buffer null if flushing the end
    @return success */
   bool ProcessSamples(const WindowProcessor &processor,
      const float *buffer, size_t len);

   //! Call once after a sequence of calls to ProcessSamples; flushes the queue and Invokes DoFinish
   /*! @return success */
   bool Finish(const WindowProcessor &processor);

   //! Derive this class to add information to the queue.  @see NewWindow()
   struct Window
   {
      explicit Window(size_t windowSize)
         : mRealFFTs( windowSize / 2 )
         , mImagFFTs( windowSize / 2 )
      {
      }

      virtual ~Window();

      void Zero()
      {
         const auto size = mRealFFTs.size();
         auto pFill = mRealFFTs.data();
         std::fill(pFill, pFill + size, 0.0f);
         pFill = mImagFFTs.data();
         std::fill(pFill, pFill + size, 0.0f);
      }

      //! index zero holds the dc coefficient, which has no imaginary part
      FloatVector mRealFFTs;
      //! index zero holds the nyquist frequency coefficient, actually real
      FloatVector mImagFFTs;
   };

   //! Allocates a window to place in the queue.
   /*! Only when initializing -- windows are recycled thereafter.
      You can derive from Window to add fields, and then override this factory function. */
   virtual std::unique_ptr<Window> NewWindow(size_t windowSize);

   //! Called before any calls to ProcessWindow.
   /*! More queue initializations can be done here.
      @return false to abort processing. Default implementation just returns true. */
   virtual bool DoStart();

   //! Called within ProcessSamples if output was requested
   virtual void DoOutput(const float *outBuffer, size_t mStepSize) = 0;

   //! Called after the last call to ProcessWindow().
   /*! @return false to abort processing. Default implementation just returns true. */
   virtual bool DoFinish();

   /// Useful functions to implement WindowProcesser:

   //! How many windows in the queue have been allocated?
   size_t TotalQueueSize() const { return mQueue.size(); }

   //! How many windows in the queue have been filled?
   /*! (Not always the allocated size of the queue) */
   size_t CurrentQueueSize() const;

   /*! Whether the last window in the queue overlapped the input
      at least partially and its coefficients will affect output. */
   bool QueueIsFull() const;

   //! Access the queue, so you can inspect and modify any window in it
   /*! Newer windows are at earlier indices.  You can't modify the length of it */
   Window &Nth(int n) { return *mQueue[n]; }

   Window &Newest() { return **mQueue.begin(); }
   Window &Latest() { return **mQueue.rbegin(); }

private:
   void ResizeQueue(size_t queueLength);
   void FillFirstWindow();
   void RotateWindows();
   void OutputStep();

protected:
   const size_t mWindowSize;
   const size_t mSpectrumSize;

   const unsigned mStepsPerWindow;
   const size_t mStepSize;
   
   const bool mLeadingPadding;

   const bool mTrailingPadding;

private:
   std::vector<std::unique_ptr<Window>> mQueue;
   HFFT     hFFT;
   sampleCount mInSampleCount = 0;
   sampleCount mOutStepCount = 0; //!< sometimes negative
   size_t mInWavePos = 0;

   //! These have size mWindowSize:
   FloatVector mFFTBuffer;
   FloatVector mInWaveBuffer;
   FloatVector mOutOverlapBuffer;
   //! These have size mWindowSize, or 0 for rectangular window:
   FloatVector mInWindow;
   FloatVector mOutWindow;

   const bool mNeedsOutput;
};

class WaveTrack;

//! Subclass of SpectrumTransformer that rewrites a track
class TrackSpectrumTransformer /* not final */ : public SpectrumTransformer {
public:
   using SpectrumTransformer::SpectrumTransformer;
   ~TrackSpectrumTransformer() override;

   //! Invokes Start(), ProcessSamples(), and Finish()
   bool Process( const WindowProcessor &processor, WaveTrack *track,
      size_t queueLength, sampleCount start, sampleCount len);

protected:
   bool DoStart() override;
   void DoOutput(const float *outBuffer, size_t mStepSize) override;
   bool DoFinish() override;

private:
   WaveTrack *mpTrack = nullptr;
   std::shared_ptr<WaveTrack> mOutputTrack;
   sampleCount mStart = 0, mLen = 0;
};

#endif
