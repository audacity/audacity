/*!********************************************************************

Audacity: A Digital Audio Editor

SpectrumTransformer.h
@brief Transformer of sample sequences by FFT, coefficient changes, inverse FFT, overlap-add

Paul Licameli

**********************************************************************/

#ifndef __AUDACITY_SPECTRUM_TRANSFORMER__
#define __AUDACITY_SPECTRUM_TRANSFORMER__
 
enum eWindowFunctions : int;

class SpectrumTransformer /* not final */
{
public:
   // Public interface
   using FloatVector = std::vector<float>;

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

   //! Invokes DoStart
   /*! @return success */
   bool Start(size_t queueLength);

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

  /*! More queue initializations can be done here.
      @return false to abort processing. Default implementation just returns true. */
   virtual bool DoStart();

   //! Called only if `NeedsOutput()`
   virtual void DoOutput(const float *outBuffer, size_t mStepSize) = 0;

   virtual bool DoFinish();

   //! How many windows in the queue have been allocated?
   size_t TotalQueueSize() const { return mQueue.size(); }

   Window &Nth(int n) { return *mQueue[n]; }

   Window &Newest() { return **mQueue.begin(); }
   Window &Latest() { return **mQueue.rbegin(); }

   void ResizeQueue(size_t queueLength);

   const size_t mWindowSize;
   const size_t mSpectrumSize;

   const unsigned mStepsPerWindow;
   const size_t mStepSize;
   
   const bool mLeadingPadding;

   const bool mTrailingPadding;

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

   bool Process( WaveTrack *track, sampleCount start, sampleCount len );

protected:
   bool DoStart() override;
   void DoOutput(const float *outBuffer, size_t mStepSize) override;
   bool DoFinish() override;

   WaveTrack *mpTrack = nullptr;
   std::shared_ptr<WaveTrack> mOutputTrack;
   sampleCount mStart = 0, mLen = 0;
};

#endif
