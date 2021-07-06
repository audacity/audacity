/**********************************************************************

Audacity: A Digital Audio Editor

Equalization48x.h

Intrinsics (SSE/AVX) and Threaded Equalization

***********************************************************************/

#ifndef __AUDACITY_EFFECT_EQUALIZATION48X__
#define __AUDACITY_EFFECT_EQUALIZATION48X__

#ifdef EXPERIMENTAL_EQ_SSE_THREADED

#include <memory>

#include <wx/thread.h> // to inherit
#include <audacity/Types.h>
class WaveTrack;
using fft_type = float;

#ifdef __AVX_ENABLED
#define __MAXBUFFERCOUNT 8
#else
#define __MAXBUFFERCOUNT 4
#endif

// bitwise function selection
// options are 
#define MATH_FUNCTION_ORIGINAL 0 // 0 original path
#define MATH_FUNCTION_BITREVERSE_TABLE 1 // 1 SSE BitReverse Table
#define MATH_FUNCTION_SIN_COS_TABLE 2 // 2 SSE SinCos Table
#define MATH_FUNCTION_THREADED 4 // 4 SSE threaded no SinCos and no BitReverse buffer
#define MATH_FUNCTION_SSE 8 // 8 SSE no SinCos and no BitReverse buffer
#define MATH_FUNCTION_AVX 16
#define MATH_FUNCTION_SEGMENTED_CODE 32

struct free_simd {
   void operator () (void*) const;
};
using simd_floats = std::unique_ptr< float[], free_simd >;

// added by Andrew Hallendorff intrinsics processing
enum EQBufferStatus
{
   BufferEmpty=0,
   BufferReady,
   BufferBusy,
   BufferDone
};

class BufferInfo {
public:
   BufferInfo() { mBufferLength=0; mBufferStatus=BufferEmpty; mContiguousBufferSize=0; };
   float* mBufferSouce[__MAXBUFFERCOUNT];
   float* mBufferDest[__MAXBUFFERCOUNT];
   size_t mBufferLength;
   size_t mFftWindowSize;
   size_t mFftFilterSize;
   float* mScratchBuffer;
   size_t mContiguousBufferSize;
   EQBufferStatus mBufferStatus;
};

typedef struct {
   int x64;
   int MMX;
   int SSE;
   int SSE2;
   int SSE3;
   int SSSE3;
   int SSE41;
   int SSE42;
   int SSE4a;
   int AVX;
   int XOP;
   int FMA3;
   int FMA4;
} MathCaps;

class EffectEqualization;

class EffectEqualization48x;

static int EQWorkerCounter=0;

class EQWorker : public wxThread {
public:
   EQWorker():wxThread(wxTHREAD_JOINABLE) {   
      mBufferInfoList=NULL;
      mBufferInfoCount=0;
      mMutex=NULL;
      mEffectEqualization48x=NULL;
      mExitLoop=false;
      mThreadID=EQWorkerCounter++;
      mProcessingType=4;
   }
   void SetData( BufferInfo* bufferInfoList, int bufferInfoCount, wxMutex *mutex, EffectEqualization48x *effectEqualization48x) {
      mBufferInfoList=bufferInfoList;
      mBufferInfoCount=bufferInfoCount;
      mMutex=mutex;
      mEffectEqualization48x=effectEqualization48x;
   }
   void ExitLoop() { // this will cause the thread to drop from the loops
      mExitLoop=true;
   }
   void* Entry() override;
   BufferInfo* mBufferInfoList;
   int mBufferInfoCount, mThreadID;
   wxMutex *mMutex;
   EffectEqualization48x *mEffectEqualization48x;
   bool mExitLoop;
   int mProcessingType;
};

class EffectEqualization48x {

public:

   EffectEqualization48x();
   virtual ~EffectEqualization48x();

   static MathCaps *GetMathCaps();
   static void SetMathPath(int mathPath);
   static int GetMathPath();
   static void AddMathPathOption(int mathPath);
   static void RemoveMathPathOption(int mathPath);

   bool Process(EffectEqualization* effectEqualization);
   bool Benchmark(EffectEqualization* effectEqualization);
private:
   bool RunFunctionSelect(int flags, int count, WaveTrack * t, sampleCount start, sampleCount len);
   bool TrackCompare();
   bool DeltaTrack(WaveTrack * t, const WaveTrack * t2, sampleCount start, sampleCount len);
   bool AllocateBuffersWorkers(int nThreads);
   bool FreeBuffersWorkers();

   bool ProcessTail(WaveTrack * t, WaveTrack * output, sampleCount start, sampleCount len);

   bool ProcessBuffer(fft_type *sourceBuffer, fft_type *destBuffer, size_t bufferLength);
   bool ProcessBuffer1x(BufferInfo *bufferInfo);
   bool ProcessOne1x(int count, WaveTrack * t, sampleCount start, sampleCount len);
   void Filter1x(size_t len, float *buffer, float *scratchBuffer);

   bool ProcessBuffer4x(BufferInfo *bufferInfo);
   bool ProcessOne4x(int count, WaveTrack * t, sampleCount start, sampleCount len);
   bool ProcessOne1x4xThreaded(int count, WaveTrack * t, sampleCount start, sampleCount len, int processingType=4);
   void Filter4x(size_t len, float *buffer, float *scratchBuffer);

#ifdef __AVX_ENABLED
   bool ProcessBuffer8x(BufferInfo *bufferInfo);
   bool ProcessOne8x(int count, WaveTrack * t, sampleCount start, sampleCount len);
   bool ProcessOne8xThreaded(int count, WaveTrack * t, sampleCount start, sampleCount len);
   void Filter8x(size_t len, float *buffer, float *scratchBuffer);
#endif
   
   EffectEqualization* mEffectEqualization;
   size_t mThreadCount;
   size_t mFilterSize;
   size_t mBlockSize;
   size_t  mWindowSize;
   int mBufferCount;
   size_t mWorkerDataCount;
   size_t mBlocksPerBuffer;
   size_t mScratchBufferSize;
   size_t mSubBufferSize;
   simd_floats mBigBuffer;
   ArrayOf<BufferInfo> mBufferInfo;
   wxMutex mDataMutex;
   ArrayOf<EQWorker> mEQWorkers;
   bool mThreaded;
   bool mBenching;
   friend EQWorker;
   friend EffectEqualization;
};

#endif

#endif
