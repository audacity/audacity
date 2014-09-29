#ifdef EXPERIMENTAL_EQ_SSE_THREADED

/**********************************************************************

Audacity: A Digital Audio Editor

Equalization48x.h

Intrinsics (SSE/AVX) and Threaded Equalization

***********************************************************************/

#ifndef __AUDACITY_EFFECT_EQUALIZATION48X__
#define __AUDACITY_EFFECT_EQUALIZATION48X__

// bitwise function selection
// options are
#define MATH_FUNCTION_ORIGINAL 0 // 0 original path
#define MATH_FUNCTION_BITREVERSE_TABLE 1 // 1 SSE BitReverse Table
#define MATH_FUNCTION_SIN_COS_TABLE 2 // 2 SSE SinCos Table
// 3 SSE with SinCos and BitReverse buffer
#define MATH_FUNCTION_SSE 4 // 4 SSE no SinCos and no BitReverse buffer
#define MATH_FUNCTION_THREADED 8 // 8 SSE threaded no SinCos and no BitReverse buffer
// 9 SSE threaded BitReverse Table
// 10 SSE threaded SinCos Table
// 11 SSE threaded with SinCos and BitReverse buffer
//#define MATH_FUNCTION_AVX 16

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
   BufferInfo() { mBufferLength=0; mBufferStatus=BufferEmpty; };
   float* mBufferSouce[4];
   float* mBufferDest[4];
   int mBufferLength;
   sampleCount mFftWindowSize;
   sampleCount mFftFilterSize;
   float* mScratchBuffer;
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
   virtual void* Entry();
   BufferInfo* mBufferInfoList;
   int mBufferInfoCount, mThreadID;
   wxMutex *mMutex;
   EffectEqualization48x *mEffectEqualization48x;
   bool mExitLoop;
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
   bool TrackCompare();
   bool DeltaTrack(WaveTrack * t, WaveTrack * t2, sampleCount start, sampleCount len);
   bool AllocateBuffersWorkers(bool threaded);
   bool FreeBuffersWorkers();
   bool ProcessBuffer(fft_type *sourceBuffer, fft_type *destBuffer, sampleCount bufferLength);
   bool ProcessBuffer4x(BufferInfo *bufferInfo);
   bool ProcessOne4x(int count, WaveTrack * t, sampleCount start, sampleCount len);
   bool ProcessOne4xThreaded(int count, WaveTrack * t, sampleCount start, sampleCount len);
   bool ProcessTail(WaveTrack * t, WaveTrack * output, sampleCount start, sampleCount len);
   void Filter4x(sampleCount len, float *buffer, float *scratchBuffer);

   EffectEqualization* mEffectEqualization;
   int mThreadCount;
   sampleCount mFilterSize;
   sampleCount mBlockSize;
   sampleCount mWindowSize;
   int mWorkerDataCount;
   int mBlocksPerBuffer;
   int mScratchBufferSize;
   int mSubBufferSize;
   float *mBigBuffer;
   BufferInfo* mBufferInfo;
   wxMutex mDataMutex;
   EQWorker* mEQWorkers;
   bool mThreaded;
   bool mBenching;
   friend EQWorker;
};

#endif

#endif