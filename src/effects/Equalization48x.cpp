/**********************************************************************

   Audacity: A Digital Audio Editor

   EffectEqualization.cpp

   Andrew Hallendorff

*******************************************************************//**

   \file Equalization48x.cpp
   \brief Fast SSE based implementation of equalization.

*//****************************************************************/

#include "../Audacity.h"
#include "../Project.h"
#ifdef EXPERIMENTAL_EQ_SSE_THREADED
#include "Equalization.h"
#include "../WaveTrack.h"
#include "float_cast.h"
#include <vector>

#include <wx/dcmemory.h>
#include <wx/event.h>
#include <wx/string.h>

#if wxUSE_TOOLTIPS
#include <wx/tooltip.h>
#endif
#include <wx/utils.h>

#include <math.h>

#include <wx/arrimpl.cpp>

#include "Equalization48x.h"
#include "../RealFFTf.h"
#include "../RealFFTf48x.h"

#ifndef USE_SSE2
#define	USE_SSE2
#endif

#include <stdlib.h>
#include <malloc.h>
#include <stdio.h>
#include <math.h>
#include <xmmintrin.h>

#ifdef _WIN32

//  Windows
#include <intrin.h>
#define cpuid    __cpuid

#else

//  GCC Inline Assembly
void cpuid(int CPUInfo[4],int InfoType){
   __asm__ __volatile__ (
      "cpuid":
   "=a" (CPUInfo[0]),
      "=b" (CPUInfo[1]),
      "=c" (CPUInfo[2]),
      "=d" (CPUInfo[3]) :
   "a" (InfoType)
      );
}

#endif


bool sMathCapsInitialized = false;
MathCaps sMathCaps;

// dirty switcher
int sMathPath=MATH_FUNCTION_SSE|MATH_FUNCTION_THREADED;
void EffectEqualization48x::SetMathPath(int mathPath) { sMathPath=mathPath; };
int EffectEqualization48x::GetMathPath() { return sMathPath; };
void EffectEqualization48x::AddMathPathOption(int mathPath) { sMathPath|=mathPath; };
void EffectEqualization48x::RemoveMathPathOption(int mathPath) { sMathPath&=~mathPath; };

MathCaps *EffectEqualization48x::GetMathCaps() 
{ 
   if(!sMathCapsInitialized)
   {
      sMathCapsInitialized=true;
      sMathCaps.x64     = false;
      sMathCaps.MMX     = false;
      sMathCaps.SSE     = false;
      sMathCaps.SSE2    = false;
      sMathCaps.SSE3    = false;
      sMathCaps.SSSE3   = false;
      sMathCaps.SSE41   = false;
      sMathCaps.SSE42   = false;
      sMathCaps.SSE4a   = false;
      sMathCaps.AVX     = false;
      sMathCaps.XOP     = false;
      sMathCaps.FMA3    = false;
      sMathCaps.FMA4    = false;

      int info[4];
      cpuid(info, 0);
      int nIds = info[0];

      cpuid(info, 0x80000000);
      int nExIds = info[0];

      //  Detect Instruction Set
      if (nIds >= 1){
         cpuid(info,0x00000001);
         sMathCaps.MMX   = (info[3] & ((int)1 << 23)) != 0;
         sMathCaps.SSE   = (info[3] & ((int)1 << 25)) != 0;
         sMathCaps.SSE2  = (info[3] & ((int)1 << 26)) != 0;
         sMathCaps.SSE3  = (info[2] & ((int)1 <<  0)) != 0;

         sMathCaps.SSSE3 = (info[2] & ((int)1 <<  9)) != 0;
         sMathCaps.SSE41 = (info[2] & ((int)1 << 19)) != 0;
         sMathCaps.SSE42 = (info[2] & ((int)1 << 20)) != 0;

         sMathCaps.AVX   = (info[2] & ((int)1 << 28)) != 0;
         sMathCaps.FMA3  = (info[2] & ((int)1 << 12)) != 0;
      }

      if (nExIds >= 0x80000001){
         cpuid(info,0x80000001);
         sMathCaps.x64   = (info[3] & ((int)1 << 29)) != 0;
         sMathCaps.SSE4a = (info[2] & ((int)1 <<  6)) != 0;
         sMathCaps.FMA4  = (info[2] & ((int)1 << 16)) != 0;
         sMathCaps.XOP   = (info[2] & ((int)1 << 11)) != 0;
      }
      if(sMathCaps.SSE)
         sMathPath=MATH_FUNCTION_SSE|MATH_FUNCTION_THREADED; // we are starting on.
   }
   return &sMathCaps; 
};

void * malloc_simd(const size_t size)
{
#if defined WIN32           // WIN32
    return _aligned_malloc(size, 16);
#elif defined __linux__     // Linux
    return memalign (16, size);
#elif defined __MACH__      // Mac OS X
    return malloc(size);
#else                       // other (use valloc for page-aligned memory)
    return valloc(size);
#endif
}

void free_simd(void* mem)
{
#if defined WIN32           // WIN32
    _aligned_free(mem);
#else  
    free(mem);
#endif
}

EffectEqualization48x::EffectEqualization48x():
         mThreadCount(0),mFilterSize(0),mWindowSize(0),mBlockSize(0),mWorkerDataCount(0),mBlocksPerBuffer(20),
         mScratchBufferSize(0),mSubBufferSize(0),mBigBuffer(NULL),mBufferInfo(NULL),mEQWorkers(0),mThreaded(false),
         mBenching(false)
{
}

EffectEqualization48x::~EffectEqualization48x()
{
}


bool EffectEqualization48x::AllocateBuffersWorkers(bool threaded)
{
   if(mBigBuffer)
      FreeBuffersWorkers(); 
   mFilterSize=(mEffectEqualization->mM-1)&(~15); // 4000 !!! Filter MUST BE QUAD WORD ALIGNED !!!!
   mWindowSize=mEffectEqualization->windowSize;
   mBlockSize=mWindowSize-mFilterSize; // 12,384
   mThreaded=threaded;
   if( mThreaded )
   {
      mThreadCount=wxThread::GetCPUCount();
      mWorkerDataCount=mThreadCount+2; // 2 extra slots (maybe double later)

      // we're skewing the data by one block to allow for 1/4 block intersections.
      // this will remove the disparity in data at the intersections of the runs

      // The nice magic allocation
      // megabyte - 3 windows - 4 overlaping buffers - filter 
      // 2^20 = 1,048,576 - 3 * 2^14 (16,384) - ((4 * 20) - 3) * 12,384 - 4000 
      // 1,048,576 - 49,152 - 953,568 - 4000 = 41,856 (leftover)

      mScratchBufferSize=mWindowSize*3*(sizeof(__m128)/sizeof(float)); // 3 window size blocks size of __m128 but we allocate in float
      mSubBufferSize=mBlockSize*((mBlocksPerBuffer<<2)-3); // we are going to do a full block overlap -(blockSize*3)
      mBigBuffer=(float *)malloc_simd(sizeof(float)*(mSubBufferSize+mFilterSize+mScratchBufferSize)*mWorkerDataCount); // we run over by filtersize
      // fill the bufferInfo
      mBufferInfo = new BufferInfo[mWorkerDataCount];
      for(int i=0;i<mWorkerDataCount;i++) {
         mBufferInfo[i].mFftWindowSize=mWindowSize;
         mBufferInfo[i].mFftFilterSize=mFilterSize;
         mBufferInfo[i].mBufferLength=mBlockSize*mBlocksPerBuffer;
         mBufferInfo[i].mScratchBuffer=&mBigBuffer[(mSubBufferSize+mScratchBufferSize)*i+mSubBufferSize];
         for(int j=0;j<4;j++)
            mBufferInfo[i].mBufferDest[j]=mBufferInfo[i].mBufferSouce[j]=&mBigBuffer[j*(mBufferInfo[i].mBufferLength-mBlockSize)+(mSubBufferSize+mScratchBufferSize)*i];
      }
      // start the workers
      mDataMutex.IsOk();
      mEQWorkers=new EQWorker[mThreadCount];
      for(int i=0;i<mThreadCount;i++) {
         mEQWorkers[i].SetData( mBufferInfo, mWorkerDataCount, &mDataMutex, this);
         mEQWorkers[i].Create();
         mEQWorkers[i].Run();
      }
   } else {
      mScratchBufferSize=mWindowSize*3*(sizeof(__m128)/sizeof(float)); // 3 window size blocks size of __m128
      mSubBufferSize=mBlockSize*((mBlocksPerBuffer<<2)-3); // we are going to do a full block overlap -(blockSize*3)
      mBigBuffer=(float *)malloc_simd(sizeof(float)*(mSubBufferSize+mFilterSize+mScratchBufferSize)); // we run over by filtersize
      mBufferInfo = new BufferInfo[1]; // yeah it looks odd but it keeps compatibility with threaded processing
      mBufferInfo[0].mFftWindowSize=mWindowSize;
      mBufferInfo[0].mFftFilterSize=mFilterSize;
      mBufferInfo[0].mBufferLength=mBlockSize*mBlocksPerBuffer;
      mBufferInfo[0].mScratchBuffer=&mBigBuffer[mSubBufferSize];
      for(int j=0;j<4;j++)
         mBufferInfo[0].mBufferDest[j]=mBufferInfo[0].mBufferSouce[j]=&mBigBuffer[j*(mBufferInfo[0].mBufferLength-mBlockSize)];
   }
   return true;
}

bool EffectEqualization48x::FreeBuffersWorkers()
{
   if(mThreaded) {
      for(int i=0;i<mThreadCount;i++) { // tell all the workers to exit
         mEQWorkers[i].ExitLoop();
      }
      for(int i=0;i<mThreadCount;i++) {
         mEQWorkers[i].Wait();
      }
      delete[] mEQWorkers; // kill the workers ( go directly to jail)
      mEQWorkers= NULL;
      mThreadCount=0;
      mWorkerDataCount=0; 
   }
   delete [] mBufferInfo;
   mBufferInfo = NULL;
   free_simd(mBigBuffer);
   mBigBuffer=NULL;
   return true;
}

bool EffectEqualization48x::Process(EffectEqualization* effectEqualization)
{
   mEffectEqualization=effectEqualization;
//   return TrackCompare(); // used for debugging data
   mEffectEqualization->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;

   TableUsage(sMathPath);
   if(sMathPath)  // !!! Filter MUST BE QUAD WORD ALIGNED !!!!
      mEffectEqualization->mM=(mEffectEqualization->mM&(~15))+1;
   AllocateBuffersWorkers((sMathPath & MATH_FUNCTION_THREADED) != 0);
   SelectedTrackListOfKindIterator iter(Track::Wave, mEffectEqualization->mOutputTracks);
   WaveTrack *track = (WaveTrack *) iter.First();
   int count = 0;
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mEffectEqualization->mT0 < trackStart? trackStart: mEffectEqualization->mT0;
      double t1 = mEffectEqualization->mT1 > trackEnd? trackEnd: mEffectEqualization->mT1;

      if (t1 > t0) {
         sampleCount start = track->TimeToLongSamples(t0);
         sampleCount end = track->TimeToLongSamples(t1);
         sampleCount len = (sampleCount)(end - start);

         if(!sMathPath) {
            if (!mEffectEqualization->ProcessOne(count, track, start, len))
            {
               bGoodResult = false;
               break;
            }
         } else {
            if(sMathPath<8) {
               if (!ProcessOne4x(count, track, start, len))
               {
                  bGoodResult = false;
                  break;
               }
            } else {
               if (!ProcessOne4xThreaded(count, track, start, len))
               {
                  bGoodResult = false;
                  break;
               }
            }
         }


      }

      track = (WaveTrack *) iter.Next();
      count++;
   }
   FreeBuffersWorkers();

   mEffectEqualization->ReplaceProcessedTracks(bGoodResult); 
   return bGoodResult;
}

bool EffectEqualization48x::TrackCompare()
{
   mEffectEqualization->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;

   TableUsage(sMathPath);
   if(sMathPath)  // !!! Filter MUST BE QUAD WORD ALIGNED !!!!
      mEffectEqualization->mM=(mEffectEqualization->mM&(~15))+1;
   AllocateBuffersWorkers((sMathPath & MATH_FUNCTION_THREADED)!=0);
   // Reset map
   wxArrayPtrVoid SecondIMap;
   wxArrayPtrVoid SecondOMap;
   SecondIMap.Clear();
   SecondOMap.Clear();

   TrackList      *SecondOutputTracks = new TrackList();

   //iterate over tracks of type trackType (All types if Track::All)
   TrackListOfKindIterator aIt(mEffectEqualization->mOutputTracksType, mEffectEqualization->mTracks);

   for (Track *aTrack = aIt.First(); aTrack; aTrack = aIt.Next()) {

      // Include selected tracks, plus sync-lock selected tracks for Track::All.
      if (aTrack->GetSelected() ||
         (mEffectEqualization->mOutputTracksType == Track::All && aTrack->IsSyncLockSelected()))
      {
         Track *o = aTrack->Duplicate();
         SecondOutputTracks->Add(o);
         SecondIMap.Add(aTrack);
         SecondIMap.Add(o);
      }
   }

   for(int i=0;i<2;i++) {
      SelectedTrackListOfKindIterator iter(Track::Wave, i?mEffectEqualization->mOutputTracks:SecondOutputTracks);
      i?sMathPath=sMathPath:sMathPath=0;
      WaveTrack *track = (WaveTrack *) iter.First();
      int count = 0;
      while (track) {
         double trackStart = track->GetStartTime();
         double trackEnd = track->GetEndTime();
         double t0 = mEffectEqualization->mT0 < trackStart? trackStart: mEffectEqualization->mT0;
         double t1 = mEffectEqualization->mT1 > trackEnd? trackEnd: mEffectEqualization->mT1;

         if (t1 > t0) {
            sampleCount start = track->TimeToLongSamples(t0);
            sampleCount end = track->TimeToLongSamples(t1);
            sampleCount len = (sampleCount)(end - start);

            if(!sMathPath) {
               if (!mEffectEqualization->ProcessOne(count, track, start, len))
               {
                  bGoodResult = false;
                  break;
               }
            } else {
               if(sMathPath<8) {
                  if (!ProcessOne4x(count, track, start, len))
                  {
                     bGoodResult = false;
                     break;
                  }
               } else {
                  if (!ProcessOne4xThreaded(count, track, start, len))
                  {
                     bGoodResult = false;
                     break;
                  }
               }
            }
         }
         track = (WaveTrack *) iter.Next();
         count++;
      }
   }
   SelectedTrackListOfKindIterator iter(Track::Wave, mEffectEqualization->mOutputTracks);
   SelectedTrackListOfKindIterator iter2(Track::Wave, SecondOutputTracks);
   WaveTrack *track =  (WaveTrack *) iter.First();
   WaveTrack *track2 = (WaveTrack *) iter2.First();
   while (track) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mEffectEqualization->mT0 < trackStart? trackStart: mEffectEqualization->mT0;
      double t1 = mEffectEqualization->mT1 > trackEnd? trackEnd: mEffectEqualization->mT1;

      if (t1 > t0) {
         sampleCount start = track->TimeToLongSamples(t0);
         sampleCount end = track->TimeToLongSamples(t1);
         sampleCount len = (sampleCount)(end - start);
         DeltaTrack(track, track2, start, len);
      }
      track = (WaveTrack *) iter.Next();
      track2 = (WaveTrack *) iter2.Next();
   }
   delete SecondOutputTracks;
   FreeBuffersWorkers();
   mEffectEqualization->ReplaceProcessedTracks(bGoodResult); 
   return bGoodResult;
}


bool EffectEqualization48x::DeltaTrack(WaveTrack * t, WaveTrack * t2, sampleCount start, sampleCount len)
{

   sampleCount trackBlockSize = t->GetMaxBlockSize();

   float *buffer1 = new float[trackBlockSize];
   float *buffer2 = new float[trackBlockSize];

   AudacityProject *p = GetActiveProject();
   WaveTrack *output=p->GetTrackFactory()->NewWaveTrack(floatSample, t->GetRate());
   sampleCount originalLen = len;
   sampleCount currentSample = start;

   while(len) {
      sampleCount curretLength=(trackBlockSize>len)?len:trackBlockSize;
      t->Get((samplePtr)buffer1, floatSample, currentSample, curretLength);
      t2->Get((samplePtr)buffer2, floatSample, currentSample, curretLength);
      for(int i=0;i<curretLength;i++)
         buffer1[i]-=buffer2[i];
      output->Append((samplePtr)buffer1, floatSample, curretLength);
      currentSample+=curretLength;
      len-=curretLength;
   }
   delete[] buffer1;
   delete[] buffer2;
   output->Flush();
   len=originalLen;
   ProcessTail(t, output, start, len);
   delete output;
   return true;
}

bool EffectEqualization48x::Benchmark(EffectEqualization* effectEqualization)
{
   mEffectEqualization=effectEqualization;
   mEffectEqualization->CopyInputTracks(); // Set up mOutputTracks.
   bool bGoodResult = true;

   TableUsage(sMathPath);
   if(sMathPath)  // !!! Filter MUST BE QUAD WORD ALIGNED !!!!
      mEffectEqualization->mM=(mEffectEqualization->mM&(~15))+1;
   AllocateBuffersWorkers((bool)MATH_FUNCTION_THREADED);
   SelectedTrackListOfKindIterator iter(Track::Wave, mEffectEqualization->mOutputTracks);
   long times[] = { 0,0,0 };
   wxStopWatch timer;
   mBenching=true;
   for(int i=0;i<3;i++) {
      int localMathPath;
      switch(i) {
         case 0: localMathPath=MATH_FUNCTION_SSE|MATH_FUNCTION_THREADED;
                 if(!sMathCaps.SSE)
                    localMathPath=-1;
            break;
         case 1: localMathPath=MATH_FUNCTION_SSE;
                 if(!sMathCaps.SSE)
                    localMathPath=-1;
            break;
         case 2: localMathPath=0;
            break;
         default: localMathPath=-1;
      }
      if(localMathPath>=0) {
         timer.Start();
         WaveTrack *track = (WaveTrack *) iter.First();
         int count = 0;
         while (track) {
            double trackStart = track->GetStartTime();
            double trackEnd = track->GetEndTime();
            double t0 = mEffectEqualization->mT0 < trackStart? trackStart: mEffectEqualization->mT0;
            double t1 = mEffectEqualization->mT1 > trackEnd? trackEnd: mEffectEqualization->mT1;

            if (t1 > t0) {
               sampleCount start = track->TimeToLongSamples(t0);
               sampleCount end = track->TimeToLongSamples(t1);
               sampleCount len = (sampleCount)(end - start);

               if(!localMathPath) {
                  if (!mEffectEqualization->ProcessOne(count, track, start, len))
                  {
                     bGoodResult = false;
                     break;
                  }
               } else {
                  if(localMathPath<8) {
                     if (!ProcessOne4x(count, track, start, len))
                     {
                        bGoodResult = false;
                        break;
                     }
                  } else {
                     if (!ProcessOne4xThreaded(count, track, start, len))
                     {
                        bGoodResult = false;
                        break;
                     }
                  }
               }
            }
            track = (WaveTrack *) iter.Next();
            count++;
         }
         times[i]=timer.Time();
      }
   }
   FreeBuffersWorkers();
   mBenching=false;
   bGoodResult=false;
   mEffectEqualization->ReplaceProcessedTracks(bGoodResult); 

   wxTimeSpan tsSSEThreaded(0, 0, 0, times[0]);
   wxTimeSpan tsSSE(0, 0, 0, times[1]);
   wxTimeSpan tsDefault(0, 0, 0, times[2]);
   wxMessageBox(wxString::Format(_("Benchmark times:\nDefault: %s\nSSE: %s\nSSE Threaded: %s\n"),tsDefault.Format(wxT("%M:%S.%l")).c_str(),tsSSE.Format(wxT("%M:%S.%l")).c_str(),tsSSEThreaded.Format(wxT("%M:%S.%l")).c_str()));
/*   wxTimeSpan tsSSEThreaded(0, 0, 0, times[0]);
   wxTimeSpan tsSSE(0, 0, 0, times[1]);
   wxTimeSpan tsDefault(0, 0, 0, times[2]);
   wxString outputString;
   outputString.Format(_("Benchmark times:\nDefault: %s\nSSE: %s\nSSE Threaded: %s\n"),tsDefault.Format(wxT("%M:%S.%l")),tsSSE.Format(wxT("%M:%S.%l")),tsSSEThreaded.Format(wxT("%M:%S.%l"))); 
   wxMessageBox(outputString); */ 


   return bGoodResult;
}


bool EffectEqualization48x::ProcessBuffer(fft_type *sourceBuffer, fft_type *destBuffer, sampleCount bufferLength)

{
   sampleCount blockCount=bufferLength/mBlockSize;
   sampleCount lastBlockSize=bufferLength%mBlockSize;
   if(lastBlockSize)
      blockCount++;

   float *workBuffer=&sourceBuffer[bufferLength];  // all scratch buffers are at the end

   for(int runx=0;runx<blockCount;runx++) 
   {
      float *currentBuffer=&workBuffer[mWindowSize*(runx&1)]; 
      for(int i=0;i<mBlockSize;i++)
         currentBuffer[i]=sourceBuffer[i];
      sourceBuffer+=mBlockSize;
      float *currentFilter=&currentBuffer[mBlockSize];
      for(int i=0;i<mFilterSize;i++)
         currentFilter[i]=0;
      mEffectEqualization->Filter(mWindowSize, currentBuffer);
      float *writeEnd=currentBuffer+mBlockSize;
      if(runx==blockCount) 
         writeEnd=currentBuffer+(lastBlockSize+mFilterSize);
      if(runx) {
         float *lastOverrun=&workBuffer[mWindowSize*((runx+1)&1)+mBlockSize]; 
         for(int j=0;j<mFilterSize;j++)
            *destBuffer++= *currentBuffer++ + *lastOverrun++;
      } else 
         currentBuffer+=mFilterSize>>1; // this will skip the first filterSize on the first run
      while(currentBuffer<writeEnd)
         *destBuffer++ = *currentBuffer++;
   }
   return true;
}


bool EffectEqualization48x::ProcessBuffer4x(BufferInfo *bufferInfo)
{
   // length must be a factor of window size for 4x processing. 
   if(bufferInfo->mBufferLength%mBlockSize)
      return false;

   sampleCount blockCount=bufferInfo->mBufferLength/mBlockSize;

   __m128 *readBlocks[4]; // some temps so we dont destroy the vars in the struct
   __m128 *writeBlocks[4];
   for(int i=0;i<4;i++) {
      readBlocks[i]=(__m128 *)bufferInfo->mBufferSouce[i];
      writeBlocks[i]=(__m128 *)bufferInfo->mBufferDest[i];
   }

   __m128 *swizzledBuffer128=(__m128 *)bufferInfo->mScratchBuffer;
   __m128 *scratchBuffer=&swizzledBuffer128[mWindowSize*2];

   for(int run4x=0;run4x<blockCount;run4x++) 
   {
      // swizzle the data to the swizzle buffer
      __m128 *currentSwizzledBlock=&swizzledBuffer128[mWindowSize*(run4x&1)]; 
      for(int i=0,j=0;j<mBlockSize;i++,j+=4) {
         __m128 tmp0   = _mm_shuffle_ps(readBlocks[0][i], readBlocks[1][i], _MM_SHUFFLE(1,0,1,0)); 
         __m128 tmp1   = _mm_shuffle_ps(readBlocks[0][i], readBlocks[1][i], _MM_SHUFFLE(3,2,3,2)); 
         __m128 tmp2   = _mm_shuffle_ps(readBlocks[2][i], readBlocks[3][i], _MM_SHUFFLE(1,0,1,0)); 
         __m128 tmp3   = _mm_shuffle_ps(readBlocks[2][i], readBlocks[3][i], _MM_SHUFFLE(3,2,3,2)); 
         currentSwizzledBlock[j]   = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(2,0,2,0)); 
         currentSwizzledBlock[j+1] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(3,1,3,1)); 
         currentSwizzledBlock[j+2] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(2,0,2,0)); 
         currentSwizzledBlock[j+3] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(3,1,3,1)); 
      }
      __m128 *thisOverrun128=&currentSwizzledBlock[mBlockSize]; 
      for(int i=0;i<mFilterSize;i++)
         thisOverrun128[i]=_mm_set1_ps(0.0);
      Filter4x(mWindowSize, (float *)currentSwizzledBlock, (float *)scratchBuffer);
      int writeStart=0, writeToStart=0; // note readStart is where the read data is written
      int writeEnd=mBlockSize;
      if(run4x) {
         // maybe later swizzle add and write in one
         __m128 *lastOverrun128=&swizzledBuffer128[mWindowSize*((run4x+1)&1)+mBlockSize]; 
         // add and swizzle data + filter
         for(int i=0,j=0;j<mFilterSize;i++,j+=4) {
            __m128 tmps0 = _mm_add_ps(currentSwizzledBlock[j], lastOverrun128[j]);
            __m128 tmps1 = _mm_add_ps(currentSwizzledBlock[j+1], lastOverrun128[j+1]);
            __m128 tmps2 = _mm_add_ps(currentSwizzledBlock[j+2], lastOverrun128[j+2]);
            __m128 tmps3 = _mm_add_ps(currentSwizzledBlock[j+3], lastOverrun128[j+3]);
            __m128 tmp0   = _mm_shuffle_ps(tmps1, tmps0, _MM_SHUFFLE(0,1,0,1)); 
            __m128 tmp1   = _mm_shuffle_ps(tmps1, tmps0, _MM_SHUFFLE(2,3,2,3)); 
            __m128 tmp2   = _mm_shuffle_ps(tmps3, tmps2, _MM_SHUFFLE(0,1,0,1)); 
            __m128 tmp3   = _mm_shuffle_ps(tmps3, tmps2, _MM_SHUFFLE(2,3,2,3)); 
            writeBlocks[0][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[1][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(0,2,0,2)); 
            writeBlocks[2][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[3][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(0,2,0,2)); 
         } 
         writeStart=mFilterSize;
         writeToStart=mFilterSize>>2;
         // swizzle it back. 
         for(int i=writeToStart,j=writeStart;j<writeEnd;i++,j+=4) {
            __m128 tmp0   = _mm_shuffle_ps(currentSwizzledBlock[j+1], currentSwizzledBlock[j], _MM_SHUFFLE(0,1,0,1)); 
            __m128 tmp1   = _mm_shuffle_ps(currentSwizzledBlock[j+1], currentSwizzledBlock[j], _MM_SHUFFLE(2,3,2,3)); 
            __m128 tmp2   = _mm_shuffle_ps(currentSwizzledBlock[j+3], currentSwizzledBlock[j+2], _MM_SHUFFLE(0,1,0,1)); 
            __m128 tmp3   = _mm_shuffle_ps(currentSwizzledBlock[j+3], currentSwizzledBlock[j+2], _MM_SHUFFLE(2,3,2,3)); 
            writeBlocks[0][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[1][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(0,2,0,2)); 
            writeBlocks[2][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[3][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(0,2,0,2)); 
         }
      } else {
         // swizzle it back. We overlap one block so we only write the first block on the first run
         writeStart=0;
         writeToStart=0;
         for(int i=writeToStart,j=writeStart;j<writeEnd;i++,j+=4) {
            __m128 tmp0   = _mm_shuffle_ps(currentSwizzledBlock[j+1], currentSwizzledBlock[j], _MM_SHUFFLE(0,1,0,1)); 
            __m128 tmp2   = _mm_shuffle_ps(currentSwizzledBlock[j+3], currentSwizzledBlock[j+2], _MM_SHUFFLE(0,1,0,1)); 
            writeBlocks[0][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(1,3,1,3)); 
         }
      }
      for(int i=0;i<4;i++) { // shift each block
         readBlocks[i]+=mBlockSize>>2; // these are 128b pointers, each window is 1/4 blockSize for those
         writeBlocks[i]+=mBlockSize>>2; 
      }
   }
   return true;
}

bool EffectEqualization48x::ProcessOne4x(int count, WaveTrack * t,
                                         sampleCount start, sampleCount len)
{
   sampleCount blockCount=len/mBlockSize;

   if(blockCount<16) // it's not worth 4x processing do a regular process
      return mEffectEqualization->ProcessOne(count, t, start, len);

   sampleCount trackBlockSize = t->GetMaxBlockSize();

   AudacityProject *p = GetActiveProject();
   WaveTrack *output=p->GetTrackFactory()->NewWaveTrack(floatSample, t->GetRate());

   mEffectEqualization->TrackProgress(count, 0.0);
   int bigRuns=len/(mSubBufferSize-mBlockSize);
   int trackBlocksPerBig=mSubBufferSize/trackBlockSize;
   int trackLeftovers=mSubBufferSize-trackBlocksPerBig*trackBlockSize;
   int singleProcessLength=(mFilterSize>>1)*bigRuns + len%(bigRuns*(mSubBufferSize-mBlockSize));
   sampleCount currentSample=start;

   for(int bigRun=0;bigRun<bigRuns;bigRun++)
   {
      // fill the buffer
      for(int i=0;i<trackBlocksPerBig;i++) {
         t->Get((samplePtr)&mBigBuffer[i*trackBlockSize], floatSample, currentSample, trackBlockSize);
         currentSample+=trackBlockSize;
      }
      if(trackLeftovers) {
         t->Get((samplePtr)&mBigBuffer[trackBlocksPerBig*trackBlockSize], floatSample, currentSample, trackLeftovers);
         currentSample+=trackLeftovers;
      }
      currentSample-=mBlockSize+(mFilterSize>>1);

      ProcessBuffer4x(mBufferInfo);
      if (mEffectEqualization->TrackProgress(count, (double)(bigRun)/(double)bigRuns))
      {
         break;
      }
      output->Append((samplePtr)&mBigBuffer[(bigRun?mBlockSize:0)+(mFilterSize>>1)], floatSample, mSubBufferSize-((bigRun?mBlockSize:0)+(mFilterSize>>1)));
   }
   if(singleProcessLength) {
      t->Get((samplePtr)mBigBuffer, floatSample, currentSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
      ProcessBuffer(mBigBuffer, mBigBuffer, singleProcessLength+mBlockSize+(mFilterSize>>1));
      output->Append((samplePtr)&mBigBuffer[mBlockSize], floatSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
   }

   output->Flush();
   ProcessTail(t, output, start, len);
   delete output;
   return true;
}

void *EQWorker::Entry()
{
   while(!mExitLoop) {
      mMutex->Lock();
      bool bufferAquired=false;
      for(int i=0;i<mBufferInfoCount;i++)
         if(mBufferInfoList[i].mBufferStatus==BufferReady) { // we found an unlocked ready buffer
            bufferAquired=true;
            mBufferInfoList[i].mBufferStatus=BufferBusy; // we own it now
            mMutex->Unlock();
            mEffectEqualization48x->ProcessBuffer4x(&mBufferInfoList[i]);
            mBufferInfoList[i].mBufferStatus=BufferDone; // we're done
            break;
         } 
         if(!bufferAquired)
            mMutex->Unlock();
   }
   return NULL;
}

bool EffectEqualization48x::ProcessOne4xThreaded(int count, WaveTrack * t,
                                                 sampleCount start, sampleCount len)
{
   sampleCount blockCount=len/mBlockSize;

   if(blockCount<16) // it's not worth 4x processing do a regular process
      return ProcessOne4x(count, t, start, len);
   if(mThreadCount<=0 || blockCount<256) // dont do it without cores or big data
      return ProcessOne4x(count, t, start, len);

   AudacityProject *p = GetActiveProject();
   WaveTrack *output=p->GetTrackFactory()->NewWaveTrack(floatSample, t->GetRate());

   sampleCount trackBlockSize = t->GetMaxBlockSize();
   mEffectEqualization->TrackProgress(count, 0.0);
   int bigRuns=len/(mSubBufferSize-mBlockSize);
   int trackBlocksPerBig=mSubBufferSize/trackBlockSize;
   int trackLeftovers=mSubBufferSize-trackBlocksPerBig*trackBlockSize;
   int singleProcessLength=(mFilterSize>>1)*bigRuns + len%(bigRuns*(mSubBufferSize-mBlockSize));
   sampleCount currentSample=start;

   int bigBlocksRead=mWorkerDataCount, bigBlocksWritten=0;

   // fill the first workerDataCount buffers we checked above and there is at least this data
   for(int i=0;i<mWorkerDataCount;i++)
   {
      // fill the buffer
      for(int j=0;j<trackBlocksPerBig;j++) {
         t->Get((samplePtr)&mBufferInfo[i].mBufferSouce[0][j*trackBlockSize], floatSample, currentSample, trackBlockSize);
         currentSample+=trackBlockSize;
      }
      if(trackLeftovers) {
         t->Get((samplePtr)&mBufferInfo[i].mBufferSouce[0][trackBlocksPerBig*trackBlockSize], floatSample, currentSample, trackLeftovers);
         currentSample+=trackLeftovers;
      }
      currentSample-=mBlockSize+(mFilterSize>>1);
      mBufferInfo[i].mBufferStatus=BufferReady; // free for grabbin
   }
   int currentIndex=0;
   while(bigBlocksWritten<bigRuns) {
      mDataMutex.Lock(); // Get in line for data
      // process as many blocks as we can
      while((mBufferInfo[currentIndex].mBufferStatus==BufferDone) && (bigBlocksWritten<bigRuns)) { // data is ours
         if (mEffectEqualization->TrackProgress(count, (double)(bigBlocksWritten)/(double)bigRuns))
         {
            break;
         }
         output->Append((samplePtr)&mBufferInfo[currentIndex].mBufferDest[0][(bigBlocksWritten?mBlockSize:0)+(mFilterSize>>1)], floatSample, mSubBufferSize-((bigBlocksWritten?mBlockSize:0)+(mFilterSize>>1)));
         bigBlocksWritten++;
         if(bigBlocksRead<bigRuns) {
            // fill the buffer
            for(int j=0;j<trackBlocksPerBig;j++) {
               t->Get((samplePtr)&mBufferInfo[currentIndex].mBufferSouce[0][j*trackBlockSize], floatSample, currentSample, trackBlockSize);
               currentSample+=trackBlockSize;
            }
            if(trackLeftovers) {
               t->Get((samplePtr)&mBufferInfo[currentIndex].mBufferSouce[0][trackBlocksPerBig*trackBlockSize], floatSample, currentSample, trackLeftovers);
               currentSample+=trackLeftovers;
            }
            currentSample-=mBlockSize+(mFilterSize>>1);
            mBufferInfo[currentIndex].mBufferStatus=BufferReady; // free for grabbin
            bigBlocksRead++;
         } else mBufferInfo[currentIndex].mBufferStatus=BufferEmpty; // this is completely unecessary
         currentIndex=(currentIndex+1)%mWorkerDataCount;
      } 
      mDataMutex.Unlock(); // Get back in line for data
   }
   if(singleProcessLength) {
      t->Get((samplePtr)mBigBuffer, floatSample, currentSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
      ProcessBuffer(mBigBuffer, mBigBuffer, singleProcessLength+mBlockSize+(mFilterSize>>1));
      output->Append((samplePtr)&mBigBuffer[mBlockSize], floatSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
   }
   output->Flush();
   ProcessTail(t, output, start, len);
   delete output;
   return true;
}

bool EffectEqualization48x::ProcessTail(WaveTrack * t, WaveTrack * output, sampleCount start, sampleCount len)
{
   //	  double offsetT0 = t->LongSamplesToTime((sampleCount)offset);
   double lenT = t->LongSamplesToTime(len);
   // 'start' is the sample offset in 't', the passed in track
   // 'startT' is the equivalent time value
   // 'output' starts at zero
   double startT = t->LongSamplesToTime(start);

   //output has one waveclip for the total length, even though 
   //t might have whitespace seperating multiple clips
   //we want to maintain the original clip structure, so
   //only paste the intersections of the new clip.

   //Find the bits of clips that need replacing
   std::vector<std::pair<double, double> > clipStartEndTimes;
   std::vector<std::pair<double, double> > clipRealStartEndTimes; //the above may be truncated due to a clip being partially selected
   for (WaveClipList::compatibility_iterator it=t->GetClipIterator(); it; it=it->GetNext())
   {
      WaveClip *clip;
      double clipStartT;
      double clipEndT;

      clip = it->GetData();
      clipStartT = clip->GetStartTime();
      clipEndT = clip->GetEndTime();
      if( clipEndT <= startT )
         continue;   // clip is not within selection
      if( clipStartT >= startT + lenT )
         continue;   // clip is not within selection

      //save the actual clip start/end so that we can rejoin them after we paste.
      clipRealStartEndTimes.push_back(std::pair<double,double>(clipStartT,clipEndT));            

      if( clipStartT < startT )  // does selection cover the whole clip?
         clipStartT = startT; // don't copy all the new clip
      if( clipEndT > startT + lenT )  // does selection cover the whole clip?
         clipEndT = startT + lenT; // don't copy all the new clip

      //save them
      clipStartEndTimes.push_back(std::pair<double,double>(clipStartT,clipEndT));
   }
   //now go thru and replace the old clips with new
   for(unsigned int i=0;i<clipStartEndTimes.size();i++)
   {
      Track *toClipOutput;
      //remove the old audio and get the new
      t->Clear(clipStartEndTimes[i].first,clipStartEndTimes[i].second);
      //         output->Copy(clipStartEndTimes[i].first-startT+offsetT0,clipStartEndTimes[i].second-startT+offsetT0, &toClipOutput);   
      output->Copy(clipStartEndTimes[i].first-startT,clipStartEndTimes[i].second-startT, &toClipOutput);   
      if(toClipOutput)
      {
         //put the processed audio in
         bool bResult = t->Paste(clipStartEndTimes[i].first, toClipOutput);
         wxASSERT(bResult); // TO DO: Actually handle this.
         //if the clip was only partially selected, the Paste will have created a split line.  Join is needed to take care of this
         //This is not true when the selection is fully contained within one clip (second half of conditional)
         if( (clipRealStartEndTimes[i].first  != clipStartEndTimes[i].first || 
            clipRealStartEndTimes[i].second != clipStartEndTimes[i].second) &&
            !(clipRealStartEndTimes[i].first <= startT &&  
            clipRealStartEndTimes[i].second >= startT+lenT) )
            t->Join(clipRealStartEndTimes[i].first,clipRealStartEndTimes[i].second);
         delete toClipOutput;
      }
   }
   return true;
}




void EffectEqualization48x::Filter4x(sampleCount len,
                                     float *buffer, float *scratchBuffer)
{
   int i;
   __m128 real128, imag128;
   // Apply FFT
   RealFFTf4x(buffer, mEffectEqualization->hFFT);

   // Apply filter
   // DC component is purely real
   __m128 *localFFTBuffer=(__m128 *)scratchBuffer;
   __m128 *localBuffer=(__m128 *)buffer;

   __m128 filterFuncR, filterFuncI;
   filterFuncR=_mm_set1_ps(mEffectEqualization->mFilterFuncR[0]);
   localFFTBuffer[0]=_mm_mul_ps(localBuffer[0], filterFuncR); 
   int halfLength=(len/2);

   bool useBitReverseTable=sMathPath&1;

   for(i=1; i<halfLength; i++)
   {
      if(useBitReverseTable) {
         real128=localBuffer[mEffectEqualization->hFFT->BitReversed[i]  ];
         imag128=localBuffer[mEffectEqualization->hFFT->BitReversed[i]+1];
      } else {
         int bitReversed=SmallReverseBits(i,mEffectEqualization->hFFT->pow2Bits);
         real128=localBuffer[bitReversed];
         imag128=localBuffer[bitReversed+1];
      }
      filterFuncR=_mm_set1_ps(mEffectEqualization->mFilterFuncR[i]);
      filterFuncI=_mm_set1_ps(mEffectEqualization->mFilterFuncI[i]);
      localFFTBuffer[2*i  ] = _mm_sub_ps( _mm_mul_ps(real128, filterFuncR), _mm_mul_ps(imag128, filterFuncI));
      localFFTBuffer[2*i+1] = _mm_add_ps( _mm_mul_ps(real128, filterFuncI), _mm_mul_ps(imag128, filterFuncR));
   }
   // Fs/2 component is purely real
   filterFuncR=_mm_set1_ps(mEffectEqualization->mFilterFuncR[halfLength]);
   localFFTBuffer[1] = _mm_mul_ps(localBuffer[1], filterFuncR);

   // Inverse FFT and normalization
   InverseRealFFTf4x(scratchBuffer, mEffectEqualization->hFFT);
   ReorderToTime4x(mEffectEqualization->hFFT, scratchBuffer, buffer);
}

#endif
