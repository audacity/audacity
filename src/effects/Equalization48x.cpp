/**********************************************************************

   Audacity: A Digital Audio Editor

   EffectEqualization.cpp

   Andrew Hallendorff

*******************************************************************//**

   \file Equalization48x.cpp
   \brief Fast SSE based implementation of equalization.

*//****************************************************************/


#include "Equalization48x.h"

#ifdef EXPERIMENTAL_EQ_SSE_THREADED
#include "../Project.h"
#include "Equalization.h"
#include "../WaveClip.h"
#include "../WaveTrack.h"
#include "../float_cast.h"
#include <vector>

#include <wx/setup.h> // for wxUSE_* macros

#include <wx/event.h>
#include <wx/string.h>

#if wxUSE_TOOLTIPS
#include <wx/tooltip.h>
#endif
#include <wx/utils.h>

#include <math.h>

#include "../RealFFTf48x.h"

#ifndef USE_SSE2
#define	USE_SSE2
#endif

#include <stdlib.h>

#ifdef __WXMSW__
#include <malloc.h>
#endif

#include <stdio.h>
#include <math.h>
#include <emmintrin.h>

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

void free_simd::operator() (void* mem) const
{
#if defined WIN32           // WIN32
    _aligned_free(mem);
#else  
    free(mem);
#endif
}

EffectEqualization48x::EffectEqualization48x():
         mThreadCount(0),mFilterSize(0),mWindowSize(0),mBlockSize(0),mWorkerDataCount(0),mBlocksPerBuffer(20),
         mScratchBufferSize(0),mSubBufferSize(0),mThreaded(false),
         mBenching(false),mBufferCount(0)
{
}

EffectEqualization48x::~EffectEqualization48x()
{
}

bool EffectEqualization48x::AllocateBuffersWorkers(int nThreads)
{
   if(mBigBuffer)
      FreeBuffersWorkers(); 
   mFilterSize=(mEffectEqualization->mM-1)&(~15); // 4000 !!! Filter MUST BE QUAD WORD ALIGNED !!!!
   mWindowSize=mEffectEqualization->windowSize;
   wxASSERT(mFilterSize < mWindowSize);
   mBlockSize=mWindowSize-mFilterSize; // 12,384
   auto threadCount = wxThread::GetCPUCount();
   mThreaded = (nThreads > 0 && threadCount > 0);
   if(mThreaded)
   {
      mThreadCount = threadCount;
      mWorkerDataCount=mThreadCount+2; // 2 extra slots (maybe double later)
   } else {
      mWorkerDataCount=1;
      mThreadCount=0;
   }
#ifdef __AVX_ENABLED
   mBufferCount=sMathPath&MATH_FUNCTION_AVX?8:4;
#else
   mBufferCount=4;
#endif
   // we're skewing the data by one block to allow for 1/4 block intersections.
   // this will remove the disparity in data at the intersections of the runs

   // The nice magic allocation
   // megabyte - 3 windows - 4 overlapping buffers - filter 
   // 2^20 = 1,048,576 - 3 * 2^14 (16,384) - ((4 * 20) - 3) * 12,384 - 4000 
   // 1,048,576 - 49,152 - 953,568 - 4000 = 41,856 (leftover)

   mScratchBufferSize=mWindowSize*3*sizeof(float)*mBufferCount; // 3 window size blocks of instruction size
   mSubBufferSize=mBlockSize*(mBufferCount*(mBlocksPerBuffer-1)); // we are going to do a full block overlap
   mBigBuffer.reset( (float *)malloc_simd(sizeof(float) * (mSubBufferSize + mFilterSize + mScratchBufferSize) * mWorkerDataCount) ); // we run over by filtersize
   // fill the bufferInfo
   mBufferInfo.reinit(mWorkerDataCount);
   for(int i=0;i<mWorkerDataCount;i++) {
      mBufferInfo[i].mFftWindowSize=mWindowSize;
      mBufferInfo[i].mFftFilterSize=mFilterSize;
      mBufferInfo[i].mBufferLength=mBlockSize*mBlocksPerBuffer;
      mBufferInfo[i].mContiguousBufferSize=mSubBufferSize;
      mBufferInfo[i].mScratchBuffer=&mBigBuffer[(mSubBufferSize+mScratchBufferSize)*i+mSubBufferSize];
      for(int j=0;j<mBufferCount;j++)
         mBufferInfo[i].mBufferDest[j]=mBufferInfo[i].mBufferSouce[j]=&mBigBuffer[j*(mBufferInfo[i].mBufferLength-mBlockSize)+(mSubBufferSize+mScratchBufferSize)*i];
   }
   if(mThreadCount) {
      // start the workers
      mDataMutex.IsOk();
      mEQWorkers.reinit(mThreadCount);
      for(int i=0;i<mThreadCount;i++) {
         mEQWorkers[i].SetData( mBufferInfo.get(), mWorkerDataCount, &mDataMutex, this);
         mEQWorkers[i].Create();
         mEQWorkers[i].Run();
      }
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
      mEQWorkers.reset(); // kill the workers ( go directly to jail)
      mThreadCount=0;
      mWorkerDataCount=0; 
   }
   mBufferInfo.reset();
   mBigBuffer.reset();
   return true;
}


#pragma warning(push)
// Disable the unreachable code warning in MSVC, for this function.
#pragma warning(disable: 4702)
bool EffectEqualization48x::RunFunctionSelect(int flags, int count, WaveTrack * track, sampleCount start, sampleCount len)
{
   // deal with tables here 
   flags&=~(MATH_FUNCTION_BITREVERSE_TABLE|MATH_FUNCTION_SIN_COS_TABLE); // clear out the table flags
   switch (flags)
   {
   case MATH_FUNCTION_SSE:
      return ProcessOne4x(count, track, start, len);
      break;
   case MATH_FUNCTION_SSE|MATH_FUNCTION_THREADED:
      return ProcessOne1x4xThreaded(count, track, start, len);
      break;
   case MATH_FUNCTION_THREADED:
   case MATH_FUNCTION_THREADED|MATH_FUNCTION_SEGMENTED_CODE:
      return ProcessOne1x4xThreaded(count, track, start, len, 1);
      break;
   case MATH_FUNCTION_SEGMENTED_CODE:
      return ProcessOne1x(count, track, start, len);
      break;
   default:
      return !mEffectEqualization->ProcessOne(count, track, start, len);
      break;
   }
   return false;
}
#pragma warning(pop)

bool EffectEqualization48x::Process(EffectEqualization* effectEqualization)
{
   mEffectEqualization=effectEqualization;
//   return TrackCompare(); // used for debugging data
   mEffectEqualization->CopyInputTracks(); // Set up mOutputTracks.
   bool bBreakLoop = false;

   TableUsage(sMathPath);
   if(sMathPath)  // !!! Filter MUST BE QUAD WORD ALIGNED !!!!
      mEffectEqualization->mM=(mEffectEqualization->mM&(~15))+1;
   AllocateBuffersWorkers(sMathPath&MATH_FUNCTION_THREADED);
   auto cleanup = finally( [&] { FreeBuffersWorkers(); } );
   int count = 0;
   for( auto track :
        mEffectEqualization->mOutputTracks->Selected< WaveTrack >() ) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mEffectEqualization->mT0 < trackStart? trackStart: mEffectEqualization->mT0;
      double t1 = mEffectEqualization->mT1 > trackEnd? trackEnd: mEffectEqualization->mT1;

      if (t1 > t0) {
         auto start = track->TimeToLongSamples(t0);
         auto end = track->TimeToLongSamples(t1);
         auto len = end - start;
         bBreakLoop=RunFunctionSelect(sMathPath, count, track, start, len);
         if( bBreakLoop )
            break;
      }
      count++;
   }

   mEffectEqualization->ReplaceProcessedTracks(!bBreakLoop); 
   return !bBreakLoop;
}

bool EffectEqualization48x::TrackCompare()
{
   mEffectEqualization->CopyInputTracks(); // Set up mOutputTracks.
   bool bBreakLoop = false;

   TableUsage(sMathPath);
   if(sMathPath)  // !!! Filter MUST BE QUAD WORD ALIGNED !!!!
      mEffectEqualization->mM=(mEffectEqualization->mM&(~15))+1;
   AllocateBuffersWorkers(sMathPath&MATH_FUNCTION_THREADED);
   auto cleanup = finally( [&] { FreeBuffersWorkers(); } );
   // Reset map
   // PRL:  These two maps aren't really used
   std::vector<const Track*> SecondIMap;
   std::vector<Track*> SecondOMap;
   SecondIMap.clear();
   SecondOMap.clear();
   
   auto pSecondOutputTracks = TrackList::Create( nullptr );
   auto &SecondOutputTracks = *pSecondOutputTracks;

   for (auto aTrack :
      mEffectEqualization->inputTracks()->Any< const WaveTrack >()) {

      // Include selected tracks, plus sync-lock selected tracks for Track::All.
      if (aTrack->GetSelected() ||
         (// mEffectEqualization->mOutputTracksType == TrackKind::All &&
          aTrack->IsSyncLockSelected()))
      {
         auto o = mEffectEqualization->mFactory->DuplicateWaveTrack( *aTrack );
         SecondIMap.push_back(aTrack);
         SecondIMap.push_back(o.get());
         SecondOutputTracks.Add( o );
      }
   }

   for(int i = 0; i < 2; i++) {
      i?sMathPath=sMathPath:sMathPath=0;
      int count = 0;
      for( auto track :
           ( i ? mEffectEqualization->mOutputTracks.get()
               : &SecondOutputTracks ) -> Selected< WaveTrack >() ) {
         double trackStart = track->GetStartTime();
         double trackEnd = track->GetEndTime();
         double t0 = mEffectEqualization->mT0 < trackStart? trackStart: mEffectEqualization->mT0;
         double t1 = mEffectEqualization->mT1 > trackEnd? trackEnd: mEffectEqualization->mT1;

         if (t1 > t0) {
            auto start = track->TimeToLongSamples(t0);
            auto end = track->TimeToLongSamples(t1);
            auto len = end - start;
            bBreakLoop=RunFunctionSelect(sMathPath, count, track, start, len);
            if( bBreakLoop )
               break;
         }
         count++;
      }
   }

   auto iter2 = (SecondOutputTracks.Selected< const WaveTrack >()).first;
   auto track2 = *iter2;
   for ( auto track :
         mEffectEqualization->mOutputTracks->Selected< WaveTrack >() ) {
      double trackStart = track->GetStartTime();
      double trackEnd = track->GetEndTime();
      double t0 = mEffectEqualization->mT0 < trackStart? trackStart: mEffectEqualization->mT0;
      double t1 = mEffectEqualization->mT1 > trackEnd? trackEnd: mEffectEqualization->mT1;

      if (t1 > t0) {
         auto start = track->TimeToLongSamples(t0);
         auto end = track->TimeToLongSamples(t1);
         auto len = end - start;
         DeltaTrack(track, track2, start, len);
      }
      track2 = * ++iter2;
   }
   mEffectEqualization->ReplaceProcessedTracks(!bBreakLoop);
   return bBreakLoop; // return !bBreakLoop ?
}

bool EffectEqualization48x::DeltaTrack(
   WaveTrack * t, const WaveTrack * t2, sampleCount start, sampleCount len)
{

   auto trackBlockSize = t->GetMaxBlockSize();

   Floats buffer1{ trackBlockSize };
   Floats buffer2{ trackBlockSize };

   auto output = t->EmptyCopy();
   t->ConvertToSampleFormat( floatSample );
   auto originalLen = len;
   auto currentSample = start;

   while(len > 0) {
      auto curretLength = limitSampleBufferSize(trackBlockSize, len);
      t->Get((samplePtr)buffer1.get(), floatSample, currentSample, curretLength);
      t2->Get((samplePtr)buffer2.get(), floatSample, currentSample, curretLength);
      for(decltype(curretLength) i=0;i<curretLength;i++)
         buffer1[i]-=buffer2[i];
      output->Append((samplePtr)buffer1.get(), floatSample, curretLength);
      currentSample+=curretLength;
      len-=curretLength;
   }
   output->Flush();
   len=originalLen;
   ProcessTail(t, output.get(), start, len);
   return true;
}

#include <wx/stopwatch.h>

bool EffectEqualization48x::Benchmark(EffectEqualization* effectEqualization)
{
   mEffectEqualization=effectEqualization;
   mEffectEqualization->CopyInputTracks(); // Set up mOutputTracks.
   bool bBreakLoop = false;

   TableUsage(sMathPath);
   if(sMathPath)  // !!! Filter MUST BE QUAD WORD ALIGNED !!!!
      mEffectEqualization->mM=(mEffectEqualization->mM&(~15))+1;
   AllocateBuffersWorkers(MATH_FUNCTION_THREADED);
   auto cleanup = finally( [&] { FreeBuffersWorkers(); } );
   long times[] = { 0,0,0,0,0 };
   wxStopWatch timer;
   mBenching = true;
   for(int i = 0; i < 5 && !bBreakLoop; i++) {
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
         case 2: localMathPath=MATH_FUNCTION_SEGMENTED_CODE;
            break;
         case 3: localMathPath=MATH_FUNCTION_THREADED|MATH_FUNCTION_SEGMENTED_CODE;
            break;
         case 4: localMathPath=0;
            break;
         default: localMathPath=-1;
      }
      if(localMathPath >= 0) {
         timer.Start();
         int count = 0;
         for (auto track :
              mEffectEqualization->mOutputTracks->Selected< WaveTrack >() ) {
            double trackStart = track->GetStartTime();
            double trackEnd = track->GetEndTime();
            double t0 = mEffectEqualization->mT0 < trackStart? trackStart: mEffectEqualization->mT0;
            double t1 = mEffectEqualization->mT1 > trackEnd? trackEnd: mEffectEqualization->mT1;

            if (t1 > t0) {
               auto start = track->TimeToLongSamples(t0);
               auto end = track->TimeToLongSamples(t1);
               auto len = end - start;
               bBreakLoop=RunFunctionSelect( localMathPath, count, track, start, len);
               if( bBreakLoop )
                  break;
            }
            count++;
         }
         times[i]=timer.Time();
      }
   }
   mBenching=false;
   bBreakLoop=false;
   mEffectEqualization->ReplaceProcessedTracks(bBreakLoop); 

   wxTimeSpan tsSSEThreaded(0, 0, 0, times[0]);
   wxTimeSpan tsSSE(0, 0, 0, times[1]);
   wxTimeSpan tsDefaultEnhanced(0, 0, 0, times[2]);
   wxTimeSpan tsDefaultThreaded(0, 0, 0, times[3]);
   wxTimeSpan tsDefault(0, 0, 0, times[4]);

   mEffectEqualization->MessageBox(
      XO(
"Benchmark times:\nOriginal: %s\nDefault Segmented: %s\nDefault Threaded: %s\nSSE: %s\nSSE Threaded: %s\n")
         .Format(
            tsDefault.Format(wxT("%M:%S.%l")),
            tsDefaultEnhanced.Format(wxT("%M:%S.%l")),
            tsDefaultThreaded.Format(wxT("%M:%S.%l")),
            tsSSE.Format(wxT("%M:%S.%l")),
            tsSSEThreaded.Format(wxT("%M:%S.%l")) ) );
   return bBreakLoop; // return !bBreakLoop ?
}

bool EffectEqualization48x::ProcessTail(WaveTrack * t, WaveTrack * output, sampleCount start, sampleCount len)
{
   //	  double offsetT0 = t->LongSamplesToTime(offset);
   double lenT = t->LongSamplesToTime(len);
   // 'start' is the sample offset in 't', the passed in track
   // 'startT' is the equivalent time value
   // 'output' starts at zero
   double startT = t->LongSamplesToTime(start);

   //output has one waveclip for the total length, even though 
   //t might have whitespace separating multiple clips
   //we want to maintain the original clip structure, so
   //only paste the intersections of the NEW clip.

   //Find the bits of clips that need replacing
   std::vector<std::pair<double, double> > clipStartEndTimes;
   std::vector<std::pair<double, double> > clipRealStartEndTimes; //the above may be truncated due to a clip being partially selected
   for (const auto &clip: t->GetClips())
   {
      double clipStartT;
      double clipEndT;

      clipStartT = clip->GetStartTime();
      clipEndT = clip->GetEndTime();
      if( clipEndT <= startT )
         continue;   // clip is not within selection
      if( clipStartT >= startT + lenT )
         continue;   // clip is not within selection

      //save the actual clip start/end so that we can rejoin them after we paste.
      clipRealStartEndTimes.push_back(std::pair<double,double>(clipStartT,clipEndT));            

      if( clipStartT < startT )  // does selection cover the whole clip?
         clipStartT = startT; // don't copy all the NEW clip
      if( clipEndT > startT + lenT )  // does selection cover the whole clip?
         clipEndT = startT + lenT; // don't copy all the NEW clip

      //save them
      clipStartEndTimes.push_back(std::pair<double,double>(clipStartT,clipEndT));
   }
   //now go thru and replace the old clips with NEW
   for(unsigned int i=0;i<clipStartEndTimes.size();i++)
   {
      //remove the old audio and get the NEW
      t->Clear(clipStartEndTimes[i].first,clipStartEndTimes[i].second);
      //         output->Copy(clipStartEndTimes[i].first-startT+offsetT0,clipStartEndTimes[i].second-startT+offsetT0, &toClipOutput);
      auto toClipOutput = output->Copy(clipStartEndTimes[i].first-startT, clipStartEndTimes[i].second-startT);
      //put the processed audio in
      t->Paste(clipStartEndTimes[i].first, toClipOutput.get());
      //if the clip was only partially selected, the Paste will have created a split line.  Join is needed to take care of this
      //This is not true when the selection is fully contained within one clip (second half of conditional)
      if( (clipRealStartEndTimes[i].first  != clipStartEndTimes[i].first || 
         clipRealStartEndTimes[i].second != clipStartEndTimes[i].second) &&
         !(clipRealStartEndTimes[i].first <= startT &&  
         clipRealStartEndTimes[i].second >= startT+lenT) )
         t->Join(clipRealStartEndTimes[i].first,clipRealStartEndTimes[i].second);
   }
   return true;
}

bool EffectEqualization48x::ProcessBuffer(fft_type *sourceBuffer, fft_type *destBuffer, size_t bufferLength)
{
   BufferInfo bufferInfo;
   bufferInfo.mContiguousBufferSize=bufferLength;
   bufferInfo.mBufferSouce[0]=sourceBuffer;
   bufferInfo.mBufferDest[0]=destBuffer;
   bufferInfo.mScratchBuffer=&sourceBuffer[mSubBufferSize];
   return ProcessBuffer1x(&bufferInfo);
}

bool EffectEqualization48x::ProcessBuffer1x(BufferInfo *bufferInfo)
{
   int bufferCount=bufferInfo->mContiguousBufferSize?1:4;
   for(int bufferIndex=0;bufferIndex<bufferCount;bufferIndex++)
   {
      auto bufferLength=bufferInfo->mBufferLength;
      if(bufferInfo->mContiguousBufferSize)
         bufferLength=bufferInfo->mContiguousBufferSize;

      auto blockCount=bufferLength/mBlockSize;
      auto lastBlockSize=bufferLength%mBlockSize;
      if(lastBlockSize)
         blockCount++;

      float *workBuffer=bufferInfo->mScratchBuffer;  // all scratch buffers are at the end
      float *scratchBuffer=&workBuffer[mWindowSize*2];  // all scratch buffers are at the end
      float *sourceBuffer=bufferInfo->mBufferSouce[bufferIndex];
      float *destBuffer=bufferInfo->mBufferDest[bufferIndex];
      for(size_t runx=0;runx<blockCount;runx++)
      {
         float *currentBuffer=&workBuffer[mWindowSize*(runx&1)]; 
         for(int i=0;i<mBlockSize;i++)
            currentBuffer[i]=sourceBuffer[i];
         sourceBuffer+=mBlockSize;
         float *currentFilter=&currentBuffer[mBlockSize];
         for(int i=0;i<mFilterSize;i++)
            currentFilter[i]=0;
//         mEffectEqualization->Filter(mWindowSize, currentBuffer);
         Filter1x(mWindowSize, currentBuffer, scratchBuffer);
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
   }
   return true;
}

bool EffectEqualization48x::ProcessOne1x(int count, WaveTrack * t,
                                         sampleCount start, sampleCount len)
{
   //sampleCount blockCount=len/mBlockSize;

   auto trackBlockSize = t->GetMaxBlockSize();

   auto output = t->EmptyCopy();
   t->ConvertToSampleFormat( floatSample );

   mEffectEqualization->TrackProgress(count, 0.0);
   int subBufferSize=mBufferCount==8?(mSubBufferSize>>1):mSubBufferSize; // half the buffers if avx is active
   auto bigRuns=len/(subBufferSize-mBlockSize);
   int trackBlocksPerBig=subBufferSize/trackBlockSize;
   int trackLeftovers=subBufferSize-trackBlocksPerBig*trackBlockSize;
   size_t singleProcessLength;
   if(bigRuns == 0)
      singleProcessLength = len.as_size_t();
   else 
      singleProcessLength =
         ((mFilterSize>>1)*bigRuns + len%(bigRuns*(subBufferSize-mBlockSize)))
            .as_size_t();
   auto currentSample=start;
   bool bBreakLoop = false;
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

      ProcessBuffer1x(mBufferInfo.get());
      bBreakLoop=mEffectEqualization->TrackProgress(count, (double)(bigRun)/bigRuns.as_double());
      if( bBreakLoop )
         break;
      output->Append((samplePtr)&mBigBuffer[(bigRun?mBlockSize:0)+(mFilterSize>>1)], floatSample, subBufferSize-((bigRun?mBlockSize:0)+(mFilterSize>>1)));
   }
   if(singleProcessLength && !bBreakLoop) {
      t->Get((samplePtr)mBigBuffer.get(), floatSample, currentSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
      ProcessBuffer(mBigBuffer.get(), mBigBuffer.get(), singleProcessLength+mBlockSize+(mFilterSize>>1));
      output->Append((samplePtr)&mBigBuffer[bigRuns > 0 ? mBlockSize : 0], floatSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
   }
   output->Flush();
   if(!bBreakLoop)
      ProcessTail(t, output.get(), start, len);
   return bBreakLoop;
}

void EffectEqualization48x::Filter1x(size_t len,
                                     float *buffer, float *scratchBuffer)
{
   int i;
   float real, imag;
   // Apply FFT
   RealFFTf1x(buffer, mEffectEqualization->hFFT.get());

   // Apply filter
   // DC component is purely real

   float filterFuncR, filterFuncI;
   filterFuncR = mEffectEqualization->mFilterFuncR[0];
   scratchBuffer[0] = buffer[0] * filterFuncR;
   auto halfLength = (len / 2);

   bool useBitReverseTable=sMathPath&1;

   for(i = 1; i < halfLength; i++)
   {
      if(useBitReverseTable) {
         real=buffer[mEffectEqualization->hFFT->BitReversed[i]  ];
         imag=buffer[mEffectEqualization->hFFT->BitReversed[i]+1];
      } else {
         int bitReversed=SmallRB(i,mEffectEqualization->hFFT->pow2Bits);
         real=buffer[bitReversed];
         imag=buffer[bitReversed+1];
      }
      filterFuncR=mEffectEqualization->mFilterFuncR[i];
      filterFuncI=mEffectEqualization->mFilterFuncI[i];

      scratchBuffer[2*i  ] = real*filterFuncR - imag*filterFuncI;
      scratchBuffer[2*i+1] = real*filterFuncI + imag*filterFuncR;
   }
   // Fs/2 component is purely real
   filterFuncR=mEffectEqualization->mFilterFuncR[halfLength];
   scratchBuffer[1] = buffer[1] * filterFuncR;

   // Inverse FFT and normalization
   InverseRealFFTf1x(scratchBuffer, mEffectEqualization->hFFT.get());
   ReorderToTime1x(mEffectEqualization->hFFT.get(), scratchBuffer, buffer);
}

bool EffectEqualization48x::ProcessBuffer4x(BufferInfo *bufferInfo)
{
   // length must be a factor of window size for 4x processing. 
   if(bufferInfo->mBufferLength%mBlockSize)
      return false;

   auto blockCount=bufferInfo->mBufferLength/mBlockSize;

   __m128 *readBlocks[4]; // some temps so we dont destroy the vars in the struct
   __m128 *writeBlocks[4];
   for(int i=0;i<4;i++) {
      readBlocks[i]=(__m128 *)bufferInfo->mBufferSouce[i];
      writeBlocks[i]=(__m128 *)bufferInfo->mBufferDest[i];
   }

   __m128 *swizzledBuffer128=(__m128 *)bufferInfo->mScratchBuffer;
   __m128 *scratchBuffer=&swizzledBuffer128[mWindowSize*2];

   for(size_t run4x=0;run4x<blockCount;run4x++)
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
   int subBufferSize=mBufferCount==8?(mSubBufferSize>>1):mSubBufferSize; // half the buffers if avx is active

   if(len<subBufferSize) // it's not worth 4x processing do a regular process
      return ProcessOne1x(count, t, start, len);

   auto trackBlockSize = t->GetMaxBlockSize();

   auto output = t->EmptyCopy();
   t->ConvertToSampleFormat( floatSample );

   mEffectEqualization->TrackProgress(count, 0.0);
   auto bigRuns = len/(subBufferSize-mBlockSize);
   int trackBlocksPerBig=subBufferSize/trackBlockSize;
   int trackLeftovers=subBufferSize-trackBlocksPerBig*trackBlockSize;
   size_t singleProcessLength =
      ((mFilterSize>>1)*bigRuns + len%(bigRuns*(subBufferSize-mBlockSize)))
         .as_size_t();
   auto currentSample=start;

   bool bBreakLoop = false;
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

      ProcessBuffer4x(mBufferInfo.get());
      bBreakLoop=mEffectEqualization->TrackProgress(count, (double)(bigRun)/bigRuns.as_double());
      if( bBreakLoop )
         break;
      output->Append((samplePtr)&mBigBuffer[(bigRun?mBlockSize:0)+(mFilterSize>>1)], floatSample, subBufferSize-((bigRun?mBlockSize:0)+(mFilterSize>>1)));
   }
   if(singleProcessLength && !bBreakLoop) {
      t->Get((samplePtr)mBigBuffer.get(), floatSample, currentSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
      ProcessBuffer(mBigBuffer.get(), mBigBuffer.get(), singleProcessLength+mBlockSize+(mFilterSize>>1));
      output->Append((samplePtr)&mBigBuffer[bigRuns > 0 ? mBlockSize : 0], floatSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
//      output->Append((samplePtr)&mBigBuffer[bigRuns?mBlockSize:0], floatSample, singleProcessLength);
   }
   output->Flush();
   if(!bBreakLoop)
      ProcessTail(t, output.get(), start, len);
   return bBreakLoop;
}

#include <wx/thread.h>

void *EQWorker::Entry()
{
   while(!mExitLoop) {
      int i = 0;
      {
         wxMutexLocker locker( *mMutex );
         for(; i < mBufferInfoCount; i++) {
            if(mBufferInfoList[i].mBufferStatus==BufferReady) { // we found an unlocked ready buffer
               mBufferInfoList[i].mBufferStatus=BufferBusy; // we own it now
               break;
            }
         }
      }
      if ( i < mBufferInfoCount ) {
         switch (mProcessingType)
         {
            case 1:
               mEffectEqualization48x->ProcessBuffer1x(&mBufferInfoList[i]);
               break;
            case 4:
               mEffectEqualization48x->ProcessBuffer4x(&mBufferInfoList[i]);
               break;
         }
         mBufferInfoList[i].mBufferStatus=BufferDone; // we're done
      }
   }
   return NULL;
}

bool EffectEqualization48x::ProcessOne1x4xThreaded(int count, WaveTrack * t,
                                                 sampleCount start, sampleCount len, int processingType)
{
   int subBufferSize=mBufferCount==8?(mSubBufferSize>>1):mSubBufferSize; // half the buffers if avx is active

   sampleCount blockCount=len/mBlockSize;

   if(blockCount<16) // it's not worth 4x processing do a regular process
      return ProcessOne4x(count, t, start, len);
   if(mThreadCount<=0 || blockCount<256) // dont do it without cores or big data
      return ProcessOne4x(count, t, start, len);

   for(int i=0;i<mThreadCount;i++)
      mEQWorkers[i].mProcessingType=processingType;

   auto output = t->EmptyCopy();
   t->ConvertToSampleFormat( floatSample );

   auto trackBlockSize = t->GetMaxBlockSize();
   mEffectEqualization->TrackProgress(count, 0.0);
   auto bigRuns = len/(subBufferSize-mBlockSize);
   int trackBlocksPerBig=subBufferSize/trackBlockSize;
   int trackLeftovers=subBufferSize-trackBlocksPerBig*trackBlockSize;
   size_t singleProcessLength =
      ((mFilterSize>>1)*bigRuns + len%(bigRuns*(subBufferSize-mBlockSize)))
         .as_size_t();
   auto currentSample=start;

   int bigBlocksRead=mWorkerDataCount, bigBlocksWritten=0;

   // fill the first workerDataCount buffers we checked above and there is at least this data
   auto maxPreFill = bigRuns < mWorkerDataCount ? bigRuns : mWorkerDataCount;
   for(int i=0;i<maxPreFill;i++)
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
   bool bBreakLoop = false;
   while(bigBlocksWritten<bigRuns && !bBreakLoop) {
      bBreakLoop=mEffectEqualization->TrackProgress(count, (double)(bigBlocksWritten)/bigRuns.as_double());
      if( bBreakLoop )
         break;
      wxMutexLocker locker( mDataMutex ); // Get in line for data
      // process as many blocks as we can
      while((mBufferInfo[currentIndex].mBufferStatus==BufferDone) && (bigBlocksWritten<bigRuns)) { // data is ours
         output->Append((samplePtr)&mBufferInfo[currentIndex].mBufferDest[0][(bigBlocksWritten?mBlockSize:0)+(mFilterSize>>1)], floatSample, subBufferSize-((bigBlocksWritten?mBlockSize:0)+(mFilterSize>>1)));
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
         } else mBufferInfo[currentIndex].mBufferStatus=BufferEmpty; // this is completely unnecessary
         currentIndex=(currentIndex+1)%mWorkerDataCount;
      } 
   }
   if(singleProcessLength && !bBreakLoop) {
      t->Get((samplePtr)mBigBuffer.get(), floatSample, currentSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
      ProcessBuffer(mBigBuffer.get(), mBigBuffer.get(), singleProcessLength+mBlockSize+(mFilterSize>>1));
      output->Append((samplePtr)&mBigBuffer[mBlockSize], floatSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
   }
   output->Flush();
   if(!bBreakLoop) 
      ProcessTail(t, output.get(), start, len);
   return bBreakLoop;
}

void EffectEqualization48x::Filter4x(size_t len,
                                     float *buffer, float *scratchBuffer)
{
   int i;
   __m128 real128, imag128;
   // Apply FFT
   RealFFTf4x(buffer, mEffectEqualization->hFFT.get());

   // Apply filter
   // DC component is purely real
   __m128 *localFFTBuffer=(__m128 *)scratchBuffer;
   __m128 *localBuffer=(__m128 *)buffer;

   __m128 filterFuncR, filterFuncI;
   filterFuncR = _mm_set1_ps(mEffectEqualization->mFilterFuncR[0]);
   localFFTBuffer[0] = _mm_mul_ps(localBuffer[0], filterFuncR);
   auto halfLength = (len / 2);

   bool useBitReverseTable = sMathPath & 1;

   for(i = 1; i < halfLength; i++)
   {
      if(useBitReverseTable) {
         real128=localBuffer[mEffectEqualization->hFFT->BitReversed[i]  ];
         imag128=localBuffer[mEffectEqualization->hFFT->BitReversed[i]+1];
      } else {
         int bitReversed=SmallRB(i,mEffectEqualization->hFFT->pow2Bits);
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
   InverseRealFFTf4x(scratchBuffer, mEffectEqualization->hFFT.get());
   ReorderToTime4x(mEffectEqualization->hFFT.get(), scratchBuffer, buffer);
}

#ifdef __AVX_ENABLED

// note although written it has not been tested

bool EffectEqualization48x::ProcessBuffer8x(BufferInfo *bufferInfo)
{
   // length must be a factor of window size for 4x processing. 
   if(bufferInfo->mBufferLength%mBlockSize || mBufferCount!=8)
      return false;

   auto blockCount=bufferInfo->mBufferLength/mBlockSize;

   __m128 *readBlocks[8]; // some temps so we dont destroy the vars in the struct
   __m128 *writeBlocks[8];
   for(int i=0;i<8;i++) {
      readBlocks[i]=(__m128 *)bufferInfo->mBufferSouce[i];
      writeBlocks[i]=(__m128 *)bufferInfo->mBufferDest[i];
   }

   __m128 *swizzledBuffer128=(__m128 *)bufferInfo->mScratchBuffer;
   __m128 *scratchBuffer=&swizzledBuffer128[mWindowSize*4];

   int doubleFilter=mFilterSize<<1;
   int doubleWindow=mWindowSize<<1;
   int doubleBlock=mBlockSize<<1;
   for(int run4x=0;run4x<blockCount;run4x++) 
   {
      // swizzle the data to the swizzle buffer
      __m128 *currentSwizzledBlock=&swizzledBuffer128[doubleWindow*(run4x&1)]; 
      for(int i=0,j=0;j<doubleBlock;i++,j+=8) { // mBlockSize or doubleBlock???
         __m128 tmp0   = _mm_shuffle_ps(readBlocks[0][i], readBlocks[1][i], _MM_SHUFFLE(1,0,1,0)); 
         __m128 tmp1   = _mm_shuffle_ps(readBlocks[0][i], readBlocks[1][i], _MM_SHUFFLE(3,2,3,2)); 
         __m128 tmp2   = _mm_shuffle_ps(readBlocks[2][i], readBlocks[3][i], _MM_SHUFFLE(1,0,1,0)); 
         __m128 tmp3   = _mm_shuffle_ps(readBlocks[2][i], readBlocks[3][i], _MM_SHUFFLE(3,2,3,2)); 
         currentSwizzledBlock[j]   = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(2,0,2,0)); 
         currentSwizzledBlock[j+2] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(3,1,3,1)); 
         currentSwizzledBlock[j+4] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(2,0,2,0)); 
         currentSwizzledBlock[j+6] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(3,1,3,1)); 
         tmp0   = _mm_shuffle_ps(readBlocks[4][i], readBlocks[5][i], _MM_SHUFFLE(1,0,1,0)); 
         tmp1   = _mm_shuffle_ps(readBlocks[4][i], readBlocks[5][i], _MM_SHUFFLE(3,2,3,2)); 
         tmp2   = _mm_shuffle_ps(readBlocks[6][i], readBlocks[7][i], _MM_SHUFFLE(1,0,1,0)); 
         tmp3   = _mm_shuffle_ps(readBlocks[6][i], readBlocks[7][i], _MM_SHUFFLE(3,2,3,2)); 
         currentSwizzledBlock[j+1] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(2,0,2,0)); 
         currentSwizzledBlock[j+3] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(3,1,3,1)); 
         currentSwizzledBlock[j+5] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(2,0,2,0)); 
         currentSwizzledBlock[j+7] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(3,1,3,1)); 
      }
      __m128 *thisOverrun128=&currentSwizzledBlock[doubleBlock]; 
      for(int i=0;i<doubleFilter;i++)
         thisOverrun128[i]=_mm_set1_ps(0.0);
      Filter8x(mWindowSize, (float *)currentSwizzledBlock, (float *)scratchBuffer);
      int writeStart=0, writeToStart=0; // note readStart is where the read data is written
      int writeEnd=doubleBlock;
      if(run4x) {
         // maybe later swizzle add and write in one
         __m128 *lastOverrun128=&swizzledBuffer128[doubleWindow*((run4x+1)&1)+doubleBlock]; 
         // add and swizzle data + filter
         for(int i=0,j=0;j<doubleFilter;i++,j+=8) {
            __m128 tmps0 = _mm_add_ps(currentSwizzledBlock[j], lastOverrun128[j]);
            __m128 tmps1 = _mm_add_ps(currentSwizzledBlock[j+2], lastOverrun128[j+2]);
            __m128 tmps2 = _mm_add_ps(currentSwizzledBlock[j+4], lastOverrun128[j+4]);
            __m128 tmps3 = _mm_add_ps(currentSwizzledBlock[j+6], lastOverrun128[j+6]);
            __m128 tmp0   = _mm_shuffle_ps(tmps1, tmps0, _MM_SHUFFLE(0,1,0,1)); 
            __m128 tmp1   = _mm_shuffle_ps(tmps1, tmps0, _MM_SHUFFLE(2,3,2,3)); 
            __m128 tmp2   = _mm_shuffle_ps(tmps3, tmps2, _MM_SHUFFLE(0,1,0,1)); 
            __m128 tmp3   = _mm_shuffle_ps(tmps3, tmps2, _MM_SHUFFLE(2,3,2,3)); 
            writeBlocks[0][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[1][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(0,2,0,2)); 
            writeBlocks[2][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[3][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(0,2,0,2)); 
            tmps0 = _mm_add_ps(currentSwizzledBlock[j+1], lastOverrun128[j+1]);
            tmps1 = _mm_add_ps(currentSwizzledBlock[j+3], lastOverrun128[j+3]);
            tmps2 = _mm_add_ps(currentSwizzledBlock[j+5], lastOverrun128[j+5]);
            tmps3 = _mm_add_ps(currentSwizzledBlock[j+7], lastOverrun128[j+7]);
            tmp0   = _mm_shuffle_ps(tmps1, tmps0, _MM_SHUFFLE(0,1,0,1)); 
            tmp1   = _mm_shuffle_ps(tmps1, tmps0, _MM_SHUFFLE(2,3,2,3)); 
            tmp2   = _mm_shuffle_ps(tmps3, tmps2, _MM_SHUFFLE(0,1,0,1)); 
            tmp3   = _mm_shuffle_ps(tmps3, tmps2, _MM_SHUFFLE(2,3,2,3)); 
            writeBlocks[4][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[5][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(0,2,0,2)); 
            writeBlocks[6][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[7][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(0,2,0,2)); 
         } 
         writeStart=doubleFilter;
         writeToStart=mFilterSize>>2;
         // swizzle it back. 
         for(int i=writeToStart,j=writeStart;j<writeEnd;i++,j+=8) {
            __m128 tmp0   = _mm_shuffle_ps(currentSwizzledBlock[j+2], currentSwizzledBlock[j], _MM_SHUFFLE(0,1,0,1)); 
            __m128 tmp1   = _mm_shuffle_ps(currentSwizzledBlock[j+2], currentSwizzledBlock[j], _MM_SHUFFLE(2,3,2,3)); 
            __m128 tmp2   = _mm_shuffle_ps(currentSwizzledBlock[j+6], currentSwizzledBlock[j+4], _MM_SHUFFLE(0,1,0,1)); 
            __m128 tmp3   = _mm_shuffle_ps(currentSwizzledBlock[j+6], currentSwizzledBlock[j+4], _MM_SHUFFLE(2,3,2,3)); 
            writeBlocks[0][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[1][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(0,2,0,2)); 
            writeBlocks[2][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[3][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(0,2,0,2)); 
            tmp0   = _mm_shuffle_ps(currentSwizzledBlock[j+3], currentSwizzledBlock[j+1], _MM_SHUFFLE(0,1,0,1)); 
            tmp1   = _mm_shuffle_ps(currentSwizzledBlock[j+3], currentSwizzledBlock[j+1], _MM_SHUFFLE(2,3,2,3)); 
            tmp2   = _mm_shuffle_ps(currentSwizzledBlock[j+7], currentSwizzledBlock[j+5], _MM_SHUFFLE(0,1,0,1)); 
            tmp3   = _mm_shuffle_ps(currentSwizzledBlock[j+7], currentSwizzledBlock[j+5], _MM_SHUFFLE(2,3,2,3)); 
            writeBlocks[4][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[5][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(0,2,0,2)); 
            writeBlocks[6][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(1,3,1,3)); 
            writeBlocks[7][i] = _mm_shuffle_ps(tmp1, tmp3, _MM_SHUFFLE(0,2,0,2)); 
         }
      } else {
         // swizzle it back. We overlap one block so we only write the first block on the first run
         writeStart=0;
         writeToStart=0;
         for(int i=writeToStart,j=writeStart;j<writeEnd;i++,j+=8) {
            __m128 tmp0   = _mm_shuffle_ps(currentSwizzledBlock[j+2], currentSwizzledBlock[j], _MM_SHUFFLE(0,1,0,1)); 
            __m128 tmp2   = _mm_shuffle_ps(currentSwizzledBlock[j+6], currentSwizzledBlock[j+4], _MM_SHUFFLE(0,1,0,1)); 
            writeBlocks[0][i] = _mm_shuffle_ps(tmp0, tmp2, _MM_SHUFFLE(1,3,1,3)); 
         }
      }
      for(int i=0;i<8;i++) { // shift each block
         readBlocks[i]+=mBlockSize>>2; // these are 128b pointers, each window is 1/4 blockSize for those
         writeBlocks[i]+=mBlockSize>>2; 
      }
   }
   return true;
}

bool EffectEqualization48x::ProcessOne8x(int count, WaveTrack * t,
                                         sampleCount start, sampleCount len)
{
   sampleCount blockCount=len/mBlockSize;

   if(blockCount<32) // it's not worth 8x processing do a regular process
      return ProcessOne4x(count, t, start, len);

   auto trackBlockSize = t->GetMaxBlockSize();

   auto output = t->EmptyCopy();
   t->ConvertToSampleFormat( floatSample );

   mEffectEqualization->TrackProgress(count, 0.0);
   int bigRuns=len/(mSubBufferSize-mBlockSize);
   int trackBlocksPerBig=mSubBufferSize/trackBlockSize;
   int trackLeftovers=mSubBufferSize-trackBlocksPerBig*trackBlockSize;
   int singleProcessLength=(mFilterSize>>1)*bigRuns + len%(bigRuns*(mSubBufferSize-mBlockSize));
   auto currentSample=start;

   bool bBreakLoop = false;
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
      if (bBreakLoop=mEffectEqualization->TrackProgress(count, (double)(bigRun)/(double)bigRuns))
      {
         break;
      }
      output->Append((samplePtr)&mBigBuffer[(bigRun?mBlockSize:0)+(mFilterSize>>1)], floatSample, mSubBufferSize-((bigRun?mBlockSize:0)+(mFilterSize>>1)));
   }
   if(singleProcessLength && !bBreakLoop) {
      t->Get((samplePtr)mBigBuffer.get(), floatSample, currentSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
      ProcessBuffer(mBigBuffer.get(), mBigBuffer.get(), singleProcessLength+mBlockSize+(mFilterSize>>1));
      output->Append((samplePtr)&mBigBuffer[mBlockSize], floatSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
   }
   output->Flush();
   if(!bBreakLoop)
      ProcessTail(t, output.get(), start, len);
   return bBreakLoop;
}

bool EffectEqualization48x::ProcessOne8xThreaded(int count, WaveTrack * t,
                                                 sampleCount start, sampleCount len)
{
   sampleCount blockCount=len/mBlockSize;

   if(blockCount<16) // it's not worth 4x processing do a regular process
      return ProcessOne4x(count, t, start, len);
   if(mThreadCount<=0 || blockCount<256) // dont do it without cores or big data
      return ProcessOne4x(count, t, start, len);

   auto output = t->EmptyCopy();
   t->ConvertToSampleFormat( floatSample );

   auto trackBlockSize = t->GetMaxBlockSize();
   mEffectEqualization->TrackProgress(count, 0.0);
   int bigRuns=len/(mSubBufferSize-mBlockSize);
   int trackBlocksPerBig=mSubBufferSize/trackBlockSize;
   int trackLeftovers=mSubBufferSize-trackBlocksPerBig*trackBlockSize;
   int singleProcessLength=(mFilterSize>>1)*bigRuns + len%(bigRuns*(mSubBufferSize-mBlockSize));
   auto currentSample=start;

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
   bool bBreakLoop = false;
   while(bigBlocksWritten<bigRuns) {
      if (bBreakLoop=mEffectEqualization->TrackProgress(count, (double)(bigBlocksWritten)/(double)bigRuns))
      {
         break;
      }
      wxMutexLocker locker( mDataMutex ); // Get in line for data
      // process as many blocks as we can
      while((mBufferInfo[currentIndex].mBufferStatus==BufferDone) && (bigBlocksWritten<bigRuns)) { // data is ours
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
         } else mBufferInfo[currentIndex].mBufferStatus=BufferEmpty; // this is completely unnecessary
         currentIndex=(currentIndex+1)%mWorkerDataCount;
      } 
   }
   if(singleProcessLength && !bBreakLoop) {
      t->Get((samplePtr)mBigBuffer.get(), floatSample, currentSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
      ProcessBuffer(mBigBuffer.get(), mBigBuffer.get(), singleProcessLength+mBlockSize+(mFilterSize>>1));
      output->Append((samplePtr)&mBigBuffer[mBlockSize], floatSample, singleProcessLength+mBlockSize+(mFilterSize>>1));
   }
   output->Flush();
   if(!bBreakLoop)
      ProcessTail(t, output.get(), start, len);
   return bBreakLoop;
}




void EffectEqualization48x::Filter8x(size_t len,
                                     float *buffer, float *scratchBuffer)
{
   int i;
   __m256 real256, imag256;
   // Apply FFT
   RealFFTf8x(buffer, mEffectEqualization->hFFT);

   // Apply filter
   // DC component is purely real
   __m256 *localFFTBuffer=(__m256 *)scratchBuffer;
   __m256 *localBuffer=(__m256 *)buffer;

   __m256 filterFuncR, filterFuncI;
   filterFuncR = _mm256_set1_ps(mEffectEqualization->mFilterFuncR[0]);
   localFFTBuffer[0] = _mm256_mul_ps(localBuffer[0], filterFuncR);
   auto halfLength = (len / 2);

   bool useBitReverseTable = sMathPath & 1;

   for(i = 1; i < halfLength; i++)
   {
      if(useBitReverseTable) {
         real256=localBuffer[mEffectEqualization->hFFT->BitReversed[i]  ];
         imag256=localBuffer[mEffectEqualization->hFFT->BitReversed[i]+1];
      } else {
         int bitReversed=SmallRB(i,mEffectEqualization->hFFT->pow2Bits);
         real256=localBuffer[bitReversed];
         imag256=localBuffer[bitReversed+1];
      }
      filterFuncR=_mm256_set1_ps(mEffectEqualization->mFilterFuncR[i]);
      filterFuncI=_mm256_set1_ps(mEffectEqualization->mFilterFuncI[i]);
      localFFTBuffer[2*i  ] = _mm256_sub_ps( _mm256_mul_ps(real256, filterFuncR), _mm256_mul_ps(imag256, filterFuncI));
      localFFTBuffer[2*i+1] = _mm256_add_ps( _mm256_mul_ps(real256, filterFuncI), _mm256_mul_ps(imag256, filterFuncR));
   }
   // Fs/2 component is purely real
   filterFuncR=_mm256_set1_ps(mEffectEqualization->mFilterFuncR[halfLength]);
   localFFTBuffer[1] = _mm256_mul_ps(localBuffer[1], filterFuncR);

   // Inverse FFT and normalization
   InverseRealFFTf8x(scratchBuffer, mEffectEqualization->hFFT);
   ReorderToTime8x(mEffectEqualization->hFFT, scratchBuffer, buffer);
}

#endif

#endif
