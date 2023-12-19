/**********************************************************************

  Audacity: A Digital Audio Editor

  AILA.cpp

  Dominic Mazzoni
  Joshua Haberman
  Markus Meyer
  Matt Brubeck

  Paul Licameli split from AudioIO.cpp

**********************************************************************/
#include "AILA.h"

#include "AudioIO.h"
#include "BasicUI.h"
#include "ProjectStatus.h"

#include <wx/wxcrtvararg.h>

#define LOWER_BOUND 0.0
#define UPPER_BOUND 1.0

using std::max;
using std::min;

AILA &AILA::Get()
{
   static AILA instance;
   return instance;
}

AILA::AILA()
   : mSubscription{ AudioIO::Get()->Subscribe(*this, &AILA::SetStartTime) }
{
}

// Guarantees that the subscription attaches at startup, but that
// must be delayed somewhat from static initialization time
static struct AILAInitializer { AILAInitializer() {
   BasicUI::CallAfter([]{ AILA::Get(); });
}} sAILAInitializer;

void AILA::Initialize(double t0) {
   gPrefs->Read(wxT("/AudioIO/AutomatedInputLevelAdjustment"), &mActive,         false);
   gPrefs->Read(wxT("/AudioIO/TargetPeak"),            &mGoalPoint,      AILA_DEF_TARGET_PEAK);
   gPrefs->Read(wxT("/AudioIO/DeltaPeakVolume"),       &mGoalDelta,      AILA_DEF_DELTA_PEAK);
   gPrefs->Read(wxT("/AudioIO/AnalysisTime"),          &mAnalysisTime,   AILA_DEF_ANALYSIS_TIME);
   gPrefs->Read(wxT("/AudioIO/NumberAnalysis"),        &mTotalAnalysis,  AILA_DEF_NUMBER_ANALYSIS);
   mGoalDelta         /= 100.0;
   mGoalPoint         /= 100.0;
   mAnalysisTime      /= 1000.0;
   mMax                = 0.0;
   mLastStartTime      = max(0.0, t0);
   mClipped            = false;
   mAnalysisCounter    = 0;
   mChangeFactor       = 1.0;
   mLastChangeType     = 0;
   mTopLevel           = 1.0;
   mAnalysisEndTime    = -1.0;
}

void AILA::Disable() {
   mActive = false;
}

bool AILA::IsActive() {
   return mActive;
}

void AILA::SetStartTime(const AudioIOEvent &evt) {
   if (evt.type == AudioIOEvent::CAPTURE && evt.on) {
      mAbsoluteStartTime = AudioIO::Get()->GetClockTime();
      wxPrintf("START TIME %f\n\n", mAbsoluteStartTime);
   }
}

double AILA::GetLastDecisionTime() {
   return mAnalysisEndTime;
}

void AILA::Process(AudacityProject *proj,
   bool isClipping, int dBRange, double maxPeak)
{
   auto &audioIO = *AudioIO::Get();
   if (proj && mActive) {
      if (isClipping) {
         mClipped = true;
         wxPrintf("clipped");
      }

      mMax = max(mMax, maxPeak);

      if ((mTotalAnalysis == 0 || mAnalysisCounter < mTotalAnalysis)
          && audioIO.GetStreamTime() - mLastStartTime >=
          mAnalysisTime)
      {
         auto ToLinearIfDB = [](double value, int dbRange) {
            if (dbRange >= 0)
               value = pow(10.0, (-(1.0-value) * dbRange)/20.0);
            return value;
         };

         putchar('\n');
         mMax = ToLinearIfDB(mMax, dBRange);
         auto settings = audioIO.GetMixer();
         auto &[_, inputVolume, __] = settings;
         const double iv = inputVolume;
         unsigned short changetype = 0; //0 - no change, 1 - increase change, 2 - decrease change
         wxPrintf("mAnalysisCounter:%d\n", mAnalysisCounter);
         wxPrintf("\tmClipped:%d\n", mClipped);
         wxPrintf("\tmMax (linear):%f\n", mMax);
         wxPrintf("\tmGoalPoint:%f\n", mGoalPoint);
         wxPrintf("\tmGoalDelta:%f\n", mGoalDelta);
         wxPrintf("\tiv:%f\n", iv);
         wxPrintf("\tmChangeFactor:%f\n", mChangeFactor);
         if (mClipped || mMax > mGoalPoint + mGoalDelta) {
            wxPrintf("too high:\n");
            mTopLevel = min(mTopLevel, iv);
            wxPrintf("\tmTopLevel:%f\n", mTopLevel);
            //if clipped or too high
            if (iv <= LOWER_BOUND) {
               //we can't improve it more now
               if (mTotalAnalysis != 0) {
                  mActive = false;
                  ProjectStatus::Get( *proj ).Set(
                     XO(
"Automated Recording Level Adjustment stopped. It was not possible to optimize it more. Still too high.") );
               }
               wxPrintf("\talready min vol:%f\n", iv);
            }
            else {
               inputVolume = max<float>(LOWER_BOUND,
                  iv + (mGoalPoint - mMax) * mChangeFactor);
               audioIO.SetMixer(settings);
               auto msg = XO(
"Automated Recording Level Adjustment decreased the volume to %f.")
                  .Format(inputVolume);
               ProjectStatus::Get( *proj ).Set(msg);
               changetype = 1;
               wxPrintf("\tnew vol:%f\n", inputVolume);
               auto [_, check, __] = audioIO.GetMixer();
               wxPrintf("\tverified %f\n", check);
            }
         }
         else if ( mMax < mGoalPoint - mGoalDelta ) {
            //if too low
            wxPrintf("too low:\n");
            if (iv >= UPPER_BOUND || iv + 0.005 > mTopLevel) { //condition for too low volumes and/or variable volumes that cause mTopLevel to decrease too much
               //we can't improve it more
               if (mTotalAnalysis != 0) {
                  mActive = false;
                  ProjectStatus::Get( *proj ).Set(
                     XO(
"Automated Recording Level Adjustment stopped. It was not possible to optimize it more. Still too low.") );
               }
               wxPrintf("\talready max vol:%f\n", iv);
            }
            else {
               inputVolume = min<float>(UPPER_BOUND,
                  iv + (mGoalPoint - mMax) * mChangeFactor);
               if (inputVolume > mTopLevel) {
                  inputVolume = (iv + mTopLevel) / 2.0;
                  wxPrintf("\tTruncated vol:%f\n", inputVolume);
               }
               audioIO.SetMixer(settings);
               auto msg = XO(
"Automated Recording Level Adjustment increased the volume to %.2f.")
                  .Format(inputVolume);
               ProjectStatus::Get( *proj ).Set(msg);
               changetype = 2;
               wxPrintf("\tnew vol:%f\n", inputVolume);
               auto [_, check, __] = audioIO.GetMixer();
               wxPrintf("\tverified %f\n", check);
            }
         }

         mAnalysisCounter++;
         //const PaStreamInfo* info = audioIO::GetClockTime();
         //double latency = 0.0;
         //if (info)
         //   latency = info->inputLatency;
         //mAnalysisEndTime = mTime+latency;
         mAnalysisEndTime = audioIO.GetClockTime() - mAbsoluteStartTime;
         mMax             = 0;
         wxPrintf("\tA decision was made @ %f\n", mAnalysisEndTime);
         mClipped         = false;
         mLastStartTime   = audioIO.GetStreamTime();

         if (changetype == 0)
            mChangeFactor *= 0.8; //time factor
         else if (mLastChangeType == changetype)
            mChangeFactor *= 1.1; //concordance factor
         else
            mChangeFactor *= 0.7; //discordance factor
         mLastChangeType = changetype;
         putchar('\n');
      }

      if (mActive && mTotalAnalysis != 0 && mAnalysisCounter >= mTotalAnalysis) {
         mActive = false;
         if (mMax > mGoalPoint + mGoalDelta)
            ProjectStatus::Get( *proj ).Set(
               XO(
"Automated Recording Level Adjustment stopped. The total number of analyses has been exceeded without finding an acceptable volume. Still too high.") );
         else if (mMax < mGoalPoint - mGoalDelta)
            ProjectStatus::Get( *proj ).Set(
               XO(
"Automated Recording Level Adjustment stopped. The total number of analyses has been exceeded without finding an acceptable volume. Still too low.") );
         else {
            auto msg = XO(
"Automated Recording Level Adjustment stopped. %.2f seems an acceptable volume.")
               .Format(audioIO.GetMixer().inputVolume);
            ProjectStatus::Get( *proj ).Set(msg);
         }
      }
   }
}
