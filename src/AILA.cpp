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
   gPrefs->Read(wxT("/AudioIO/AutomatedInputLevelAdjustment"), &mAILAActive,         false);
   gPrefs->Read(wxT("/AudioIO/TargetPeak"),            &mAILAGoalPoint,      AILA_DEF_TARGET_PEAK);
   gPrefs->Read(wxT("/AudioIO/DeltaPeakVolume"),       &mAILAGoalDelta,      AILA_DEF_DELTA_PEAK);
   gPrefs->Read(wxT("/AudioIO/AnalysisTime"),          &mAILAAnalysisTime,   AILA_DEF_ANALYSIS_TIME);
   gPrefs->Read(wxT("/AudioIO/NumberAnalysis"),        &mAILATotalAnalysis,  AILA_DEF_NUMBER_ANALYSIS);
   mAILAGoalDelta         /= 100.0;
   mAILAGoalPoint         /= 100.0;
   mAILAAnalysisTime      /= 1000.0;
   mAILAMax                = 0.0;
   mAILALastStartTime      = max(0.0, t0);
   mAILAClipped            = false;
   mAILAAnalysisCounter    = 0;
   mAILAChangeFactor       = 1.0;
   mAILALastChangeType     = 0;
   mAILATopLevel           = 1.0;
   mAILAAnalysisEndTime    = -1.0;
}

void AILA::Disable() {
   mAILAActive = false;
}

bool AILA::IsActive() {
   return mAILAActive;
}

void AILA::SetStartTime(const AudioIOEvent &evt) {
   if (evt.type == AudioIOEvent::CAPTURE && evt.on) {
      mAILAAbsoluteStartTime = AudioIO::Get()->GetClockTime();
      wxPrintf("START TIME %f\n\n", mAILAAbsoluteStartTime);
   }
}

double AILA::GetLastDecisionTime() {
   return mAILAAnalysisEndTime;
}

void AILA::Process(AudacityProject *proj,
   bool isClipping, int dBRange, double maxPeak)
{
   auto &audioIO = *AudioIO::Get();
   if (proj && mAILAActive) {
      if (isClipping) {
         mAILAClipped = true;
         wxPrintf("clipped");
      }

      mAILAMax = max(mAILAMax, maxPeak);

      if ((mAILATotalAnalysis == 0 || mAILAAnalysisCounter < mAILATotalAnalysis)
          && audioIO.GetStreamTime() - mAILALastStartTime >=
          mAILAAnalysisTime)
      {
         auto ToLinearIfDB = [](double value, int dbRange) {
            if (dbRange >= 0)
               value = pow(10.0, (-(1.0-value) * dbRange)/20.0);
            return value;
         };

         putchar('\n');
         mAILAMax = ToLinearIfDB(mAILAMax, dBRange);
         auto settings = audioIO.GetMixer();
         auto &[_, inputVolume, __] = settings;
         const double iv = inputVolume;
         unsigned short changetype = 0; //0 - no change, 1 - increase change, 2 - decrease change
         wxPrintf("mAILAAnalysisCounter:%d\n", mAILAAnalysisCounter);
         wxPrintf("\tmAILAClipped:%d\n", mAILAClipped);
         wxPrintf("\tmAILAMax (linear):%f\n", mAILAMax);
         wxPrintf("\tmAILAGoalPoint:%f\n", mAILAGoalPoint);
         wxPrintf("\tmAILAGoalDelta:%f\n", mAILAGoalDelta);
         wxPrintf("\tiv:%f\n", iv);
         wxPrintf("\tmAILAChangeFactor:%f\n", mAILAChangeFactor);
         if (mAILAClipped || mAILAMax > mAILAGoalPoint + mAILAGoalDelta) {
            wxPrintf("too high:\n");
            mAILATopLevel = min(mAILATopLevel, iv);
            wxPrintf("\tmAILATopLevel:%f\n", mAILATopLevel);
            //if clipped or too high
            if (iv <= LOWER_BOUND) {
               //we can't improve it more now
               if (mAILATotalAnalysis != 0) {
                  mAILAActive = false;
                  ProjectStatus::Get( *proj ).Set(
                     XO(
"Automated Recording Level Adjustment stopped. It was not possible to optimize it more. Still too high.") );
               }
               wxPrintf("\talready min vol:%f\n", iv);
            }
            else {
               inputVolume = max<float>(LOWER_BOUND,
                  iv + (mAILAGoalPoint - mAILAMax) * mAILAChangeFactor);
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
         else if ( mAILAMax < mAILAGoalPoint - mAILAGoalDelta ) {
            //if too low
            wxPrintf("too low:\n");
            if (iv >= UPPER_BOUND || iv + 0.005 > mAILATopLevel) { //condition for too low volumes and/or variable volumes that cause mAILATopLevel to decrease too much
               //we can't improve it more
               if (mAILATotalAnalysis != 0) {
                  mAILAActive = false;
                  ProjectStatus::Get( *proj ).Set(
                     XO(
"Automated Recording Level Adjustment stopped. It was not possible to optimize it more. Still too low.") );
               }
               wxPrintf("\talready max vol:%f\n", iv);
            }
            else {
               inputVolume = min<float>(UPPER_BOUND,
                  iv + (mAILAGoalPoint - mAILAMax) * mAILAChangeFactor);
               if (inputVolume > mAILATopLevel) {
                  inputVolume = (iv + mAILATopLevel) / 2.0;
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

         mAILAAnalysisCounter++;
         //const PaStreamInfo* info = audioIO::GetClockTime();
         //double latency = 0.0;
         //if (info)
         //   latency = info->inputLatency;
         //mAILAAnalysisEndTime = mTime+latency;
         mAILAAnalysisEndTime = audioIO.GetClockTime() - mAILAAbsoluteStartTime;
         mAILAMax             = 0;
         wxPrintf("\tA decision was made @ %f\n", mAILAAnalysisEndTime);
         mAILAClipped         = false;
         mAILALastStartTime   = audioIO.GetStreamTime();

         if (changetype == 0)
            mAILAChangeFactor *= 0.8; //time factor
         else if (mAILALastChangeType == changetype)
            mAILAChangeFactor *= 1.1; //concordance factor
         else
            mAILAChangeFactor *= 0.7; //discordance factor
         mAILALastChangeType = changetype;
         putchar('\n');
      }

      if (mAILAActive && mAILATotalAnalysis != 0 && mAILAAnalysisCounter >= mAILATotalAnalysis) {
         mAILAActive = false;
         if (mAILAMax > mAILAGoalPoint + mAILAGoalDelta)
            ProjectStatus::Get( *proj ).Set(
               XO(
"Automated Recording Level Adjustment stopped. The total number of analyses has been exceeded without finding an acceptable volume. Still too high.") );
         else if (mAILAMax < mAILAGoalPoint - mAILAGoalDelta)
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
