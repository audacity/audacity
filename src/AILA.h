/**********************************************************************

  Audacity: A Digital Audio Editor

  @file AILA.h
  @brief Automated input level adjustment

  Dominic Mazzoni

  Paul Licameli split from AudioIO.cpp

**********************************************************************/
#ifndef __AUDACITY_AILA__
#define __AUDACITY_AILA__

class AudacityProject;
struct AudioIOEvent;

#include "Observer.h"
#include "Prefs.h"

class AILA {
public:
   static BoolSetting Enabled;
   static IntSetting TargetPeak; // percentage
   static IntSetting DeltaPeak; // percentage
   static IntSetting AnalysisTime; // milliseconds
   static IntSetting NumberAnalyses;

   static AILA &Get();

   void Initialize(double t0);
   void Disable();
   bool IsActive();
   void Process(AudacityProject *pProject,
      bool isClipping, int dBRange, double maxPeak);
   double GetLastDecisionTime();

private:
   AILA();
   void SetStartTime(const AudioIOEvent &evt);

   const Observer::Subscription mSubscription;

   bool           mActive{ false };
   bool           mClipped{};
   int            mTotalAnalysis{};
   int            mAnalysisCounter{};
   double         mMax{};
   double         mGoalPoint{};
   double         mGoalDelta{};
   double         mAnalysisTime{};
   double         mLastStartTime{};
   double         mChangeFactor{};
   double         mTopLevel{};
   double         mAnalysisEndTime{};
   double         mAbsoluteStartTime{};
   //! 0 - no change, 1 - increase change, 2 - decrease change
   unsigned short mLastChangeType{ 0 };
};

#endif
