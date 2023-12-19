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

#define AILA_DEF_TARGET_PEAK 92.0
#define AILA_DEF_DELTA_PEAK 2.0
#define AILA_DEF_ANALYSIS_TIME 1000.0
#define AILA_DEF_NUMBER_ANALYSIS 5

class AILA {
public:
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
