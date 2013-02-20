/***************************************************************************

   Audacity: A Digtial Audio Editor

   VoiceKey.h: a class implementing a voice key

   (c) 2002-2005  Shane T. Mueller
   Distributed under the terms of the GPL Version 2 or later.

***************************************************************************/
#ifndef __AUDACITY_VOICEKEY__
#define __AUDACITY_VOICEKEY__

#include "WaveTrack.h"


#ifndef M_PI
#define	M_PI		3.14159265358979323846  /* pi */
#endif

enum VoiceKeyTypes
  {
    VKT_NONE = 0,
    VKT_ENERGY = 1,
    VKT_SIGN_CHANGES_LOW = 2,
    VKT_SIGN_CHANGES_HIGH = 4,
    VKT_DIRECTION_CHANGES_LOW = 8,
    VKT_DIRECTION_CHANGES_HIGH = 16
  };

class VoiceKey {

 public:
   VoiceKey();
   ~VoiceKey();
   sampleCount OnForward   (WaveTrack & t, sampleCount start, sampleCount len);
   sampleCount OnBackward  (WaveTrack & t, sampleCount start, sampleCount len);
   sampleCount OffForward  (WaveTrack & t, sampleCount start, sampleCount len);
   sampleCount OffBackward (WaveTrack & t, sampleCount start, sampleCount len);

   void CalibrateNoise(WaveTrack & t, sampleCount start, sampleCount len);
   void AdjustThreshold(double t);


   bool AboveThreshold(WaveTrack & t, sampleCount start,sampleCount len);

   void SetKeyType(bool erg, bool scLow, bool scHigh,
                   bool dcLow, bool dcHigh);

 private:

   double mWindowSize;                 //Size of analysis window, in milliseconds

   double mThresholdAdjustment;               //User-accessible sensitivity calibration variable

   double mEnergyMean;
   double mEnergySD;
   double mSignChangesMean;
   double mSignChangesSD;
   double mDirectionChangesMean;
   double mDirectionChangesSD;

   double mThresholdEnergy;                   // Threshold[*] is equal to [*]Mean + [*]SD * ThresholdAdjustment
   double mThresholdSignChangesLower;
   double mThresholdSignChangesUpper;
   double mThresholdDirectionChangesLower;
   double mThresholdDirectionChangesUpper;

   //These determine which statistics should be used.
   bool mUseEnergy;
   bool mUseSignChangesLow;
   bool mUseSignChangesHigh;
   bool mUseDirectionChangesLow;
   bool mUseDirectionChangesHigh;


   double mSilentWindowSize;           //Time in milliseconds of below-threshold windows required for silence
   double mSignalWindowSize;           //Time in milliseconds of above-threshold windows required for speech

   double TestEnergy (WaveTrack & t, sampleCount start,sampleCount len);
   double TestSignChanges (WaveTrack & t, sampleCount start, sampleCount len);
   double TestDirectionChanges(WaveTrack & t, sampleCount start, sampleCount len);

   void TestEnergyUpdate (double & prevErg, int length, const sampleFormat & drop, const sampleFormat & add);
   void TestSignChangesUpdate(double & currentsignchanges,int length, const sampleFormat & a1,
                              const sampleFormat & a2,   const sampleFormat & z1, const sampleFormat & z2);
   void TestDirectionChangesUpdate(double & currentdirectionchanges,int length,
                                   int & atrend, const sampleFormat & a1, const sampleFormat & a2,
                                   int & ztrend, const sampleFormat & z1, const sampleFormat & z2);

};


inline int sgn(int  number){ return (number<0) ? -1: 1;};

//This returns a logistic density based on a z-score
// a logistic distn has variance (pi*s)^2/3

//inline float inline float logistic(float z){   return fexp(-1 * z/(pi / sqrt(3)) / (1 + pow(fexp(-1 * z(pi / sqrt(3))),2)));};
#endif
