/**********************************************************************

  Audacity: A Digital Audio Editor

  ScoreAlignParams.h

**********************************************************************/

#ifndef __AUDACITY_SCORE_ALIGN_PARAMS__
#define __AUDACITY_SCORE_ALIGN_PARAMS__

struct ScoreAlignParams {
    double mFramePeriod;
    double mWindowSize;
    double mSilenceThreshold;
    double mForceFinalAlignment;
    double mIgnoreSilence;
    double mPresmoothTime;
    double mLineTime;
    double mSmoothTime;
    // information returned from score alignment:
    int mStatus; // wxID_OK or not?
    double mAudioStart;
    double mAudioEnd;
    double mMidiStart;
    double mMidiEnd;
};

#endif
