/**********************************************************************

  Audacity: A Digital Audio Editor

  ContrastBase.h

**********************************************************************/
#pragma once

class AudacityProject;

class BUILTIN_EFFECTS_API ContrastBase
{
public:
    double mT0;
    double mT1;
    double mProjectRate;
    double mStartTimeF;
    double mEndTimeF;
    double mStartTimeB;
    double mEndTimeB;

protected:
    float foregrounddB;
    float backgrounddB;
    bool mForegroundIsDefined;
    bool mBackgroundIsDefined;
    double mT0orig;
    double mT1orig;

    bool mDoBackground;
    bool GetDB(float& dB);
    void SetStartAndEndTime();

    double length;

private:
    virtual AudacityProject& GetProject() = 0;
};
