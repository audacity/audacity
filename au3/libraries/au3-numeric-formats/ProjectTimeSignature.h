/*  SPDX-License-Identifier: GPL-2.0-or-later */
/**********************************************************************

 Audacity: A Digital Audio Editor

 @file ProjectTimeSignature.h

 Dmitry Vedenko

 **********************************************************************/
#pragma once

#include "ClientData.h"
#include "Observer.h"

class AudacityProject;

struct TimeSignatureChangedMessage final
{
    double newTempo {};
    int newUpperTimeSignature {};
    int newLowerTimeSignature {};
};

class NUMERIC_FORMATS_API ProjectTimeSignature final : public ClientData::Base, public Observer::Publisher<TimeSignatureChangedMessage>
{
public:
    static ProjectTimeSignature& Get(AudacityProject& project);
    static const ProjectTimeSignature& Get(const AudacityProject& project);

    ProjectTimeSignature();
    ~ProjectTimeSignature() override;

    double GetTempo() const;
    void   SetTempo(double tempo);

    int  GetUpperTimeSignature() const;
    void SetUpperTimeSignature(int upperTimeSignature);

    int  GetLowerTimeSignature() const;
    void SetLowerTimeSignature(int lowerTimeSignature);

    double GetQuarterDuration() const;
    double GetBeatDuration() const;
    double GetBarDuration() const;

private:
    void PublishSignatureChange();

    double mTempo;
    int mUpperTimeSignature;
    int mLowerTimeSignature;
};
