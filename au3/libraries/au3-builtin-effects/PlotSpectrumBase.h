/**********************************************************************

  Audacity: A Digital Audio Editor

  PlotSpectrumBase.h

  Dominic Mazzoni
  Matthieu Hodgkinson split from FreqWindow.h

**********************************************************************/
#pragma once

#include "MemoryX.h"
#include "SpectrumAnalyst.h"
#include <memory>

class AudacityProject;

class BUILTIN_EFFECTS_API PlotSpectrumBase
{
public:
    PlotSpectrumBase(AudacityProject& project);

protected:
    bool GetAudio();

    AudacityProject* mProject;
    std::unique_ptr<SpectrumAnalyst> mAnalyst;

    bool mDrawGrid;
    int mSize;
    SpectrumAnalyst::Algorithm mAlg;
    int mFunc;
    int mAxis;
    int dBRange;
    double mRate;
    size_t mDataLen;
    ArrayOf<float> mData;
    size_t mWindowSize;
};
