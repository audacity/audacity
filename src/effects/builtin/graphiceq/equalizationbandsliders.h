/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

/**********************************************************************

  Audacity: A Digital Audio Editor

  EqualizationBandSliders.h

  Mitch Golden
  Vaughan Johnson (Preview)

  Paul Licameli split from Equalization.h

***********************************************************************/

#define NUMBER_OF_BANDS 31
#define NUM_PTS 180

#include "libraries/lib-builtin-effects/EqualizationCurvesList.h"
#include "libraries/lib-builtin-effects/EqualizationFilter.h"

#include <functional>

struct EqualizationBandSliders
{
public:
    EqualizationBandSliders(std::function<EqualizationCurvesList* ()> getCurvesList);
    void Init();
    void Flatten();
    void GraphicEQ(Envelope& env);
    void Invert();
    void EnvLogToLin();
    void EnvLinToLog();
    void ErrMin();

    double GetSliderValue(int index) const;
    void SetSliderValue(int index, double db);

private:
    const std::function<EqualizationCurvesList* ()> m_getCurvesList;

    double mWhens[NUM_PTS]{};
    double mWhenSliders[NUMBER_OF_BANDS + 1]{};
    size_t mBandsInUse{ NUMBER_OF_BANDS };

    double mEQVals[NUMBER_OF_BANDS + 1]{};

    std::string mSliderTooltips[NUMBER_OF_BANDS]{};

    static void spline(double x[], double y[], size_t n, double y2[]);
    static
    double splint(double x[], double y[], size_t n, double y2[], double xr);

    void OnSliderUpdated(int index);
};
