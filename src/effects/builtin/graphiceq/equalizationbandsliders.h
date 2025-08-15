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

struct EqualizationBandSliders
{
public:
    static constexpr auto maxDbGain = 25.0;
    static constexpr auto minDbGain = -25.0;
    static constexpr double kThirdOct[] =
    {
        20., 25., 31., 40., 50., 63., 80., 100., 125., 160., 200.,
        250., 315., 400., 500., 630., 800., 1000., 1250., 1600., 2000.,
        2500., 3150., 4000., 5000., 6300., 8000., 10000., 12500., 16000., 20000.,
    };

    EqualizationBandSliders(EqualizationCurvesList&);
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
    EqualizationCurvesList& m_curvesList;

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
