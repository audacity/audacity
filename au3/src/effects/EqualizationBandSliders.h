/**********************************************************************

  Audacity: A Digital Audio Editor

  EqualizationBandSliders.h

  Mitch Golden
  Vaughan Johnson (Preview)

  Paul Licameli split from Equalization.h

***********************************************************************/
#ifndef __AUDACITY_EQUALIZATION_BAND_SLIDERS__
#define __AUDACITY_EQUALIZATION_BAND_SLIDERS__

#define NUMBER_OF_BANDS 31
#define NUM_PTS 180

#include "EqualizationCurvesList.h"
#include "EqualizationFilter.h"
#include <wx/event.h>

class wxSlider;

struct EqualizationBandSliders : public wxEvtHandler
{
public:
    EqualizationBandSliders(EqualizationCurvesList& curvesList);
    void Init();
    void AddBandSliders(ShuttleGui& S);
    void Flatten();
    void GraphicEQ(Envelope& env);
    void Invert();
    void EnvLogToLin();
    void EnvLinToLog();
    void ErrMin();

private:
    double mWhens[NUM_PTS]{};
    double mWhenSliders[NUMBER_OF_BANDS + 1]{};
    size_t mBandsInUse{ NUMBER_OF_BANDS };

    int mSlidersOld[NUMBER_OF_BANDS]{};
    double mEQVals[NUMBER_OF_BANDS + 1]{};

    wxSlider* mSliders[NUMBER_OF_BANDS]{};

    EqualizationCurvesList& mCurvesList;

    static void spline(double x[], double y[], size_t n, double y2[]);
    static
    double splint(double x[], double y[], size_t n, double y2[], double xr);

    // Convenience function template for binding event handler functions
    template<typename EventTag, typename Class, typename Event>
    void BindTo(
        wxEvtHandler& src, const EventTag& eventType, void (Class::*pmf)(Event&))
    {
        src.Bind(eventType, pmf, static_cast<Class*>(this));
    }

    void OnErase(wxEvent& event);
    void OnSlider(wxCommandEvent& event);
};
#endif
