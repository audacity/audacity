/*
 * Audacity: A Digital Audio Editor
 */

/**********************************************************************

   Audacity: A Digital Audio Editor

   EqualizationBandSliders.cpp

   Mitch Golden
   Vaughan Johnson (Preview)
   Martyn Shaw (FIR filters, response curve, graphic EQ)

   Paul Licameli split from Equalization.cpp

**********************************************************************/
#include "equalizationbandsliders.h"
#include "libraries/lib-math/SampleFormat.h"

#include "log.h"

EqualizationBandSliders::
EqualizationBandSliders(EqualizationCurvesList& curvesList)
    : m_curvesList(curvesList)
{
    for (size_t i = 0; i < NUM_PTS - 1; ++i) {
        mWhens[i] = (double)i / (NUM_PTS - 1.);
    }
    mWhens[NUM_PTS - 1] = 1.;
    mWhenSliders[NUMBER_OF_BANDS] = 1.;
    mEQVals[NUMBER_OF_BANDS] = 0.;
}

void EqualizationBandSliders::Init()
{
    mBandsInUse = 0;
    while (kThirdOct[mBandsInUse] <= m_curvesList.mParameters.mHiFreq) {
        ++mBandsInUse;
        if (mBandsInUse == NUMBER_OF_BANDS) {
            break;
        }
    }
}

double EqualizationBandSliders::GetSliderValue(int index) const
{
    IF_ASSERT_FAILED(index >= 0 && index < NUMBER_OF_BANDS) {
        return 0.0;
    }
    return mEQVals[index];
}

void EqualizationBandSliders::SetSliderValue(int index, double dbValue)
{
    mEQVals[index] = dbValue;
    OnSliderUpdated(index);
}

//
// Flatten the curve
//
void EqualizationBandSliders::Flatten()
{
    auto& parameters = m_curvesList.mParameters;
    const auto& drawMode = parameters.mDrawMode;
    auto& linEnvelope = parameters.mLinEnvelope;
    auto& logEnvelope = parameters.mLogEnvelope;

    logEnvelope.Flatten(0.);
    logEnvelope.SetTrackLen(1.0);
    linEnvelope.Flatten(0.);
    linEnvelope.SetTrackLen(1.0);
    m_curvesList.ForceRecalc();
    if (!drawMode) {
        for ( size_t i = 0; i < mBandsInUse; i++) {
            mEQVals[i] = 0.;

            wxString tip;
            if (kThirdOct[i] < 1000.) {
                tip.Printf(wxT("%dHz\n%.1fdB"), (int)kThirdOct[i], 0.);
            } else {
                tip.Printf(wxT("%gkHz\n%.1fdB"), kThirdOct[i] / 1000., 0.);
            }
            mSliderTooltips[i] = tip.ToStdString();
        }
    }
    m_curvesList.EnvelopeUpdated();
}

void EqualizationBandSliders::EnvLogToLin()
{
    auto& parameters = m_curvesList.mParameters;
    auto& linEnvelope = parameters.mLinEnvelope;
    auto& logEnvelope = parameters.mLogEnvelope;
    const auto& hiFreq = parameters.mHiFreq;

    size_t numPoints = logEnvelope.GetNumberOfPoints();
    if (numPoints == 0) {
        return;
    }

    Doubles when{ numPoints };
    Doubles value{ numPoints };

    linEnvelope.Flatten(0.);
    linEnvelope.SetTrackLen(1.0);
    logEnvelope.GetPoints(when.get(), value.get(), numPoints);
    linEnvelope.Reassign(0., value[0]);
    double loLog = log10(20.);
    double hiLog = log10(hiFreq);
    double denom = hiLog - loLog;

    for (size_t i = 0; i < numPoints; i++) {
        linEnvelope.Insert(pow(10., ((when[i] * denom) + loLog)) / hiFreq, value[i]);
    }
    linEnvelope.Reassign(1., value[numPoints - 1]);
}

void EqualizationBandSliders::EnvLinToLog()
{
    auto& parameters = m_curvesList.mParameters;
    auto& linEnvelope = parameters.mLinEnvelope;
    auto& logEnvelope = parameters.mLogEnvelope;
    const auto& hiFreq = parameters.mHiFreq;

    size_t numPoints = linEnvelope.GetNumberOfPoints();
    if (numPoints == 0) {
        return;
    }

    Doubles when{ numPoints };
    Doubles value{ numPoints };

    logEnvelope.Flatten(0.);
    logEnvelope.SetTrackLen(1.0);
    linEnvelope.GetPoints(when.get(), value.get(), numPoints);
    logEnvelope.Reassign(0., value[0]);
    double loLog = log10(20.);
    double hiLog = log10(hiFreq);
    double denom = hiLog - loLog;
    bool changed = false;

    for (size_t i = 0; i < numPoints; i++) {
        if (when[i] * hiFreq >= 20) {
            // Caution: on Linux, when when == 20, the log calculation rounds
            // to just under zero, which causes an assert error.
            double flog = (log10(when[i] * hiFreq) - loLog) / denom;
            logEnvelope.Insert(std::max(0.0, flog), value[i]);
        } else { //get the first point as close as we can to the last point requested
            changed = true;
            double v = value[i];
            logEnvelope.Insert(0., v);
        }
    }
    logEnvelope.Reassign(1., value[numPoints - 1]);

    if (changed) {
        m_curvesList.EnvelopeUpdated(logEnvelope, false);
    }
}

void EqualizationBandSliders::ErrMin()
{
    const auto& parameters = m_curvesList.mParameters;
    const auto& logEnvelope = parameters.mLogEnvelope;
    const auto& curves = m_curvesList.mCurves;
    const auto& loFreq = parameters.mLoFreq;
    const auto& hiFreq = parameters.mHiFreq;

    const double loLog = log10(loFreq);
    const double hiLog = log10(hiFreq);
    const double denom = hiLog - loLog;

    for (size_t i = 0; i < mBandsInUse; ++i) {
        if (kThirdOct[i] == loFreq) {
            mWhenSliders[i] = 0.;
        } else {
            mWhenSliders[i] = (log10(kThirdOct[i]) - loLog) / denom;
        }
        // set initial values of sliders
        mEQVals[i] = std::clamp(logEnvelope.GetValue(mWhenSliders[i]), minDbGain, maxDbGain);
    }

    double vals[NUM_PTS];
    double error = 0.0;
    double oldError = 0.0;
    double mEQValsOld = 0.0;
    double correction = 1.6;
    bool flag;
    size_t j=0;
    Envelope testEnvelope{ logEnvelope };

    for (size_t i = 0; i < NUM_PTS; i++) {
        vals[i] = testEnvelope.GetValue(mWhens[i]);
    }

    //   Do error minimisation
    error = 0.;
    GraphicEQ(testEnvelope);
    for (size_t i = 0; i < NUM_PTS; i++) { //calc initial error
        double err = vals[i] - testEnvelope.GetValue(mWhens[i]);
        error += err * err;
    }
    oldError = error;
    while (j < mBandsInUse * 12)  //loop over the sliders a number of times
    {
        auto i = j % mBandsInUse;     //use this slider
        if ((j > 0) & (i == 0)) { // if we've come back to the first slider again...
            if (correction > 0) {
                correction = -correction; //go down
            } else {
                correction = -correction / 2.; //go up half as much
            }
        }
        flag = true; // check if we've hit the slider limit
        do{
            oldError = error;
            mEQValsOld = mEQVals[i];
            mEQVals[i] += correction; //move fader value
            if (mEQVals[i] > maxDbGain) {
                mEQVals[i] = maxDbGain;
                flag = false;
            }
            if (mEQVals[i] < minDbGain) {
                mEQVals[i] = minDbGain;
                flag = false;
            }
            GraphicEQ(testEnvelope);      //calculate envelope
            error = 0.;
            for (size_t k = 0; k < NUM_PTS; k++) { //calculate error
                double err = vals[k] - testEnvelope.GetValue(mWhens[k]);
                error += err * err;
            }
        }while ((error < oldError) && flag);
        if (error > oldError) {
            mEQVals[i] = mEQValsOld; //last one didn't work
            error = oldError;
        } else {
            oldError = error;
        }
        if (error < .0025 * mBandsInUse) {
            break; // close enuff
        }
        j++; //try next slider
    }
    if (error > .0025 * mBandsInUse) { // not within 0.05dB on each slider, on average
        m_curvesList.Select((int)curves.size() - 1);
        m_curvesList.EnvelopeUpdated(testEnvelope, false);
    }

    for (size_t i = 0; i < mBandsInUse; ++i) {
        wxString tip;
        if (kThirdOct[i] < 1000.) {
            tip.Printf(wxT("%dHz\n%.1fdB"), (int)kThirdOct[i], mEQVals[i]);
        } else {
            tip.Printf(wxT("%gkHz\n%.1fdB"), kThirdOct[i] / 1000., mEQVals[i]);
        }
        mSliderTooltips[i] = tip.ToStdString();
    }
}

void EqualizationBandSliders::GraphicEQ(Envelope& env)
{
    const auto& parameters = m_curvesList.mParameters;
    const auto& interp = parameters.mInterp;

    // JKC: 'value' is for height of curve.
    // The 0.0 initial value would only get used if NUM_PTS were 0.
    double value = 0.0;
    double dist, span, s;

    env.Flatten(0.);
    env.SetTrackLen(1.0);

    switch (interp) {
    case EqualizationParameters::kBspline: // B-spline
    {
        int minF = 0;
        for (size_t i = 0; i < NUM_PTS; i++) {
            while ((mWhenSliders[minF] <= mWhens[i]) & (minF < (int)mBandsInUse)) {
                minF++;
            }
            minF--;
            if (minF < 0) { //before first slider
                dist = mWhens[i] - mWhenSliders[0];
                span = mWhenSliders[1] - mWhenSliders[0];
                s = dist / span;
                if (s < -1.5) {
                    value = 0.;
                } else if (s < -.5) {
                    value = mEQVals[0] * (s + 1.5) * (s + 1.5) / 2.;
                } else {
                    value = mEQVals[0] * (.75 - s * s) + mEQVals[1] * (s + .5) * (s + .5) / 2.;
                }
            } else {
                if (mWhens[i] > mWhenSliders[mBandsInUse - 1]) { //after last fader
                    dist = mWhens[i] - mWhenSliders[mBandsInUse - 1];
                    span = mWhenSliders[mBandsInUse - 1] - mWhenSliders[mBandsInUse - 2];
                    s = dist / span;
                    if (s > 1.5) {
                        value = 0.;
                    } else if (s > .5) {
                        value = mEQVals[mBandsInUse - 1] * (s - 1.5) * (s - 1.5) / 2.;
                    } else {
                        value = mEQVals[mBandsInUse - 1] * (.75 - s * s)
                                + mEQVals[mBandsInUse - 2] * (s - .5) * (s - .5) / 2.;
                    }
                } else { //normal case
                    dist = mWhens[i] - mWhenSliders[minF];
                    span = mWhenSliders[minF + 1] - mWhenSliders[minF];
                    s = dist / span;
                    if (s < .5) {
                        value = mEQVals[minF] * (0.75 - s * s);
                        if (minF + 1 < (int)mBandsInUse) {
                            value += mEQVals[minF + 1] * (s + .5) * (s + .5) / 2.;
                        }
                        if (minF - 1 >= 0) {
                            value += mEQVals[minF - 1] * (s - .5) * (s - .5) / 2.;
                        }
                    } else {
                        value = mEQVals[minF] * (s - 1.5) * (s - 1.5) / 2.;
                        if (minF + 1 < (int)mBandsInUse) {
                            value += mEQVals[minF + 1] * (.75 - (1. - s) * (1. - s));
                        }
                        if (minF + 2 < (int)mBandsInUse) {
                            value += mEQVals[minF + 2] * (s - .5) * (s - .5) / 2.;
                        }
                    }
                }
            }
            if (mWhens[i] <= 0.) {
                env.Reassign(0., value);
            }
            env.Insert(mWhens[i], value);
        }
        env.Reassign(1., value);
        break;
    }

    case EqualizationParameters::kCosine: // Cosine squared
    {
        int minF = 0;
        for (size_t i = 0; i < NUM_PTS; i++) {
            while ((mWhenSliders[minF] <= mWhens[i]) & (minF < (int)mBandsInUse)) {
                minF++;
            }
            minF--;
            if (minF < 0) { //before first slider
                dist = mWhenSliders[0] - mWhens[i];
                span = mWhenSliders[1] - mWhenSliders[0];
                if (dist < span) {
                    value = mEQVals[0] * (1. + cos(M_PI * dist / span)) / 2.;
                } else {
                    value = 0.;
                }
            } else {
                if (mWhens[i] > mWhenSliders[mBandsInUse - 1]) { //after last fader
                    span = mWhenSliders[mBandsInUse - 1] - mWhenSliders[mBandsInUse - 2];
                    dist = mWhens[i] - mWhenSliders[mBandsInUse - 1];
                    if (dist < span) {
                        value = mEQVals[mBandsInUse - 1] * (1. + cos(M_PI * dist / span)) / 2.;
                    } else {
                        value = 0.;
                    }
                } else { //normal case
                    span = mWhenSliders[minF + 1] - mWhenSliders[minF];
                    dist = mWhenSliders[minF + 1] - mWhens[i];
                    value = mEQVals[minF] * (1. + cos(M_PI * (span - dist) / span)) / 2.
                            + mEQVals[minF + 1] * (1. + cos(M_PI * dist / span)) / 2.;
                }
            }
            if (mWhens[i] <= 0.) {
                env.Reassign(0., value);
            }
            env.Insert(mWhens[i], value);
        }
        env.Reassign(1., value);
        break;
    }

    case EqualizationParameters::kCubic: // Cubic Spline
    {
        double y2[NUMBER_OF_BANDS + 1];
        mEQVals[mBandsInUse] = mEQVals[mBandsInUse - 1];
        spline(mWhenSliders, mEQVals, mBandsInUse + 1, y2);
        for (double xf=0; xf < 1.; xf+=1. / NUM_PTS) {
            env.Insert(xf, splint(mWhenSliders, mEQVals, mBandsInUse + 1, y2, xf));
        }
        break;
    }
    }

    m_curvesList.ForceRecalc();
}

void EqualizationBandSliders::spline(
    double x[], double y[], size_t n, double y2[])
{
    wxASSERT(n > 0);

    double p, sig;
    Doubles u{ n };

    y2[0] = 0.; //
    u[0] = 0.;  //'natural' boundary conditions
    for (size_t i = 1; i + 1 < n; i++) {
        sig = (x[i] - x[i - 1]) / (x[i + 1] - x[i - 1]);
        p = sig * y2[i - 1] + 2.;
        y2[i] = (sig - 1.) / p;
        u[i] = (y[i + 1] - y[i]) / (x[i + 1] - x[i]) - (y[i] - y[i - 1]) / (x[i] - x[i - 1]);
        u[i] = (6. * u[i] / (x[i + 1] - x[i - 1]) - sig * u[i - 1]) / p;
    }
    y2[n - 1] = 0.;
    for (size_t i = n - 1; i--;) {
        y2[i] = y2[i] * y2[i + 1] + u[i];
    }
}

double EqualizationBandSliders::splint(
    double x[], double y[], size_t n, double y2[], double xr)
{
    wxASSERT(n > 1);

    double a, b, h;
    static double xlast = 0.;  // remember last x value requested
    static size_t k = 0;          // and which interval we were in

    if (xr < xlast) {
        k = 0;                 // gone back to start, (or somewhere to the left)
    }
    xlast = xr;
    while ((x[k] <= xr) && (k + 1 < n)) {
        k++;
    }
    wxASSERT(k > 0);
    k--;
    h = x[k + 1] - x[k];
    a = (x[k + 1] - xr) / h;
    b = (xr - x[k]) / h;
    return a * y[k] + b * y[k + 1] + ((a * a * a - a) * y2[k] + (b * b * b - b) * y2[k + 1]) * h * h / 6.;
}

void EqualizationBandSliders::OnSliderUpdated(int i)
{
    auto& parameters = m_curvesList.mParameters;
    auto& logEnvelope = parameters.mLogEnvelope;

    wxString tip;
    if (kThirdOct[i] < 1000.) {
        tip.Printf(wxT("%dHz\n%.1fdB"), (int)kThirdOct[i], mEQVals[i]);
    } else {
        tip.Printf(wxT("%gkHz\n%.1fdB"), kThirdOct[i] / 1000., mEQVals[i]);
    }
    mSliderTooltips[i] = tip.ToStdString();

    GraphicEQ(logEnvelope);
    m_curvesList.EnvelopeUpdated();
}

void EqualizationBandSliders::Invert() // Inverts any curve
{
    auto& parameters = m_curvesList.mParameters;
    auto& linEnvelope = parameters.mLinEnvelope;
    auto& logEnvelope = parameters.mLogEnvelope;

    if (!parameters.mDrawMode) { // Graphic (Slider) mode. Invert the sliders.
        for (size_t i = 0; i < mBandsInUse; i++) {
            mEQVals[i] = -mEQVals[i];

            wxString tip;
            if (kThirdOct[i] < 1000.) {
                tip.Printf(wxT("%dHz\n%.1fdB"), (int)kThirdOct[i], mEQVals[i]);
            } else {
                tip.Printf(wxT("%gkHz\n%.1fdB"), kThirdOct[i] / 1000., mEQVals[i]);
            }
            mSliderTooltips[i] = tip.ToStdString();
        }
        GraphicEQ(logEnvelope);
    } else { // Draw mode.  Invert the points.
        bool lin = parameters.IsLinear(); // refers to the 'log' or 'lin' of the frequency scale, not the amplitude
        size_t numPoints; // number of points in the curve/envelope

        // determine if log or lin curve is the current one
        // and find out how many points are in the curve
        if (lin) { // lin freq scale and so envelope
            numPoints = linEnvelope.GetNumberOfPoints();
        } else {
            numPoints = logEnvelope.GetNumberOfPoints();
        }

        if (numPoints == 0) {
            return;
        }

        Doubles when{ numPoints };
        Doubles value{ numPoints };

        if (lin) {
            linEnvelope.GetPoints(when.get(), value.get(), numPoints);
        } else {
            logEnvelope.GetPoints(when.get(), value.get(), numPoints);
        }

        // invert the curve
        for (size_t i = 0; i < numPoints; i++) {
            if (lin) {
                linEnvelope.Reassign(when[i], -value[i]);
            } else {
                logEnvelope.Reassign(when[i], -value[i]);
            }
        }

        // copy it back to the other one (just in case)
        if (lin) {
            EnvLinToLog();
        } else {
            EnvLogToLin();
        }
    }

    // and update the display etc
    m_curvesList.ForceRecalc();
    m_curvesList.EnvelopeUpdated();
}
