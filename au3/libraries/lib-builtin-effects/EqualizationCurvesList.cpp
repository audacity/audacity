/**********************************************************************

   Audacity: A Digital Audio Editor

   EqualizationCurvesList.cpp

   Mitch Golden
   Vaughan Johnson (Preview)
   Martyn Shaw (FIR filters, response curve, graphic EQ)

   Paul Licameli split from Equalization.cpp

 /*********************************************************************/
#include "EqualizationCurvesList.h"
#include "BasicUI.h"
#include "EqualizationFilter.h"
#include "Envelope.h"
#include "SampleFormat.h"

//
// Set NEW curve selection (safe to call outside of the UI)
//
void EqualizationCurvesList::Select(int curve)
{
    mParameters.mCurveName = mCurves[ curve ].Name;
}

//
// Capture updated envelope
//
void EqualizationCurvesList::EnvelopeUpdated()
{
    if (mParameters.IsLinear()) {
        EnvelopeUpdated(mParameters.mLinEnvelope, true);
    } else {
        EnvelopeUpdated(mParameters.mLogEnvelope, false);
    }
}

void EqualizationCurvesList::EnvelopeUpdated(const Envelope& env, bool lin)
{
    const auto& hiFreq = mParameters.mHiFreq;
    const auto& drawMode = mParameters.mDrawMode;
    auto& logEnvelope = mParameters.mLogEnvelope;

    // Allocate and populate point arrays
    size_t numPoints = env.GetNumberOfPoints();
    Doubles when{ numPoints };
    Doubles value{ numPoints };
    env.GetPoints(when.get(), value.get(), numPoints);

    // Clear the unnamed curve
    int curve = mCurves.size() - 1;
    mCurves[ curve ].points.clear();

    if (lin) {
        // Copy and convert points
        for (size_t point = 0; point < numPoints; point++) {
            double freq = when[ point ] * hiFreq;
            double db = value[ point ];

            // Add it to the curve
            mCurves[ curve ].points.push_back(EQPoint(freq, db));
        }
    } else {
        double loLog = log10(20.);
        double hiLog = log10(hiFreq);
        double denom = hiLog - loLog;

        // Copy and convert points
        for (size_t point = 0; point < numPoints; point++) {
            double freq = pow(10., ((when[ point ] * denom) + loLog));
            double db = value[ point ];

            // Add it to the curve
            mCurves[ curve ].points.push_back(EQPoint(freq, db));
        }
    }

    // Update unnamed curve (so it's there for next time)
    //(done in a hurry, may not be the neatest -MJS)
    if (!drawMode) {
        size_t numPoints = logEnvelope.GetNumberOfPoints();
        Doubles when{ numPoints };
        Doubles value{ numPoints };
        logEnvelope.GetPoints(when.get(), value.get(), numPoints);
        for (size_t i = 0, j = 0; j + 2 < numPoints; i++, j++) {
            if ((value[i] < value[i + 1] + .05) && (value[i] > value[i + 1] - .05)
                && (value[i + 1] < value[i + 2] + .05) && (value[i + 1] > value[i + 2] - .05)) { // within < 0.05 dB?
                logEnvelope.Delete(j + 1);
                numPoints--;
                j--;
            }
        }
        Select((int)mCurves.size() - 1);
    }

    // set 'unnamed' as the selected curve
    Select((int)mCurves.size() - 1);
}

//
// Make the passed curve index the active one
//
void EqualizationCurvesList::setCurve(int currentCurve)
{
    constexpr auto loFreqI = EqualizationFilter::loFreqI;

    const auto& lin = mParameters.mLin;
    const auto& hiFreq = mParameters.mHiFreq;

    // Set current choice
    wxASSERT(currentCurve < (int)mCurves.size());
    Select(currentCurve);

    int numPoints = (int)mCurves[currentCurve].points.size();

    auto& env = mParameters.ChooseEnvelope();
    env.Flatten(0.);
    env.SetTrackLen(1.0);

    // Handle special case of no points.
    if (numPoints == 0) {
        ForceRecalc();
        return;
    }

    double when, value;

    // Handle special case 1 point.
    if (numPoints == 1) {
        // only one point, so ensure it is in range then return.
        when = mCurves[currentCurve].points[0].Freq;
        if (lin) {
            when = when / hiFreq;
        } else { // log scale
            // We don't go below loFreqI (20 Hz) in log view.
            double loLog = log10((double)loFreqI);
            double hiLog = log10(hiFreq);
            double denom = hiLog - loLog;
            when
                =(log10(std::max<double>(loFreqI, when))
                  - loLog) / denom;
        }
        value = mCurves[currentCurve].points[0].dB;
        env.Insert(std::min(1.0, std::max(0.0, when)), value);
        ForceRecalc();
        return;
    }

    // We have at least two points, so ensure they are in frequency order.
    std::sort(mCurves[currentCurve].points.begin(),
              mCurves[currentCurve].points.end());

    if (mCurves[currentCurve].points[0].Freq < 0) {
        // Corrupt or invalid curve, so bail.
        ForceRecalc();
        return;
    }

    if (lin) { // linear Hz scale
        for (int pointCount = 0; pointCount < numPoints; pointCount++) {
            when = mCurves[currentCurve].points[pointCount].Freq / hiFreq;
            value = mCurves[currentCurve].points[pointCount].dB;
            if (when <= 1) {
                env.Insert(when, value);
                if (when == 1) {
                    break;
                }
            } else {
                // There are more points at higher freqs,
                // so interpolate next one then stop.
                when = 1.0;
                double nextDB = mCurves[currentCurve].points[pointCount].dB;
                if (pointCount > 0) {
                    double nextF = mCurves[currentCurve].points[pointCount].Freq;
                    double lastF = mCurves[currentCurve].points[pointCount - 1].Freq;
                    double lastDB = mCurves[currentCurve].points[pointCount - 1].dB;
                    value = lastDB
                            + ((nextDB - lastDB)
                               * ((hiFreq - lastF) / (nextF - lastF)));
                } else {
                    value = nextDB;
                }
                env.Insert(when, value);
                break;
            }
        }
    } else { // log Hz scale
        double loLog = log10((double)loFreqI);
        double hiLog = log10(hiFreq);
        double denom = hiLog - loLog;
        int firstAbove20Hz;

        // log scale EQ starts at 20 Hz (threshold of hearing).
        // so find the first point (if any) above 20 Hz.
        for (firstAbove20Hz = 0; firstAbove20Hz < numPoints; firstAbove20Hz++) {
            if (mCurves[currentCurve].points[firstAbove20Hz].Freq > loFreqI) {
                break;
            }
        }

        if (firstAbove20Hz == numPoints) {
            // All points below 20 Hz, so just use final point.
            when = 0.0;
            value = mCurves[currentCurve].points[numPoints - 1].dB;
            env.Insert(when, value);
            ForceRecalc();
            return;
        }

        if (firstAbove20Hz > 0) {
            // At least one point is before 20 Hz and there are more
            // beyond 20 Hz, so interpolate the first
            double prevF = mCurves[currentCurve].points[firstAbove20Hz - 1].Freq;
            prevF = log10(std::max(1.0, prevF)); // log zero is bad.
            double prevDB = mCurves[currentCurve].points[firstAbove20Hz - 1].dB;
            double nextF = log10(mCurves[currentCurve].points[firstAbove20Hz].Freq);
            double nextDB = mCurves[currentCurve].points[firstAbove20Hz].dB;
            when = 0.0;
            value = nextDB - ((nextDB - prevDB) * ((nextF - loLog) / (nextF - prevF)));
            env.Insert(when, value);
        }

        // Now get the rest.
        for (int pointCount = firstAbove20Hz; pointCount < numPoints; pointCount++) {
            double flog = log10(mCurves[currentCurve].points[pointCount].Freq);
            wxASSERT(mCurves[currentCurve].points[pointCount].Freq >= loFreqI);

            when = (flog - loLog) / denom;
            value = mCurves[currentCurve].points[pointCount].dB;
            if (when <= 1.0) {
                env.Insert(when, value);
            } else {
                // This looks weird when adjusting curve in Draw mode if
                // there is a point off-screen.

                /*
                // we have a point beyond fs/2.  Insert it so that env code can use it.
                // but just this one, we have no use for the rest
                env.SetTrackLen(when); // can't Insert if the envelope isn't long enough
                env.Insert(when, value);
                break;
                */

                // interpolate the final point instead
                when = 1.0;
                if (pointCount > 0) {
                    double lastDB = mCurves[currentCurve].points[pointCount - 1].dB;
                    double logLastF
                        =log10(mCurves[currentCurve].points[pointCount - 1].Freq);
                    value = lastDB
                            + ((value - lastDB)
                               * ((log10(hiFreq) - logLastF) / (flog - logLastF)));
                }
                env.Insert(when, value);
                break;
            }
        }
    }
    ForceRecalc();
}

void EqualizationCurvesList::setCurve()
{
    setCurve((int)mCurves.size() - 1);
}

void EqualizationCurvesList::setCurve(const wxString& curveName)
{
    unsigned i = 0;
    for ( i = 0; i < mCurves.size(); i++ ) {
        if (curveName == mCurves[ i ].Name) {
            break;
        }
    }
    if (i == mCurves.size()) {
        using namespace BasicUI;
        ShowMessageBox(
            XO("Requested curve not found, using 'unnamed'"),
            MessageBoxOptions {}.IconStyle(Icon::Error));
        setCurve();
    } else {
        setCurve(i);
    }
}
