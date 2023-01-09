/**********************************************************************

   Audacity: A Digital Audio Editor

   EqualizationCurvesList.cpp

   Mitch Golden
   Vaughan Johnson (Preview)
   Martyn Shaw (FIR filters, response curve, graphic EQ)

   Paul Licameli split from Equalization.cpp

 /*********************************************************************/
#include "EqualizationCurvesList.h"
#include "EqualizationFilter.h"
#include "Envelope.h"
#include "SampleFormat.h"

//
// Set NEW curve selection (safe to call outside of the UI)
//
void EqualizationCurvesList::Select( int curve )
{
   mParameters.mCurveName = mCurves[ curve ].Name;
}

//
// Capture updated envelope
//
void EqualizationCurvesList::EnvelopeUpdated()
{
   if (mParameters.IsLinear())
      EnvelopeUpdated(mParameters.mLinEnvelope, true);
   else
      EnvelopeUpdated(mParameters.mLogEnvelope, false);
}

void EqualizationCurvesList::EnvelopeUpdated(const Envelope &env, bool lin)
{
   const auto &hiFreq = mParameters.mHiFreq;
   const auto &drawMode = mParameters.mDrawMode;
   auto &logEnvelope = mParameters.mLogEnvelope;

   // Allocate and populate point arrays
   size_t numPoints = env.GetNumberOfPoints();
   Doubles when{ numPoints };
   Doubles value{ numPoints };
   env.GetPoints( when.get(), value.get(), numPoints );

   // Clear the unnamed curve
   int curve = mCurves.size() - 1;
   mCurves[ curve ].points.clear();

   if(lin)
   {
      // Copy and convert points
      for (size_t point = 0; point < numPoints; point++)
      {
         double freq = when[ point ] * hiFreq;
         double db = value[ point ];

         // Add it to the curve
         mCurves[ curve ].points.push_back( EQPoint( freq, db ) );
      }
   }
   else
   {
      double loLog = log10( 20. );
      double hiLog = log10( hiFreq );
      double denom = hiLog - loLog;

      // Copy and convert points
      for (size_t point = 0; point < numPoints; point++)
      {
         double freq = pow( 10., ( ( when[ point ] * denom ) + loLog ));
         double db = value[ point ];

         // Add it to the curve
         mCurves[ curve ].points.push_back( EQPoint( freq, db ) );
      }
   }

   // Update unnamed curve (so it's there for next time)
   //(done in a hurry, may not be the neatest -MJS)
   if (!drawMode)
   {
      size_t numPoints = logEnvelope.GetNumberOfPoints();
      Doubles when{ numPoints };
      Doubles value{ numPoints };
      logEnvelope.GetPoints(when.get(), value.get(), numPoints);
      for (size_t i = 0, j = 0; j + 2 < numPoints; i++, j++)
      {
         if ((value[i] < value[i + 1] + .05) && (value[i] > value[i + 1] - .05) &&
            (value[i + 1] < value[i + 2] + .05) && (value[i + 1] > value[i + 2] - .05))
         {   // within < 0.05 dB?
            logEnvelope.Delete(j + 1);
            numPoints--;
            j--;
         }
      }
      Select((int) mCurves.size() - 1);
   }

   // set 'unnamed' as the selected curve
   Select( (int) mCurves.size() - 1 );
}
