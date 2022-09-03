/**********************************************************************

  Audacity: A Digital Audio Editor

  RulerFormat.h

  Michael Papadopoulos

**********************************************************************/

#ifndef __AUDACITY_RULER_FORMAT__
#define __AUDACITY_RULER_FORMAT__

#include <wx/string.h>
#include <cmath>

class RulerFormat {
public:
   explicit RulerFormat() {}
   virtual ~RulerFormat();

   virtual void SetTickSizes(
      double units, double& mMajor, double& mMinor, double &minorMinor,
      int &mDigits
   ) const = 0;

   virtual void SetLabelString(
      wxString& s, double d, double mMinor, int mDigits, bool useMajor
   ) const = 0;
};

#endif
