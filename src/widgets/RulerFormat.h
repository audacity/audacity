/**********************************************************************

  Audacity: A Digital Audio Editor

  RulerFormat.h

  Michael Papadopoulos

**********************************************************************/

#ifndef __AUDACITY_RULER_FORMAT__
#define __AUDACITY_RULER_FORMAT__

#include <wx/string.h>
#include <cmath>
#include <any> // needed for customizable data

class RulerFormat {
public:
   enum TickType {
      t_major, t_minor, t_minorMinor
   };
   explicit RulerFormat() {}
   virtual ~RulerFormat() = 0;

   virtual void SetTickSizes(
      double units, double& mMajor, double& mMinor, double &minorMinor,
      int &mDigits, const std::any& data
   ) const = 0;

   virtual void SetLabelString(
      wxString& s, double d, double mMinor, int mDigits, TickType tickType,
      const std::any& data
   ) const = 0;

   virtual std::string Identify() const = 0;
};

#endif
