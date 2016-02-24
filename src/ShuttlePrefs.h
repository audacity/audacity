/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttlePrefs.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_SHUTTLE_PREFS__
#define __AUDACITY_SHUTTLE_PREFS__

#include "Shuttle.h"

class ShuttlePrefs final : public Shuttle
{
public:
   // constructors and destructors
   ShuttlePrefs(){;};
   virtual ~ShuttlePrefs() {};

public:
   bool TransferBool( const wxString & Name, bool & bValue, const bool & bDefault ) override;
//   bool TransferFloat( const wxString & Name, float & fValue, const float &fDefault ) override;
   bool TransferDouble( const wxString & Name, double & dValue, const double &dDefault ) override;
   bool TransferInt(const wxString & Name, int & iValue, const int &iDefault) override;
   bool TransferString(const wxString & Name, wxString & strValue, const wxString &strDefault) override;
   bool TransferWrappedType(const wxString & Name, WrappedType & W) override;
   bool ExchangeWithMaster(const wxString & Name) override;
};

#endif
