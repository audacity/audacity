/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttlePrefs.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_SHUTTLE_PREFS__
#define __AUDACITY_SHUTTLE_PREFS__

#include "Shuttle.h"

class ShuttlePrefs : public Shuttle
{
public:
   // constructors and destructors
   ShuttlePrefs(){;};
   virtual ~ShuttlePrefs() {;};

public:
   virtual bool TransferBool( const wxString & Name, bool & bValue, const bool & bDefault );
//   virtual bool TransferFloat( const wxString & Name, float & fValue, const float &fDefault );
   virtual bool TransferDouble( const wxString & Name, double & dValue, const double &dDefault );
   virtual bool TransferInt( const wxString & Name, int & iValue, const int &iDefault );
   virtual bool TransferString( const wxString & Name, wxString & strValue, const wxString &strDefault );
   virtual bool TransferWrappedType( const wxString & Name, WrappedType & W );
   virtual bool ExchangeWithMaster(const wxString & Name);
};

#endif
