/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttlePrefs.h

  Dominic Mazzoni
  James Crook

**********************************************************************/

#ifndef __AUDACITY_SHUTTLE_PREFS__
#define __AUDACITY_SHUTTLE_PREFS__

#include "SettingsVisitor.h"

class COMMAND_PARAMETERS_API ShuttlePrefs final
{
public:
    bool mbStoreInClient{};
    wxString mValueString;

    // constructors and destructors
    ShuttlePrefs() {}
    ~ShuttlePrefs() {}

public:
    bool TransferBool(const wxString& Name, bool& bValue, const bool& bDefault);
//   bool TransferFloat( const wxString & Name, float & fValue, const float &fDefault );
    bool TransferDouble(const wxString& Name, double& dValue, const double& dDefault);
    bool TransferInt(const wxString& Name, int& iValue, const int& iDefault);
    bool TransferString(const wxString& Name, wxString& strValue, const wxString& strDefault);
    bool TransferWrappedType(const wxString& Name, WrappedType& W);
    bool ExchangeWithMaster(const wxString& Name);
};

#endif
