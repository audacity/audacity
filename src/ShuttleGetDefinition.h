/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttleGetDefinition.h

  Paul Licameli split this out of Shuttle.h

**********************************************************************/

#ifndef __AUDACITY_SHUTTLE_GET_DEFINITION__
#define __AUDACITY_SHUTTLE_GET_DEFINITION__

#include "Shuttle.h" // to inherit
#include "commands/CommandTargets.h" // to inherit

/**************************************************************************//**
\brief Shuttle that retrieves a JSON format definition of a command's parameters.
********************************************************************************/
class AUDACITY_DLL_API ShuttleGetDefinition final
   : public ShuttleParams, public CommandMessageTargetDecorator
{
public:
   ShuttleGetDefinition( CommandMessageTarget & target );
   wxString Result;
   bool IsOptional();
   ShuttleParams & Optional( bool & var ) override;
   void Define( bool & var,     const wxChar * key, const bool vdefault, const bool vmin, const bool vmax, const bool vscl ) override;
   void Define( int & var,      const wxChar * key, const int vdefault, const int vmin, const int vmax, const int vscl ) override;
   void Define( size_t & var,   const wxChar * key, const int vdefault, const int vmin, const int vmax, const int vscl ) override;
   void Define( float & var,    const wxChar * key, const float vdefault, const float vmin, const float vmax, const float vscl ) override;
   void Define( double & var,   const wxChar * key, const float vdefault, const float vmin, const float vmax, const float vscl ) override;
   void Define( double & var,   const wxChar * key, const double vdefault, const double vmin, const double vmax, const double vscl ) override;
   void Define( wxString &var,  const wxChar * key, const wxString vdefault, const wxString vmin, const wxString vmax, const wxString vscl ) override;
   void DefineEnum( int &var, const wxChar * key, const int vdefault,
      const EnumValueSymbol strings[], size_t nStrings ) override;
};

#endif
