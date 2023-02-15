/**********************************************************************

  Audacity: A Digital Audio Editor

  ShuttleGetDefinition.h

  Paul Licameli split this out of SettingsVisitor.h

**********************************************************************/

#ifndef __AUDACITY_SHUTTLE_GET_DEFINITION__
#define __AUDACITY_SHUTTLE_GET_DEFINITION__

#include "SettingsVisitor.h" // to inherit
#include "commands/CommandTargets.h" // to inherit

/**************************************************************************//**
\brief SettingsVisitor that retrieves a JSON format definition of a command's parameters.
********************************************************************************/
class AUDACITY_DLL_API ShuttleGetDefinition final
   : public ConstSettingsVisitor, public CommandMessageTargetDecorator
{
public:
   ShuttleGetDefinition( CommandMessageTarget & target );
   wxString Result;
   bool IsOptional();
   ConstSettingsVisitor & Optional( const bool & var ) override;
   void Define(bool var, const wxChar * key, bool vdefault,
      bool vmin, bool vmax, bool vscl) override;
   void Define(int var, const wxChar * key, int vdefault,
      int vmin, int vmax, int vscl) override;
   void Define(size_t var, const wxChar * key, int vdefault,
      int vmin, int vmax, int vscl) override;
   void Define(float var, const wxChar * key, float vdefault,
      float vmin, float vmax, float vscl) override;
   void Define(double var, const wxChar * key, float vdefault,
      float vmin, float vmax, float vscl) override;
   void Define(double var, const wxChar * key, double vdefault,
      double vmin, double vmax, double vscl) override;
   void Define(const wxString &var,  const wxChar * key, wxString vdefault,
      wxString vmin, wxString vmax, wxString vscl) override;
   void DefineEnum( int var, const wxChar * key, int vdefault,
      const EnumValueSymbol strings[], size_t nStrings) override;
};

#endif
