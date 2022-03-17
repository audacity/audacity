/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ShuttleAutomation.h

  Paul Licameli split from Shuttle.h

**********************************************************************/

#ifndef __AUDACITY_SHUTTLE_AUTOMATION__
#define __AUDACITY_SHUTTLE_AUTOMATION__

#include "Shuttle.h"

/**************************************************************************//**
\brief SettingsVisitor that gets parameter values into a string.
********************************************************************************/
class AUDACITY_DLL_API ShuttleGetAutomation final : public SettingsVisitor
{
public:
   SettingsVisitor & Optional( bool & var ) override;
   void Define( bool & var, const wxChar * key, bool vdefault,
      bool vmin, bool vmax, bool vscl ) override;
   void Define( int & var, const wxChar * key, int vdefault,
      int vmin, int vmax, int vscl ) override;
   void Define( size_t & var, const wxChar * key, int vdefault,
      int vmin, int vmax, int vscl ) override;
   void Define( float & var, const wxChar * key, float vdefault,
      float vmin, float vmax, float vscl ) override;
   void Define( double & var, const wxChar * key, float vdefault,
      float vmin, float vmax, float vscl ) override;
   void Define( double & var, const wxChar * key, double vdefault,
      double vmin, double vmax, double vscl ) override;
   void Define( wxString &var,  const wxChar * key, wxString vdefault,
      wxString vmin, wxString vmax, wxString vscl ) override;
   void DefineEnum( int &var, const wxChar * key, int vdefault,
      const EnumValueSymbol strings[], size_t nStrings ) override;
};

/**************************************************************************//**
\brief SettingsVisitor that sets parameters to a value (from a string)
********************************************************************************/
class AUDACITY_DLL_API ShuttleSetAutomation final : public SettingsVisitor
{
public:
   ShuttleSetAutomation() {}
   bool bOK{ false };
   bool bWrite{ false };

   bool CouldGet(const wxString &key);
   void SetForValidating( CommandParameters * pEap) {
      mpEap = pEap;
      bOK = true;
      bWrite = false;
   }
   void SetForWriting(CommandParameters * pEap) {
      mpEap = pEap;
      bOK = true;
      bWrite = true;
   }

   SettingsVisitor & Optional( bool & var ) override;
   void Define( bool & var, const wxChar * key, bool vdefault,
      bool vmin, bool vmax, bool vscl ) override;
   void Define( int & var, const wxChar * key, int vdefault,
      int vmin, int vmax, int vscl ) override;
   void Define( size_t & var, const wxChar * key, int vdefault,
      int vmin, int vmax, int vscl ) override;
   void Define( float & var, const wxChar * key, float vdefault,
      float vmin, float vmax, float vscl ) override;
   void Define( double & var, const wxChar * key, float vdefault,
      float vmin, float vmax, float vscl ) override;
   void Define( double & var, const wxChar * key, double vdefault,
      double vmin, double vmax, double vscl ) override;
   void Define( wxString &var,  const wxChar * key, wxString vdefault,
      wxString vmin, wxString vmax, wxString vscl ) override;
   void DefineEnum( int &var, const wxChar * key, int vdefault,
      const EnumValueSymbol strings[], size_t nStrings ) override;
};

/**************************************************************************//**
\brief SettingsVisitor that sets parameters to their default values.
********************************************************************************/
class ShuttleDefaults final : public SettingsVisitor
{
public:
   wxString Result;
   
   SettingsVisitor & Optional( bool & var ) override;
   SettingsVisitor & OptionalY( bool & var ) override;
   SettingsVisitor & OptionalN( bool & var ) override;
   void Define( bool & var, const wxChar * key, bool vdefault,
      bool vmin, bool vmax, bool vscl ) override;
   void Define( int & var, const wxChar * key, int vdefault,
      int vmin, int vmax, int vscl ) override;
   void Define( size_t & var, const wxChar * key, int vdefault,
      int vmin, int vmax, int vscl ) override;
   void Define( float & var, const wxChar * key, float vdefault,
      float vmin, float vmax, float vscl ) override;
   void Define( double & var, const wxChar * key, float vdefault,
      float vmin, float vmax, float vscl ) override;
   void Define( double & var, const wxChar * key, double vdefault,
      double vmin, double vmax, double vscl ) override;
   void Define( wxString &var,  const wxChar * key, wxString vdefault,
      wxString vmin, wxString vmax, wxString vscl ) override;
   void DefineEnum( int &var, const wxChar * key, int vdefault,
      const EnumValueSymbol strings[], size_t nStrings ) override;
};

#endif
