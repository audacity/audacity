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

/**************************************************************************//**
\brief SettingsVisitor that sets parameters to a value (from a string)
********************************************************************************/
class AUDACITY_DLL_API ShuttleSetAutomation final : public SettingsVisitor
{
public:
   ShuttleSetAutomation(){ bWrite = false; bOK = false;};
   bool bOK;
   bool bWrite;
   SettingsVisitor & Optional( bool & var ) override;
   bool CouldGet(const wxString &key);
   void SetForValidating( CommandParameters * pEap){ mpEap=pEap; bOK=true;bWrite=false;};
   void SetForWriting(CommandParameters * pEap){ mpEap=pEap;bOK=true;bWrite=true;};
   void Define( bool & var,     const wxChar * key, const bool vdefault, const bool vmin, const bool vmax, const bool vscl ) override;
   void Define( int & var,      const wxChar * key, const int vdefault, const int vmin, const int vmax, const int vscl ) override;
   void Define( size_t & var,   const wxChar * key, const int vdefault, const int vmin, const int vmax, const int vscl ) override;
   void Define( float & var,   const wxChar * key, const float vdefault, const float vmin, const float vmax, const float vscl ) override;
   void Define( double & var,   const wxChar * key, const float vdefault, const float vmin, const float vmax, const float vscl ) override;
   void Define( double & var,   const wxChar * key, const double vdefault, const double vmin, const double vmax, const double vscl ) override;
   void Define( wxString &var,  const wxChar * key, const wxString vdefault, const wxString vmin, const wxString vmax, const wxString vscl ) override;
   void DefineEnum( int &var, const wxChar * key, const int vdefault,
      const EnumValueSymbol strings[], size_t nStrings ) override;
};

/**************************************************************************//**
\brief SettingsVisitor that sets parameters to their default values.
********************************************************************************/
class ShuttleDefaults final : public SettingsVisitor
{
public:
   wxString Result;
   virtual SettingsVisitor & Optional( bool & var )override{  var = true; pOptionalFlag = NULL;return *this;};
   virtual SettingsVisitor & OptionalY( bool & var )override{ var = true; pOptionalFlag = NULL;return *this;};
   virtual SettingsVisitor & OptionalN( bool & var )override{ var = false;pOptionalFlag = NULL;return *this;};

   void Define( bool & var,          const wxChar * WXUNUSED(key),  const bool     vdefault,
      const bool     WXUNUSED(vmin), const bool     WXUNUSED(vmax), const bool     WXUNUSED(vscl) )
      override { var = vdefault;};
   void Define( int & var,           const wxChar * WXUNUSED(key),  const int      vdefault,
      const int      WXUNUSED(vmin), const int      WXUNUSED(vmax), const int      WXUNUSED(vscl) )
      override { var = vdefault;};
   void Define( size_t & var,        const wxChar * WXUNUSED(key),  const int      vdefault,
      const int      WXUNUSED(vmin), const int      WXUNUSED(vmax), const int      WXUNUSED(vscl) )
      override{ var = vdefault;};
   void Define( float & var,         const wxChar * WXUNUSED(key),  const float    vdefault,
      const float    WXUNUSED(vmin), const float    WXUNUSED(vmax), const float    WXUNUSED(vscl) )
      override { var = vdefault;};
   void Define( double & var,        const wxChar * WXUNUSED(key),  const float    vdefault,
      const float    WXUNUSED(vmin), const float    WXUNUSED(vmax), const float    WXUNUSED(vscl) )
      override { var = vdefault;};
   void Define( double & var,        const wxChar * WXUNUSED(key),  const double   vdefault,
      const double   WXUNUSED(vmin), const double   WXUNUSED(vmax), const double   WXUNUSED(vscl) )
      override { var = vdefault;};
   void Define( wxString &var,       const wxChar * WXUNUSED(key),  const wxString vdefault,
      const wxString WXUNUSED(vmin), const wxString WXUNUSED(vmax), const wxString WXUNUSED(vscl) )
      override { var = vdefault;};
   void DefineEnum( int &var,        const wxChar * WXUNUSED(key),  const int vdefault,
      const EnumValueSymbol WXUNUSED(strings) [], size_t WXUNUSED( nStrings ) )
      override { var = vdefault;};
};

#endif
