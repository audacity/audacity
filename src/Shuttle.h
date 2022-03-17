/**********************************************************************

  Audacity: A Digital Audio Editor

  Shuttle.h

  James Crook

**********************************************************************/

#ifndef __AUDACITY_SHUTTLE__
#define __AUDACITY_SHUTTLE__

#include <type_traits>

#include "ComponentInterfaceSymbol.h"

class ComponentInterfaceSymbol;
class WrappedType;

class Shuttle /* not final */ {
 public:
   // constructors and destructors
   Shuttle();
   virtual ~Shuttle() {}

 public:
   bool mbStoreInClient;
   wxString mValueString;
   // Even though virtual, mostly the transfer functions won't change
   // for special kinds of archive.
   virtual bool TransferBool( const wxString & Name, bool & bValue, const bool & bDefault );
   virtual bool TransferFloat( const wxString & Name, float & fValue, const float &fDefault );
   virtual bool TransferDouble( const wxString & Name, double & dValue, const double &dDefault );
   virtual bool TransferInt( const wxString & Name, int & iValue, const int &iDefault );
   virtual bool TransferInt( const wxString & Name, wxLongLong_t & iValue, const wxLongLong_t &iDefault );
   virtual bool TransferLongLong( const wxString & Name, wxLongLong_t & iValue, const wxLongLong_t &iDefault );
   virtual bool TransferString( const wxString & Name, wxString & strValue, const wxString &strDefault );
   virtual bool TransferEnum( const wxString & Name, int & iValue,
      const int nChoices, const wxString * pFirstStr);
   virtual bool TransferWrappedType( const wxString & Name, WrappedType & W );
   // We expect the ExchangeWithMaster function to change from one type of
   // archive to another.
   virtual bool ExchangeWithMaster(const wxString & Name);
};

class ShuttleCli final : public Shuttle
{
public:
   wxString mParams;
   ShuttleCli() {}
   virtual ~ShuttleCli() {}
   bool ExchangeWithMaster(const wxString & Name) override;
};

class CommandParameters;
/**************************************************************************//**
\brief Visitor of effect or command parameters.  This is a base class with lots of
virtual functions that do nothing by default.
Unrelated to class Shuttle.

@tparam Const if true, then visited settings are not modifiable.
********************************************************************************/
template<bool Const>
class SettingsVisitorBase /* not final */
{
public:
   // By-value argument for const visitor, otherwise reference
   template<typename T> using Ref = std::conditional_t<Const, const T&, T&>;
   // const-reference argument for const visitor, otherwise reference
   template<typename T> using Arg = std::conditional_t<Const, T, T&>;

   wxString mParams;
   std::conditional_t<Const, const bool, bool> *pOptionalFlag{};
   CommandParameters * mpEap{};

   SettingsVisitorBase() {}
   virtual ~SettingsVisitorBase();

   bool ShouldSet();
   virtual SettingsVisitorBase &Optional( Ref<bool> var );
   virtual SettingsVisitorBase &OptionalY( Ref<bool> var );
   virtual SettingsVisitorBase &OptionalN( Ref<bool> var );
   virtual void Define( Arg<bool> var, const wxChar * key, bool vdefault,
      bool vmin = false, bool vmax = false, bool vscl = false );
   virtual void Define( Arg<size_t> var, const wxChar * key, int vdefault,
      int vmin = 0, int vmax = 100000, int vscl = 1 );
   virtual void Define( Arg<int> var, const wxChar * key, int vdefault,
      int vmin = 0, int vmax = 100000, int vscl = 1 );
   virtual void Define( Arg<float> var, const wxChar * key, float vdefault,
      float vmin, float vmax, float vscl = 1.0f );
   virtual void Define( Arg<double> var, const wxChar * key, float vdefault,
      float vmin, float vmax, float vscl = 1.0f );
   virtual void Define( Arg<double> var, const wxChar * key, double vdefault,
      double vmin, double vmax, double vscl = 1.0f );
   virtual void Define( Ref<wxString> var, const wxChar * key,
      wxString vdefault,
      wxString vmin = {}, wxString vmax = {},
      wxString vscl = {} );
   virtual void DefineEnum( Arg<int> var, const wxChar * key, int vdefault,
      const EnumValueSymbol strings[], size_t nStrings );
};

extern template class AUDACITY_DLL_API SettingsVisitorBase<false>;
extern template class AUDACITY_DLL_API SettingsVisitorBase<true>;

using SettingsVisitor = SettingsVisitorBase<false>;
using ConstSettingsVisitor = SettingsVisitorBase<true>;

#define SHUTTLE_PARAM( var, name ) \
  Define( var, KEY_ ## name, DEF_ ## name, MIN_ ## name, MAX_ ## name, SCL_ ## name )

#define SHUTTLE_ENUM_PARAM( var, name, strings, nStrings ) \
  DefineEnum( var, KEY_ ## name, DEF_ ## name, strings, nStrings )

#endif
