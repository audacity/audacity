/**********************************************************************

  Audacity: A Digital Audio Editor

  SettingsVisitor.h

  James Crook

**********************************************************************/
#ifndef __AUDACITY_SETTINGS_VISITOR__
#define __AUDACITY_SETTINGS_VISITOR__

#include <type_traits>

#include "ComponentInterfaceSymbol.h"

class ComponentInterfaceSymbol;
class WrappedType;

template<
   typename Structure, //!< Structure holding the parameter
   typename Member, //!< Often the same as Type
   typename Type, //!< Type of the given default value
   typename Value = Type //!< A type constructible from the default
> struct EffectParameter {
   Member Structure::*const mem{}; //!< Member holding the parameter
   const wxChar *const key{}; //!< Identifier in configuration file
   const Type def{};          //!< Default value
   const Type min{};          //!< Minimum value
   const Type max{};          //!< Maximum value
   const Type scale{};        //!< Scaling factor, for slider control
};

// Deduction guides
// Type of def chooses the parameter type; others just need to be convertible
template<typename Structure, typename Member,
   typename Type, typename... Args>
EffectParameter(Member Structure::*const mem,
   const wxChar *key, const Type &def, Args...)
      -> EffectParameter<Structure, Member, Type>;
// Deduce string type from string literal
template<typename Structure, typename Member,
   typename Char, size_t N, typename... Args>
EffectParameter(Member Structure::*const mem,
   const wxChar *key, const Char (&def)[N], Args...)
      -> EffectParameter<Structure, Member, const Char *, wxString>;

template<typename Structure, typename Member>
struct EnumParameter : EffectParameter<Structure, Member, int>
{
   constexpr EnumParameter(Member Structure::*const mem,
      const wxChar *key, int def, int min, int max, int scale,
      const EnumValueSymbol *symbols_, size_t nSymbols_ )
      : EffectParameter<Structure, Member, int>{
         mem, key, def, min, max, scale }
      , symbols{ symbols_ }
      , nSymbols{ nSymbols_ }
   {}

   const EnumValueSymbol *const symbols;
   const size_t nSymbols;
};

// Deduction guide
template<typename Structure, typename Member, typename... Args>
EnumParameter(Member Structure::*const mem, Args...)
   -> EnumParameter<Structure, Member>;

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

extern template class COMPONENTS_API SettingsVisitorBase<false>;
extern template class COMPONENTS_API SettingsVisitorBase<true>;

using SettingsVisitor = SettingsVisitorBase<false>;
using ConstSettingsVisitor = SettingsVisitorBase<true>;

#endif
