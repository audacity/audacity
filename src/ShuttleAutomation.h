/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ShuttleAutomation.h

  Paul Licameli split from Shuttle.h

**********************************************************************/

#ifndef __AUDACITY_SHUTTLE_AUTOMATION__
#define __AUDACITY_SHUTTLE_AUTOMATION__

#include <type_traits>
#include "EffectInterface.h"
#include "Shuttle.h"

class Effect;
//! Interface for manipulations of an Effect's settings
/*!
 It is meant to be stateless, so all member functions are const
 */
class AUDACITY_DLL_API EffectParameterMethods {
public:
   virtual ~EffectParameterMethods();
   virtual void Reset(Effect &effect) const = 0;
   virtual void Visit(Effect &effect, SettingsVisitor & S) const = 0;
   virtual void Get(const Effect &effect, CommandParameters & parms) const = 0;
   virtual bool Set(Effect &effect, const CommandParameters & parms) const = 0;
};

//! Generates EffectParameterMethods overrides from variadic arguments.
/*!
For each effect parameter, the function...
   Reset resets it to a default
   Visit visits it with a SettingsVisitor object
   Get serializes it to a string
   Set deserializes it from a string and returns a success flag (if there is
      failure, parameters might not all be unchanged)

The constructor optionally takes an argument which is a function to be called at
the end of Reset or Set, and returning a value for Set.

 @tparam EffectType subclass of Effect,
    expected to define a public static member function FetchParameters,
    taking EffectType & and EffectSettings &,
    and returning a pointer to something that holds the parameters
*/
template <typename EffectType>
class CapturedParameters : public EffectParameterMethods {
public:
   using Params = std::remove_pointer_t< decltype(
      // When all effects become stateless, EffectType argument won't be needed
      EffectType::FetchParameters(
         std::declval<EffectType&>(), std::declval<EffectSettings &>())
   ) >;

   virtual ~CapturedParameters() = default;

   // Another helper type
   // boolean argument is true if updating (not resetting defaults)
   // Returns true if successful, but that is ignored when resetting
   // When all effects become stateless, EffectType argument won't be needed
   using PostSetFunction =
      std::function< bool(EffectType&, Params &, bool) >;

   template< typename ...Args >
   // Arguments are references to EffectParameter structures
   CapturedParameters(const Args &...args)
      // Delegate to the other constructor
      : CapturedParameters( PostSetFunction{}, args... ) {}

   template< typename Fn, typename ...Args,
      // Require that the first argument be callable with appropriate arguments,
      // making this constructor the more specific overload when it applies
      typename = decltype( std::declval<Fn>()(
         std::declval<EffectType &>(), std::declval<Params &>(), true) )
   >
   // Like the previous, but with an extra first argument,
   // which is called at the end of Reset or Set.  Its return value is
   // ignored in Reset() and passed as the result of Set.
   CapturedParameters(Fn &&PostSet, const Args &...args)
   {
      PostSetFn = std::forward<Fn>(PostSet);
      // Capture the variadic arguments in lambdas that do the work
      // for the virtual functions
      ResetFn = [&args...](Effect &effect, Params &structure,
         const CapturedParameters &This)
            { DoReset(effect, structure, This, args...); };
      VisitFn = [&args...](Params &structure, SettingsVisitor &S)
         { DoVisit(structure, S, args...); };
      GetFn = [&args...](const Params &structure, CommandParameters &parms)
         { DoGet(structure, parms, args...); };
      SetFn = [&args...](Effect &effect, Params &structure,
         const CapturedParameters &This, const CommandParameters &parms)
            { return DoSet(effect, structure, This, parms, args...); };
   }

   void Reset(Effect &effect) const override {
      if (ResetFn) {
         EffectSettings dummy;
         if (auto pStruct = EffectType::FetchParameters(
            static_cast<EffectType&>(effect), dummy))
            ResetFn(effect, *pStruct, *this);
      }
   }
   void Visit(Effect &effect, SettingsVisitor & S) const override {
      if (VisitFn) {
         EffectSettings dummy;
         if (auto pStruct = EffectType::FetchParameters(
            static_cast<EffectType&>(effect), dummy))
            VisitFn(*pStruct, S);
      }
   }
   void Get(const Effect &effect, CommandParameters & parms) const override {
      if (GetFn) {
         EffectSettings dummy;
         // const_cast the effect...
         auto &nonconstEffect = const_cast<Effect &>(effect);
         // ... but only to fetch the structure and pass it as const &
         if (auto pStruct = EffectType::FetchParameters(
            static_cast<EffectType&>(nonconstEffect), dummy))
            GetFn(*pStruct, parms);
      }
   }
   bool Set(Effect &effect, const CommandParameters & parms) const override {
      if (SetFn) {
         EffectSettings dummy;
         if (auto pStruct = EffectType::FetchParameters(
            static_cast<EffectType&>(effect), dummy))
            return SetFn(effect, *pStruct, *this, parms);
         else
            return false;
      }
      return true;
   }

private:
   // Signatures of the lambdas that capture the arguments
   using ResetFunction = std::function<
      // When all effects become stateless, the first argument won't be needed
      void(Effect &effect, Params &structure,
         const CapturedParameters &This) >;
   using VisitFunction = std::function<
      void(Params &structure, SettingsVisitor & S) >;
   using GetFunction = std::function<
      void(const Params &structure, CommandParameters & parms) >;
   // Returns true if successful:
   using SetFunction = std::function<
      // When all effects become stateless, the first argument won't be needed
      bool(Effect &effect, Params &structure,
         const CapturedParameters &This, const CommandParameters & parms) >;

   ResetFunction ResetFn;
   VisitFunction VisitFn;
   GetFunction GetFn;
   SetFunction SetFn;

   PostSetFunction PostSetFn;

   // Function templates to generate the bodies of the lambdas in the
   // constructor.  There are functions to treat individual parameters,
   // sometimes with two overloads, and variadic functions that use
   // fold expressions that apply to the sequence of parameters.

   template< typename Member, typename Type >
   static void ResetOne(Params &structure,
      const EffectParameter< Params, Member, Type > &param) {
      // Do one assignment of the default value
      structure.*(param.mem) = param.def;
   }
   template<typename... Args>
   static void DoReset(Effect &effect, Params &structure,
      const CapturedParameters &This, Args &...args) {
      (ResetOne(structure, args), ...);
      // Call the post-set function after all other assignments
      if (This.PostSetFn)
         This.PostSetFn(static_cast<EffectType&>(effect), structure, false);
   }

   template< typename Member, typename Type >
   static void VisitOne(Params &structure, SettingsVisitor &S,
      const EffectParameter< Params, Member, Type > &param) {
      // Visit one variable
      S.Define( structure.*(param.mem),
         param.key, param.def, param.min, param.max, param.scale );
   }
   // More specific overload for enumeration parameters
   template< typename Member >
   static void VisitOne(Params &structure, SettingsVisitor &S,
      const EnumParameter<Params, Member> &param) {
      // Visit one enumeration variable, passing the table of names
      S.DefineEnum( structure.*(param.mem),
         param.key, param.def, param.symbols, param.nSymbols );
   }
   template<typename... Args>
   static void DoVisit(Params &structure, SettingsVisitor &S, Args &...args) {
      (VisitOne(structure, S, args), ...);
   }

   template< typename Member, typename Type >
   static void GetOne(const Params &structure, CommandParameters & parms,
      const EffectParameter< Params, Member, Type > &param) {
      // Serialize one variable
      parms.Write( param.key, static_cast<Type>(structure.*(param.mem)) );
   }
   // More specific overload for enumeration parameters
   template< typename Member >
   static void GetOne(const Params &structure, CommandParameters & parms,
      const EnumParameter<Params, Member> &param) {
      // Serialize one enumeration variable as a string identifier, not a number
      parms.Write(
         param.key, param.symbols[ structure.*(param.mem) ].Internal() );
   }
   template<typename... Args>
   static void DoGet(const Params &structure, CommandParameters &parms,
      Args &...args) {
      (GetOne(structure, parms, args), ...);
   }

   template< typename Member, typename Type >
   static bool SetOne(Params &structure,
      const CommandParameters &parms,
      const EffectParameter< Params, Member, Type > &param) {
      // Deserialize and assign one variable (or fail)
      if (!parms.ReadAndVerify(param.key, &param.cache, param.def,
         param.min, param.max))
         return false;
      structure.*(param.mem) = param;
      return true;
   }
   // More specific overload for enumeration parameters
   template< typename Member >
   static bool SetOne(Params &structure,
      const CommandParameters &parms,
      const EnumParameter<Params, Member> &param) {
      // Deserialize and assign one enumeration variable (or fail)
      if (!parms.ReadAndVerify(param.key, &param.cache, param.def,
         param.symbols, param.nSymbols))
         return false;
      structure.*(param.mem) = param;
      return true;
   }
   template<typename... Args>
   static bool DoSet(Effect &effect, Params &structure,
      const CapturedParameters &This, const CommandParameters &parms,
      Args &...args) {
      if (!(SetOne(structure, parms, args) && ...))
         return false;
      // Call the post-set function after all other assignments, or return
      // true if no function was given
      return !This.PostSetFn ||
         This.PostSetFn(static_cast<EffectType&>(effect), structure, true);
   }
};

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
