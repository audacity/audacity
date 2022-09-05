/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistControls.h

  Dominic Mazzoni

  Paul Licameli split from Nyquist.h

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST_CONTROLS__
#define __AUDACITY_EFFECT_NYQUIST_CONTROLS__

#include "EffectInterface.h"
#include "FileNames.h"

namespace NyquistFormatting {

wxString EscapeString(const wxString & inStr);

double GetCtrlValue(const wxString &s);

/*!
 A file path given to Nyquist may be a platform-independent canonicalized
 form using certain abbreviations that are expanded into the platform-dependent
 equivalent.

 If the path names only a directory, also append "/untitled" plus extension
 */
void resolveFilePath(wxString & path, FileExtension extension = {});

}

enum NyqControlType
{
   NYQ_CTRL_INT,
   NYQ_CTRL_FLOAT,
   NYQ_CTRL_STRING,
   NYQ_CTRL_CHOICE,
   NYQ_CTRL_INT_TEXT,
   NYQ_CTRL_FLOAT_TEXT,
   NYQ_CTRL_TEXT,
   NYQ_CTRL_TIME,
   NYQ_CTRL_FILE,
};

#define UNINITIALIZED_CONTROL ((double)99999999.99)

//! Correponds to a NyqControl
struct NyqValue {
   wxString valStr;
   double val = 0.0;
};

using NyquistBindings = std::vector<NyqValue>;

class NyqControl
{
public:
   NyqControl() = default;
   NyqControl( const NyqControl& ) = default;
   NyqControl &operator = ( const NyqControl & ) = default;
   //NyqControl( NyqControl && ) = default;
   //NyqControl &operator = ( NyqControl && ) = default;

   int type;
   wxString var;
   wxString name;
   wxString label;
   std::vector<EnumValueSymbol> choices;
   FileNames::FileTypes fileTypes;
   wxString lowStr;
   wxString highStr;
   double low;
   double high;
   int ticks;
};

class NyquistControls {
public:
   using Bindings = NyquistBindings;

   const size_t size() const { return mControls.size(); }
   auto begin() const { return mControls.cbegin(); }
   auto end() const { return mControls.cend(); }
   void emplace_back(const NyqControl &ctrl)
      { mControls.push_back(std::move(ctrl)); }
   void clear() { mControls.clear(); }
   const auto &operator[] (size_t ii) const { return mControls[ii]; }

   void Visit(const Bindings &bindings, ConstSettingsVisitor &visitor)
      const;
   bool Save(const Bindings &bindings, CommandParameters &parms) const;
   //! Sets the lisp variables form the parameters.
   /*!
    We can run this just testing for bad values, or actually setting when
    the values are good.

    @return the number of bad settings.
    */
   int Load(Bindings &bindings,
      const CommandParameters & parms, bool bTestOnly);

   //! Generate Lisp assignment expressions for the interpreter
   wxString Expression(const Bindings &bindings) const;

   void SetControls(std::vector</*const*/ NyqControl> controls)
      { mControls = move(controls); }
   std::vector</*const*/ NyqControl> MoveControls() { return move(mControls); }

private:
   std::vector</*const*/ NyqControl> mControls;
};

struct NyquistSettings {
   // other settings, for the Nyquist prompt; else null
   EffectSettings proxySettings;
   bool proxyDebug{ false };
   std::vector</*const*/ NyqControl> controls;
   std::vector<NyqValue> bindings;

   // Other fields, to do
};
#endif
