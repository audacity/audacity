/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ComponentInterfaceSymbol.h

  Paul Licameli split from ComponentInterface.h

**********************************************************************/

#ifndef __AUDACITY_COMPONENT_INTERFACE_SYMBOL__
#define __AUDACITY_COMPONENT_INTERFACE_SYMBOL__

#include "Identifier.h"
#include "Internat.h"

/**************************************************************************//**

\brief ComponentInterfaceSymbol pairs a persistent string identifier used internally
with an optional, different string as msgid for lookup in a translation catalog.
\details  If there is need to change a msgid in a later version of the
program, change the constructor call to supply a second argument but leave the
first the same, so that compatibility of older configuration files containing
that internal string is not broken.
********************************************************************************/
class ComponentInterfaceSymbol
{
public:
   ComponentInterfaceSymbol() = default;
   
   // Allows implicit construction from a msgid re-used as an internal string
   ComponentInterfaceSymbol( const TranslatableString &msgid )
      : mInternal{ msgid.MSGID().GET(), }, mMsgid{ msgid }
   {}

   // Allows implicit construction from an internal string re-used as a msgid
   ComponentInterfaceSymbol( const wxString &internal )
      : mInternal{ internal }, mMsgid{ internal, {} }
   {}

   // Allows implicit construction from an internal string re-used as a msgid
   ComponentInterfaceSymbol( const wxChar *msgid )
      : mInternal{ msgid }, mMsgid{ msgid, {} }
   {}

   // Two-argument version distinguishes internal from translatable string
   // such as when the first squeezes spaces out
   ComponentInterfaceSymbol( const Identifier &internal,
                         const TranslatableString &msgid )
      : mInternal{ internal.GET() }
      // Do not permit non-empty msgid with empty internal
      , mMsgid{ internal.empty() ? TranslatableString{} : msgid }
   {}

   const wxString &Internal() const { return mInternal; }
   const TranslatableString &Msgid() const { return mMsgid; }
   const TranslatableString Stripped() const { return mMsgid.Stripped(); }
   const wxString Translation() const { return mMsgid.Translation(); }
   const wxString StrippedTranslation() const
      { return Stripped().Translation(); }

   bool empty() const { return mInternal.empty(); }

   friend inline bool operator == (
      const ComponentInterfaceSymbol &a, const ComponentInterfaceSymbol &b )
   { return a.mInternal == b.mInternal; }

   friend inline bool operator != (
      const ComponentInterfaceSymbol &a, const ComponentInterfaceSymbol &b )
   { return !( a == b ); }

private:
   wxString mInternal;
   TranslatableString mMsgid;
};

// TODO: real type distinctions for these aliases, and move them elsewhere
using EnumValueSymbol = ComponentInterfaceSymbol;
using NumericFormatSymbol = EnumValueSymbol;
using EffectFamilySymbol = ComponentInterfaceSymbol;

#endif
