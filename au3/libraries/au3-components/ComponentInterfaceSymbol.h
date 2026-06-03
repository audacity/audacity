/**********************************************************************

  Audacity: A Digital Audio Editor

  @file ComponentInterfaceSymbol.h

  Paul Licameli split from ComponentInterface.h

**********************************************************************/

#ifndef __AUDACITY_COMPONENT_INTERFACE_SYMBOL__
#define __AUDACITY_COMPONENT_INTERFACE_SYMBOL__

#include "au3-strings/Identifier.h"
#include "au3-strings/Internat.h"

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
    ComponentInterfaceSymbol(const ::TranslatableString& msgid)
        : mInternal{au3::qtToWx(msgid.msgid())}, mMsgid{msgid}
    {}

    // Allows implicit construction from an internal string re-used as a msgid
    ComponentInterfaceSymbol(const wxString& internal)
        : mInternal{internal}, mMsgid{::TranslatableString::untranslatable(internal)}
    {}

    // Allows implicit construction from an internal string re-used as a msgid
    ComponentInterfaceSymbol(const wxChar* msgid)
        : mInternal{msgid}, mMsgid{::TranslatableString::untranslatable(wxString(msgid))}
    {}

    // Two-argument version distinguishes internal from translatable string
    // such as when the first squeezes spaces out
    ComponentInterfaceSymbol(const Identifier& internal,
                             const ::TranslatableString& msgid)
        : mInternal{internal.GET()}
        // Do not permit non-empty msgid with empty internal
        , mMsgid{internal.empty() ? ::TranslatableString {} : msgid}
    {}

    const wxString& Internal() const { return mInternal; }
    const ::TranslatableString& Msgid() const { return mMsgid; }
    const ::TranslatableString Stripped() const { return mMsgid.stripped(); }
    const wxString Translation() const { return wxString::FromUTF8(mMsgid.Translation().c_str()); }
    const wxString StrippedTranslation() const
    { return wxString::FromUTF8(Stripped().Translation().c_str()); }

    bool empty() const { return mInternal.empty(); }

    //! Comparator for such as find_if, using internal name only
    friend inline bool operator ==(
        const ComponentInterfaceSymbol& a, const ComponentInterfaceSymbol& b)
    { return a.mInternal == b.mInternal; }

    friend inline bool operator !=(
        const ComponentInterfaceSymbol& a, const ComponentInterfaceSymbol& b)
    { return !(a == b); }

    //! Comparator for use in ordered containers, using internal name only
    friend inline bool operator <(
        const ComponentInterfaceSymbol& a, const ComponentInterfaceSymbol& b)
    { return a.mInternal < b.mInternal; }

private:
    wxString mInternal;
    ::TranslatableString mMsgid;
};

// TODO: real type distinctions for these aliases, and move them elsewhere
using EnumValueSymbol = ComponentInterfaceSymbol;
using NumericFormatSymbol = EnumValueSymbol;
using EffectFamilySymbol = ComponentInterfaceSymbol;

#endif
