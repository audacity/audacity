/**********************************************************************

 Audacity: A Digital Audio Editor

 @file WxMuseStringCompat.h

 wxString <-> Qt bridge for TranslatableString. Header-only. Included
 from TranslatableString.h at the end of that header so the class is
 fully defined here.

 **********************************************************************/

#pragma once

#include "TranslatableString.h"

#include <QString>
#include <wx/string.h>

namespace au3 {

//! UTF-8 round-trip preserves non-ASCII bytes on every platform.
inline QString wxToQt(const wxString& s)
{
    return QString::fromUtf8(s.ToUTF8().data());
}

inline wxString qtToWx(const QString& s)
{
    return wxString::FromUTF8(s.toUtf8().constData());
}

//! Wrap a wxString as a no-context (untranslated) TranslatableString.
//! UTF-8 round-trip avoids the wxUSE_UNSAFE_WXSTRING_CONV mojibake trap on
//! Windows.
inline ::TranslatableString untranslatable(const wxString& s)
{
    return ::TranslatableString::untranslatable(wxToQt(s));
}

//! Non-template overload of TranslatableString.h's argDecay so
//! `.arg(wxStringValue)` resolves: wxString -> QString -> Arg<QString>.
inline QString argDecay(const wxString& s)
{
    return wxToQt(s);
}

} // namespace au3

//! Out-of-line so wxString is complete here. UTF-8 round-trip preserves
//! non-ASCII on every platform, unlike wxString's locale-based std::string ctor.
inline wxString TranslatableString::Translation() const
{
    return wxString::FromUTF8(translated().toUtf8().constData());
}

//! Stream operator (au3 convention) — emits the wxString translation,
//! so `out << someTS` against wxTextOutputStream / wxString sinks keeps
//! working. Lives in this header (rather than TranslatableString.h) so
//! wxString is complete at template-definition time.
template<typename Sink>
inline Sink& operator<<(Sink& sink, const ::TranslatableString& s)
{
    return sink << s.Translation();
}
