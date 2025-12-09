/**********************************************************************

 Audacity: A Digital Audio Editor

 @file TranslatableString.cpp

 Paul Licameli split from Internat.cpp

 **********************************************************************/

#include "TranslatableString.h"
#include "Identifier.h"
#include <wx/translation.h>

const wxChar* const TranslatableString::NullContextName = wxT("*");

Identifier TranslatableString::MSGID() const
{
    return Identifier{ mMsgid };
}

const TranslatableString::Formatter
TranslatableString::NullContextFormatter {
    [](const wxString& str, TranslatableString::Request request) -> wxString {
        switch (request) {
        case Request::Context:
            return NullContextName;
        case Request::Format:
        case Request::DebugFormat:
        default:
            return str;
        }
    }
};

bool TranslatableString::IsVerbatim() const
{
    return DoGetContext(mFormatter) == NullContextName;
}

TranslatableString& TranslatableString::Strip(unsigned codes)
&
{
    auto prevFormatter = mFormatter;
    mFormatter = [prevFormatter, codes]
                 ( const wxString& str, TranslatableString::Request request ) -> wxString {
        switch (request) {
        case Request::Context:
            return TranslatableString::DoGetContext(prevFormatter);
        case Request::Format:
        case Request::DebugFormat:
        default: {
            bool debug = request == Request::DebugFormat;
            auto result
                =TranslatableString::DoSubstitute(
                      prevFormatter,
                      str, TranslatableString::DoGetContext(prevFormatter),
                      debug);
            if (codes & MenuCodes) {
                // Don't use this, it's in wxCore
                // result = wxStripMenuCodes( result );
                decltype(result) temp;
                temp.swap(result);
                for ( auto iter = temp.begin(), end = temp.end();
                      iter != end; ++iter ) {
                    // Stop at trailing hot key name
                    if (*iter == '\t') {
                        break;
                    }
                    // Strip & (unless escaped by another preceding &)
                    if (*iter == '&' && ++iter == end) {
                        break;
                    }
                    result.append(1, *iter);
                }
            }
            if (codes & Ellipses) {
                if (result.EndsWith(wxT("..."))) {
                    result = result.Left(result.length() - 3);
                }
                // Also check for the single-character Unicode ellipsis
                else if (result.EndsWith(wxT("\u2026"))) {
                    result = result.Left(result.length() - 1);
                }
            }
            return result;
        }
        }
    };

    return *this;
}

wxString TranslatableString::DoGetContext(const Formatter& formatter)
{
    return formatter ? formatter({}, Request::Context) : wxString {};
}

wxString TranslatableString::DoSubstitute(const Formatter& formatter,
                                          const wxString& format, const wxString& context, bool debug)
{
    return formatter
           ? formatter(format, debug ? Request::DebugFormat : Request::Format)
           : // come here for most translatable strings, which have no formatting
           (debug ? format : wxGetTranslation(format, wxString {}, context));
}

wxString TranslatableString::DoChooseFormat(
    const Formatter& formatter,
    const wxString& singular, const wxString& plural, unsigned nn, bool debug)
{
    // come here for translatable strings that choose among forms by number;
    // if not debugging, then two keys are passed to an overload of
    // wxGetTranslation, and also a number.
    // Some languages might choose among more or fewer than two forms
    // (e.g. Arabic has duals and Russian has complicated declension rules)
    wxString context;
    return (debug || NullContextName == (context = DoGetContext(formatter)))
           ? (nn == 1 ? singular : plural)
           : wxGetTranslation(
        singular, plural, nn
#if HAS_I18N_CONTEXTS
        , wxString {},   // domain
        context
#endif
        );
}

TranslatableString& TranslatableString::Join(
    const TranslatableString arg, const wxString& separator)
&
{
    auto prevFormatter = mFormatter;
    mFormatter
        =[prevFormatter,
          arg /* = std::move( arg ) */,
          separator](const wxString& str, Request request)
          -> wxString {
        switch (request) {
        case Request::Context:
            return TranslatableString::DoGetContext(prevFormatter);
        case Request::Format:
        case Request::DebugFormat:
        default: {
            bool debug = request == Request::DebugFormat;
            return
                TranslatableString::DoSubstitute(prevFormatter,
                                                 str, TranslatableString::DoGetContext(prevFormatter),
                                                 debug)
                + separator
                + arg.DoFormat(debug);
        }
        }
    };
    return *this;
}

const TranslatableString TranslatableString::Inaudible{ wxT("\a") };
