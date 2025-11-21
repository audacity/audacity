/**********************************************************************

 Audacity: A Digital Audio Editor

 @file TranslatableString.h

 Paul Licameli split from Types.h

 **********************************************************************/

#ifndef __AUDACITY_TRANSLATABLE_STRING__
#define __AUDACITY_TRANSLATABLE_STRING__

#include <stddef.h> // for size_t
#include <functional>
#include <wx/string.h>

class Identifier;

#include <vector>

//! Holds a msgid for the translation catalog; may also bind format arguments
/*!
 Different string-valued accessors for the msgid itself, and for the
 user-visible translation with substitution of captured format arguments.
 Also an accessor for format substitution into the English msgid, for debug-
 only outputs.
 The msgid should be used only in unusual cases and the translation more often

 Implicit conversions to and from wxString are intentionally disabled
*/
class STRINGS_API TranslatableString
{
    enum class Request;
    template< size_t N > struct PluralTemp;

public:
    //! A special string value that will have no screen reader pronunciation
    static const TranslatableString Inaudible;

    //! A multi-purpose function, depending on the enum argument
    /*! the string
       argument is unused in some cases
       If there is no function, defaults are empty context string, no plurals,
       and no substitutions */
    using Formatter = std::function< wxString (const wxString&, Request) >;

    TranslatableString() {}

    /*! Supply {} for the second argument to cause lookup of the msgid with
       empty context string (default context) rather than the null context */
    explicit TranslatableString(wxString str, Formatter formatter)
        : mFormatter{std::move(formatter)}
    {
        mMsgid.swap(str);
    }

    // copy and move
    TranslatableString(const TranslatableString&) = default;
    TranslatableString& operator=(const TranslatableString&) = default;
    TranslatableString(TranslatableString&& str)
        : mFormatter(std::move(str.mFormatter))
    {
        mMsgid.swap(str.mMsgid);
    }

    TranslatableString& operator=(TranslatableString&& str)
    {
        mFormatter = std::move(str.mFormatter);
        mMsgid.swap(str.mMsgid);
        return *this;
    }

    bool empty() const { return mMsgid.empty(); }

    //! MSGID is the English lookup key in the catalog, not necessarily for user's eyes if locale is some other.
    /*! The MSGID might not be all the information TranslatableString holds.
       This is a deliberately ugly-looking function name.  Use with caution. */
    Identifier MSGID() const;

    wxString Translation() const { return DoFormat(false); }

    //! Format as an English string for debugging logs and developers' eyes, not for end users
    wxString Debug() const { return DoFormat(true); }

    //! Warning: comparison of msgids only, which is not all of the information!
    /*! This operator makes it easier to define a std::unordered_map on TranslatableStrings */
    friend bool operator ==(
        const TranslatableString& x, const TranslatableString& y)
    { return x.mMsgid == y.mMsgid; }

    friend bool operator !=(
        const TranslatableString& x, const TranslatableString& y)
    { return !(x == y); }

    //! Returns true if context is NullContextFormatter
    bool IsVerbatim() const;

    //! Capture variadic format arguments (by copy) when there is no plural.
    /*! The substitution is computed later in a call to Translate() after msgid is
       looked up in the translation catalog.
       Any format arguments that are also of type TranslatableString will be
       translated too at substitution time, for non-debug formatting */
    template< typename ... Args >
    TranslatableString& Format(Args&&... args)
    &
    {
        auto prevFormatter = mFormatter;
        this->mFormatter = [prevFormatter, args ...]
                           (const wxString& str, Request request) -> wxString {
            switch (request) {
            case Request::Context:
                return TranslatableString::DoGetContext(prevFormatter);
            case Request::Format:
            case Request::DebugFormat:
            default: {
                bool debug = request == Request::DebugFormat;
                return wxString::Format(
                    TranslatableString::DoSubstitute(
                        prevFormatter,
                        str, TranslatableString::DoGetContext(prevFormatter),
                        debug),
                    TranslatableString::TranslateArgument(args, debug)...
                    );
            }
            }
        };
        return *this;
    }

    template< typename ... Args >
    TranslatableString && Format(Args && ... args)
    && {
        return std::move(Format(std::forward<Args>(args)...));
    }

    //! Choose a non-default and non-null disambiguating context for lookups
    /*! This is meant to be the first of chain-call modifications of the
       TranslatableString object; it will destroy any previously captured
       information */
    TranslatableString& Context(const wxString& context)
    &
    {
        this->mFormatter = [context]
                           (const wxString& str, Request request) -> wxString {
            switch (request) {
            case Request::Context:
                return context;
            case Request::DebugFormat:
                return DoSubstitute({}, str, context, true);
            case Request::Format:
            default:
                return DoSubstitute({}, str, context, false);
            }
        };
        return *this;
    }

    TranslatableString&& Context(const wxString& context)
    &&
    {
        return std::move(Context(context));
    }

    //! Append another translatable string
    /*! lookup of msgids for
       this and for the argument are both delayed until Translate() is invoked
       on this, and then the formatter concatenates the translations */
    TranslatableString& Join(
        TranslatableString arg, const wxString& separator = {}) &;
    TranslatableString&& Join(
        TranslatableString arg, const wxString& separator = {})
    && { return std::move(Join(std::move(arg), separator)); }

    TranslatableString& operator +=(TranslatableString arg)
    {
        Join(std::move(arg));
        return *this;
    }

    //! Implements the XP macro
    /*! That macro specifies a second msgid, a list
       of format arguments, and which of those format arguments selects among
       messages; the translated strings to select among, depending on language,
       might actually be more or fewer than two.  See Internat.h. */
    template< size_t N >
    PluralTemp< N > Plural(const wxString& pluralStr)
    &&
    {
        return PluralTemp< N > { *this, pluralStr };
    }

    /*! Translated strings may still contain menu hot-key codes (indicated by &)
       that wxWidgets interprets, and also trailing ellipses, that should be
       removed for other uses. */
    enum StripOptions : unsigned {
        // Values to be combined with bitwise OR
        MenuCodes = 0x1,
        Ellipses = 0x2,
    };
    TranslatableString& Strip(unsigned options = MenuCodes) &;
    TranslatableString&& Strip(unsigned options = MenuCodes)
    && { return std::move(Strip(options)); }

    //! non-mutating, constructs another TranslatableString object
    TranslatableString Stripped(unsigned options = MenuCodes) const
    { return TranslatableString{ *this }.Strip(options); }

    wxString StrippedTranslation() const { return Stripped().Translation(); }

private:
    static const Formatter NullContextFormatter;

    //! Construct a TranslatableString that does no translation but passes str verbatim
    explicit TranslatableString(wxString str)
        : mFormatter{NullContextFormatter}
    {
        mMsgid.swap(str);
    }

    friend TranslatableString Verbatim(wxString str);

    enum class Request {
        Context,   //!< return a disambiguating context string
        Format,    //!< Given the msgid, format the string for end users
        DebugFormat, //!< Given the msgid, format the string for developers
    };

    static const wxChar* const NullContextName;
    friend std::hash< TranslatableString >;

    static wxString DoGetContext(const Formatter& formatter);
    static wxString DoSubstitute(
        const Formatter& formatter, const wxString& format, const wxString& context, bool debug);
    wxString DoFormat(bool debug) const
    {
        return DoSubstitute(
            mFormatter, mMsgid, DoGetContext(mFormatter), debug);
    }

    static wxString DoChooseFormat(
        const Formatter& formatter, const wxString& singular, const wxString& plural, unsigned nn, bool debug);

    template< typename T > static const T& TranslateArgument(const T& arg, bool)
    { return arg; }
    //! This allows you to wrap arguments of Format in std::cref
    /*! (So that they are captured (as if) by reference rather than by value) */
    template< typename T > static auto TranslateArgument(
        const std::reference_wrapper<T>& arg, bool debug)
    -> decltype(
        TranslatableString::TranslateArgument(arg.get(), debug))
    { return TranslatableString::TranslateArgument(arg.get(), debug); }
    static wxString TranslateArgument(const TranslatableString& arg, bool debug)
    { return arg.DoFormat(debug); }

    template< size_t N > struct PluralTemp {
        TranslatableString& ts;
        const wxString& pluralStr;
        template< typename ... Args >
        TranslatableString && operator()(Args&&... args)
        {
            // Pick from the pack the argument that specifies number
            auto selector
                =std::template get< N >(std::forward_as_tuple(args ...));
            // We need an unsigned value.  Guard against negative values.
            auto nn = static_cast<unsigned>(
                std::max<unsigned long long>(0, selector)
                );
            auto plural = this->pluralStr;
            auto prevFormatter = this->ts.mFormatter;
            this->ts.mFormatter = [prevFormatter, plural, nn, args ...]
                                  (const wxString& str, Request request) -> wxString {
                switch (request) {
                case Request::Context:
                    return TranslatableString::DoGetContext(prevFormatter);
                case Request::Format:
                case Request::DebugFormat:
                default:
                {
                    bool debug = request == Request::DebugFormat;
                    return wxString::Format(
                        TranslatableString::DoChooseFormat(
                            prevFormatter, str, plural, nn, debug),
                        TranslatableString::TranslateArgument(args, debug)...
                        );
                }
                }
            };
            return std::move(ts);
        }
    };

    wxString mMsgid;
    Formatter mFormatter;
};

inline TranslatableString operator +(
    TranslatableString x, TranslatableString y)
{
    return std::move(x += std::move(y));
}

using TranslatableStrings = std::vector<TranslatableString>;

//! For using std::unordered_map on TranslatableString
/*! Note:  hashing on msgids only, which is not all of the information */
namespace std {
template<> struct hash< TranslatableString > {
    size_t operator ()(const TranslatableString& str) const    // noexcept
    {
        const wxString& stdstr = str.mMsgid.ToStdWstring();  // no allocations, a cheap fetch
        using Hasher = hash< wxString >;
        return Hasher{}(stdstr);
    }
};
}

//! Allow TranslatableString to work with shift output operators
template< typename Sink >
inline Sink& operator <<(Sink& sink, const TranslatableString& str)
{
    return sink << str.Translation();
}

//! Require calls to the one-argument constructor to go through this distinct global function name.
/*! This makes it easier to locate and
   review the uses of this function, separately from the uses of the type. */
inline TranslatableString Verbatim(wxString str)
{ return TranslatableString(std::move(str)); }

//! A commonly needed sort comparator, which depends on the language setting
inline bool TranslationLess(
    const TranslatableString& a, const TranslatableString& b)
{
    return a.Translation() < b.Translation();
}

#endif
