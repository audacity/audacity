/**********************************************************************

   Audacity: A Digital Audio Editor

   Types.h

   Leland Lucius

   Copyright (c) 2014, Audacity Team 
   All rights reserved.

   Redistribution and use in source and binary forms, with or without
   modification, are permitted provided that the following conditions
   are met:

   1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

   3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
   FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
   COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
   INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
   BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
   LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
   CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
   LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
   ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
   POSSIBILITY OF SUCH DAMAGE.
   
**********************************************************************/

#ifndef __AUDACITY_TYPES_H__
#define __AUDACITY_TYPES_H__

#include <algorithm>
#include <functional>
#include <type_traits>
#include <vector>
#include <wx/debug.h> // for wxASSERT
#include <wx/string.h> // type used in inline function and member variable

// ----------------------------------------------------------------------------
// TODO:  I'd imagine this header may be replaced by other public headers. But,
//        to try and minimize more changes to the base code, we can use this
//        until proper public headers are created for the stuff in here.
// ----------------------------------------------------------------------------

// An explicitly nonlocalized string, not meant for the user to see.
// String manipulations are discouraged, other than splitting and joining on
// separator characters.
// Wherever GET is used to fetch the underlying wxString, there should be a
// comment explaining the need for it.
class Identifier
{
public:

   Identifier() = default;

   // Allow implicit conversion to this class, but not from
   Identifier(const wxString &str) : value{ str } {}

   // Allow implicit conversion to this class, but not from
   Identifier(const wxChar *str) : value{ str } {}

   // Allow implicit conversion to this class, but not from
   Identifier(const char *str) : value{ str } {}

   // Copy construction and assignment
   Identifier( const Identifier & ) = default;
   Identifier &operator = ( const Identifier& ) = default;

   // Move construction and assignment
   Identifier( wxString && str )
      { value.swap( str ); }
   Identifier( Identifier && id )
      { swap( id ); }
   Identifier &operator= ( Identifier&& id )
   {
      if ( this != &id )
         value.clear(), swap( id );
      return *this;
   }

   // Implements moves
   void swap( Identifier &id ) { value.swap( id.value ); }

   // Convenience for building concatenated identifiers.
   // The list must have at least two members
   // (so you don't easily circumvent the restrictions on interconversions
   // intended in TaggedIdentifier below)
   explicit
   Identifier(std::initializer_list<Identifier> components, wxChar separator);

   bool empty() const { return value.empty(); }
   size_t size() const { return value.size(); }
   size_t length() const { return value.length(); }

   // Explicit conversion to wxString, meant to be ugly-looking and
   // demanding of a comment why it's correct
   const wxString &GET() const { return value; }

   std::vector< Identifier > split( wxChar separator ) const;

private:
   wxString value;
};

// Comparisons of Identifiers are case-sensitive
inline bool operator == ( const Identifier &x, const Identifier &y )
{ return x.GET() == y.GET(); }

inline bool operator != ( const Identifier &x, const Identifier &y )
{ return !(x == y); }

inline bool operator < ( const Identifier &x, const Identifier &y )
{ return x.GET() < y.GET(); }

inline bool operator > ( const Identifier &x, const Identifier &y )
{ return y < x; }

inline bool operator <= ( const Identifier &x, const Identifier &y )
{ return !(y < x); }

inline bool operator >= ( const Identifier &x, const Identifier &y )
{ return !(x < y); }

namespace std
{
   template<> struct hash< Identifier > {
      size_t operator () ( const Identifier &id ) const // noexcept
       { return hash<wxString>{}( id.GET() ); }
   };
}

// This lets you pass Identifier into wxFileConfig::Read
inline bool wxFromString(const wxString& str, Identifier *id)
   { if (id) { *id = str; return true; } else return false; }

// This lets you pass Identifier into wxFileConfig::Write
inline wxString wxToString( const Identifier& str ) { return str.GET(); }

// Template parameter allows generation of different TaggedIdentifier classes
// that don't interconvert implicitly
// The second template parameter determines whether comparisons are case
// sensitive; default is case sensitive
template<typename Tag, bool CaseSensitive = true >
class TaggedIdentifier : public Identifier
{
public:

   using TagType = Tag;

   using Identifier::Identifier;
   TaggedIdentifier() = default;

   // Allowed for the same Tag class and case sensitivity
   TaggedIdentifier( const TaggedIdentifier& ) = default;
   TaggedIdentifier( TaggedIdentifier&& ) = default;
   TaggedIdentifier& operator= ( const TaggedIdentifier& ) = default;
   TaggedIdentifier& operator= ( TaggedIdentifier&& ) = default;

   // Prohibited for other Tag classes or case sensitivity
   template< typename Tag2, bool b >
   TaggedIdentifier( const TaggedIdentifier<Tag2, b>& ) = delete;
   template< typename Tag2, bool b >
   TaggedIdentifier( TaggedIdentifier<Tag2, b>&& ) = delete;
   template< typename Tag2, bool b >
   TaggedIdentifier& operator= ( const TaggedIdentifier<Tag2, b>& ) = delete;
   template< typename Tag2, bool b >
   TaggedIdentifier& operator= ( TaggedIdentifier<Tag2, b>&& ) = delete;

   // Allow implicit conversion to this class from un-tagged Identifier,
   // but not from; resolution will use other overloads above if argument
   // has a tag
   TaggedIdentifier(const Identifier &str) : Identifier{ str } {}

   // Conversion to another kind of TaggedIdentifier
   template<typename String, typename = typename String::TagType>
      String CONVERT() const
         { return String{ this->GET() }; }
};

// Comparison of a TaggedIdentifier with an Identifier is allowed, resolving
// to one of the operators on Identifiers defined above, but always case
// sensitive.

// Comparison operators for two TaggedIdentifiers, below, require the same tags
// and case sensitivity.
template< typename Tag1, typename Tag2, bool b1, bool b2 >
inline bool operator == (
   const TaggedIdentifier< Tag1, b1 > &x, const TaggedIdentifier< Tag2, b2 > &y )
{
   static_assert( std::is_same< Tag1, Tag2 >::value && b1 == b2,
      "TaggedIdentifiers with different tags or sensitivity are not comparable" );
   // This test should be eliminated at compile time:
   if ( b1 )
      return x.GET(). Cmp ( y.GET() ) == 0;
   else
      return x.GET(). CmpNoCase ( y.GET() ) == 0;
}

template< typename Tag1, typename Tag2, bool b1, bool b2 >
inline bool operator != (
   const TaggedIdentifier< Tag1, b1 > &x, const TaggedIdentifier< Tag2, b2 > &y )
{ return !(x == y); }

template< typename Tag1, typename Tag2, bool b1, bool b2 >
inline bool operator < (
   const TaggedIdentifier< Tag1, b1 > &x, const TaggedIdentifier< Tag2, b2 > &y )
{
   static_assert( std::is_same< Tag1, Tag2 >::value && b1 == b2,
      "TaggedIdentifiers with different tags or sensitivity are not comparable" );
   // This test should be eliminated at compile time:
   if ( b1 )
      return x.GET(). Cmp ( y.GET() ) < 0;
   else
      return x.GET(). CmpNoCase ( y.GET() ) < 0;
}

template< typename Tag1, typename Tag2, bool b1, bool b2 >
inline bool operator > (
   const TaggedIdentifier< Tag1, b1 > &x, const TaggedIdentifier< Tag2, b2 > &y )
{ return y < x; }

template< typename Tag1, typename Tag2, bool b1, bool b2 >
inline bool operator <= (
   const TaggedIdentifier< Tag1, b1 > &x, const TaggedIdentifier< Tag2, b2 > &y )
{ return !(y < x); }

template< typename Tag1, typename Tag2, bool b1, bool b2 >
inline bool operator >= (
   const TaggedIdentifier< Tag1, b1 > &x, const TaggedIdentifier< Tag2, b2 > &y )
{ return !(x < y); }

namespace std
{
   template<typename Tag, bool b > struct hash< TaggedIdentifier<Tag, b> >
      : hash< Identifier > {};
}

/**************************************************************************//**

\brief type alias for identifying a Plugin supplied by a module, each module
defining its own interpretation of the strings, which may or may not be as a
file system path
********************************************************************************/
using PluginPath = wxString;
using PluginPaths = std::vector< PluginPath >;

// A key to be passed to wxConfigBase
using RegistryPath = wxString;
using RegistryPaths = std::vector< RegistryPath >;

class wxArrayStringEx;

// File extensions, not including any leading dot
using FileExtension = wxString;
using FileExtensions = wxArrayStringEx;

using FilePath = wxString;
using FilePaths = wxArrayStringEx;

// Identifies a menu command or macro.
// Case-insensitive comparison
struct CommandIdTag;
using CommandID = TaggedIdentifier< CommandIdTag, false >;
using CommandIDs = std::vector<CommandID>;


// Holds a msgid for the translation catalog and may hold a closure that
// captures formatting arguments
//
// Different string-valued accessors for the msgid itself, and for the
// user-visible translation with substitution of captured format arguments.
// Also an accessor for format substitution into the English msgid, for debug-
// only outputs.
// The msgid should be used only in unusual cases and the translation more often
//
// Implicit conversions to and from wxString are intentionally disabled
class TranslatableString {
   enum class Request;
   template< size_t N > struct PluralTemp;

public:
   // A special string value that will have no screen reader pronunciation
   static const TranslatableString Inaudible;

   // A multi-purpose function, depending on the enum argument; the string
   // argument is unused in some cases
   // If there is no function, defaults are empty context string, no plurals,
   // and no substitutions
   using Formatter = std::function< wxString(const wxString &, Request) >;

   TranslatableString() {}

   // Supply {} for the second argument to cause lookup of the msgid with
   // empty context string (default context) rather than the null context
   explicit TranslatableString( wxString str, Formatter formatter )
      : mFormatter{ std::move(formatter) }
   {
      mMsgid.swap( str );
   }

   // copy and move
   TranslatableString( const TranslatableString & ) = default;
   TranslatableString &operator=( const TranslatableString & ) = default;
   TranslatableString( TranslatableString && str )
      : mFormatter( std::move( str.mFormatter ) )
   {
      mMsgid.swap( str.mMsgid );
   }
   TranslatableString &operator=( TranslatableString &&str )
   {
      mFormatter = std::move( str.mFormatter );
      mMsgid.swap( str.mMsgid );
      return *this;
   }

   bool empty() const { return mMsgid.empty(); }

   // MSGID is the English lookup key in the message catalog, not necessarily
   // for user's eyes if the locale is some other.
   // The MSGID might not be all the information TranslatableString holds.
   // This is a deliberately ugly-looking function name.  Use with caution.
   Identifier MSGID() const { return Identifier{ mMsgid }; }

   wxString Translation() const { return DoFormat( false ); }

   // Format as an English string for debugging logs and developers' eyes, not
   // for end users
   wxString Debug() const { return DoFormat( true ); }

   // Warning: comparison of msgids only, which is not all of the information!
   // This operator makes it easier to define a std::unordered_map on
   // TranslatableStrings
   friend bool operator == (
      const TranslatableString &x, const TranslatableString &y)
   { return x.mMsgid == y.mMsgid; }

   friend bool operator != (
      const TranslatableString &x, const TranslatableString &y)
   { return !(x == y); }

   // Returns true if context is NullContextFormatter
   bool IsVerbatim() const;

   // Capture variadic format arguments (by copy) when there is no plural.
   // The substitution is computed later in a call to Translate() after msgid is
   // looked up in the translation catalog.
   // Any format arguments that are also of type TranslatableString will be
   // translated too at substitution time, for non-debug formatting
   template< typename... Args >
   TranslatableString &Format( Args &&...args ) &
   {
      auto prevFormatter = mFormatter;
      this->mFormatter = [prevFormatter, args...]
      (const wxString &str, Request request) -> wxString {
         switch ( request ) {
            case Request::Context:
               return TranslatableString::DoGetContext( prevFormatter );
            case Request::Format:
            case Request::DebugFormat:
            default: {
               bool debug = request == Request::DebugFormat;
               return wxString::Format(
                  TranslatableString::DoSubstitute( prevFormatter, str, debug ),
                  TranslatableString::TranslateArgument( args, debug )...
               );
            }
         }
      };
      return *this;
   }
   template< typename... Args >
   TranslatableString &&Format( Args &&...args ) &&
   {
      return std::move( Format( std::forward<Args>(args)... ) );
   }

   // Choose a non-default and non-null disambiguating context for lookups
   // (but this is not fully implemented)
   // This is meant to be the first of chain-call modifications of the
   // TranslatableString object; it will destroy any previously captured
   // information
   TranslatableString &Context( const wxString &context ) &
   {
      this->mFormatter = [context]
      (const wxString &str, Request request) -> wxString {
         switch ( request ) {
            case Request::Context:
               return context;
            default:
               return str;
         }
      };
      return *this;
   }
   TranslatableString &&Context( const wxString &context ) &&
   {
      return std::move( Context( context ) );
   }

   // Append another translatable string; lookup of msgids for
   // this and for the argument are both delayed until Translate() is invoked
   // on this, and then the formatter concatenates the translations
   TranslatableString &Join(
      TranslatableString arg, const wxString &separator = {} ) &;
   TranslatableString &&Join(
      TranslatableString arg, const wxString &separator = {} ) &&
   { return std::move( Join( std::move(arg), separator ) ); }

   TranslatableString &operator +=( TranslatableString arg )
   {
      Join( std::move( arg ) );
      return *this;
   }

   // Implements the XP macro, which specifies a second msgid, a list
   // of format arguments, and which of those format arguments selects among
   // messages; the translated strings to select among, depending on language,
   // might actually be more or fewer than two.  See Internat.h.
   template< size_t N >
   PluralTemp< N > Plural( const wxString &pluralStr ) &&
   {
     return PluralTemp< N >{ *this, pluralStr };
   }

   // Translated strings may still contain menu hot-key codes (indicated by &)
   // that wxWidgets interprets, and also trailing ellipses, that should be
   // removed for other uses.
   enum StripOptions : unsigned {
      // Values to be combined with bitwise OR
      MenuCodes = 0x1,
      Ellipses = 0x2,
   };
   TranslatableString &Strip( unsigned options = MenuCodes ) &;
   TranslatableString &&Strip( unsigned options = MenuCodes ) &&
   { return std::move( Strip( options ) ); }

   // non-mutating, constructs another TranslatableString object
   TranslatableString Stripped( unsigned options = MenuCodes ) const
   { return TranslatableString{ *this }.Strip( options ); }

   wxString StrippedTranslation() const { return Stripped().Translation(); }

private:
   static const Formatter NullContextFormatter;

   // Construct a TranslatableString that does no translation but passes
   // str verbatim
   explicit TranslatableString( wxString str )
      : mFormatter{ NullContextFormatter }
   {
      mMsgid.swap( str );
   }

   friend TranslatableString Verbatim( wxString str );

   enum class Request {
      Context,     // return a disambiguating context string
      Format,      // Given the msgid, format the string for end users
      DebugFormat, // Given the msgid, format the string for developers
   };

   static const wxChar *const NullContextName;
   friend std::hash< TranslatableString >;

   static wxString DoGetContext( const Formatter &formatter );
   static wxString DoSubstitute(
      const Formatter &formatter, const wxString &format, bool debug );
   wxString DoFormat( bool debug ) const
   {  return DoSubstitute( mFormatter, mMsgid, debug ); }

   static wxString DoChooseFormat(
      const Formatter &formatter,
      const wxString &singular, const wxString &plural, unsigned nn, bool debug );

   template< typename T > static const T &TranslateArgument( const T &arg, bool )
   { return arg; }
   // This allows you to wrap arguments of Format in std::cref so that they
   // are captured (as if) by reference rather than by value
   template< typename T > static auto TranslateArgument(
      const std::reference_wrapper<T> &arg, bool debug )
         -> decltype(
            TranslatableString::TranslateArgument( arg.get(), debug ) )
   { return TranslatableString::TranslateArgument( arg.get(), debug ); }
   static wxString TranslateArgument( const TranslatableString &arg, bool debug )
   { return arg.DoFormat( debug ); }

   template< size_t N > struct PluralTemp{
      TranslatableString &ts;
      const wxString &pluralStr;
      template< typename... Args >
         TranslatableString &&operator()( Args&&... args )
      {
         // Pick from the pack the argument that specifies number
         auto selector =
            std::template get< N >( std::forward_as_tuple( args... ) );
         // We need an unsigned value.  Guard against negative values.
         auto nn = static_cast<unsigned>(
            std::max<unsigned long long>( 0, selector )
         );
         auto plural = this->pluralStr;
         auto prevFormatter = this->ts.mFormatter;
         this->ts.mFormatter = [prevFormatter, plural, nn, args...]
         (const wxString &str, Request request) -> wxString {
            switch ( request ) {
               case Request::Context:
                  return TranslatableString::DoGetContext( prevFormatter );
               case Request::Format:
               case Request::DebugFormat:
               default:
               {
                  bool debug = request == Request::DebugFormat;
                  return wxString::Format(
                     TranslatableString::DoChooseFormat(
                        prevFormatter, str, plural, nn, debug ),
                     TranslatableString::TranslateArgument( args, debug )...
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
   TranslatableString x, TranslatableString y  )
{
   return std::move(x += std::move(y));
}

using TranslatableStrings = std::vector<TranslatableString>;

// For using std::unordered_map on TranslatableString
// Note:  hashing on msgids only, which is not all of the information
namespace std
{
   template<> struct hash< TranslatableString > {
      size_t operator () (const TranslatableString &str) const // noexcept
      {
         const wxString &stdstr = str.mMsgid.ToStdWstring(); // no allocations, a cheap fetch
         using Hasher = hash< wxString >;
         return Hasher{}( stdstr );
      }
   };
}

// Allow TranslatableString to work with shift output operators
template< typename Sink >
inline Sink &operator <<( Sink &sink, const TranslatableString &str )
{
   return sink << str.Translation();
}

// Require calls to the one-argument constructor to go through this
// distinct global function name.  This makes it easier to locate and
// review the uses of this function, separately from the uses of the type.
inline TranslatableString Verbatim( wxString str )
{ return TranslatableString( std::move( str ) ); }

// ----------------------------------------------------------------------------
// A native 64-bit integer...used when referring to any number of samples
// ----------------------------------------------------------------------------

class sampleCount
{
public:
   using type = long long;
   static_assert(sizeof(type) == 8, "Wrong width of sampleCount");

   sampleCount () : value { 0 } {}

   // Allow implicit conversion from integral types
   sampleCount ( type v ) : value { v } {}
   sampleCount ( unsigned long long v ) : value ( v ) {}
   sampleCount ( int v ) : value { v } {}
   sampleCount ( unsigned v ) : value { v } {}
   sampleCount ( long v ) : value { v } {}

   // unsigned long is 64 bit on some platforms.  Let it narrow.
   sampleCount ( unsigned long v ) : value ( v ) {}

   // Beware implicit conversions from floating point values!
   // Otherwise the meaning of binary operators with sampleCount change
   // their meaning when sampleCount is not an alias!
   explicit sampleCount ( float f ) : value ( f ) {}
   explicit sampleCount ( double d ) : value ( d ) {}

   sampleCount ( const sampleCount& ) = default;
   sampleCount &operator= ( const sampleCount& ) = default;

   float as_float() const { return value; }
   double as_double() const { return value; }

   long long as_long_long() const { return value; }

   size_t as_size_t() const {
      wxASSERT(value >= 0);
      wxASSERT(static_cast<std::make_unsigned<type>::type>(value) <= std::numeric_limits<size_t>::max());
      return value;
   }

   sampleCount &operator += (sampleCount b) { value += b.value; return *this; }
   sampleCount &operator -= (sampleCount b) { value -= b.value; return *this; }
   sampleCount &operator *= (sampleCount b) { value *= b.value; return *this; }
   sampleCount &operator /= (sampleCount b) { value /= b.value; return *this; }
   sampleCount &operator %= (sampleCount b) { value %= b.value; return *this; }

   sampleCount operator - () const { return -value; }

   sampleCount &operator ++ () { ++value; return *this; }
   sampleCount operator ++ (int)
      { sampleCount result{ *this }; ++value; return result; }

   sampleCount &operator -- () { --value; return *this; }
   sampleCount operator -- (int)
      { sampleCount result{ *this }; --value; return result; }

private:
   type value;
};

inline bool operator == (sampleCount a, sampleCount b)
{
   return a.as_long_long() == b.as_long_long();
}

inline bool operator != (sampleCount a, sampleCount b)
{
   return !(a == b);
}

inline bool operator < (sampleCount a, sampleCount b)
{
   return a.as_long_long() < b.as_long_long();
}

inline bool operator >= (sampleCount a, sampleCount b)
{
   return !(a < b);
}

inline bool operator > (sampleCount a, sampleCount b)
{
   return b < a;
}

inline bool operator <= (sampleCount a, sampleCount b)
{
   return !(b < a);
}

inline sampleCount operator + (sampleCount a, sampleCount b)
{
   return sampleCount{ a } += b;
}

inline sampleCount operator - (sampleCount a, sampleCount b)
{
   return sampleCount{ a } -= b;
}

inline sampleCount operator * (sampleCount a, sampleCount b)
{
   return sampleCount{ a } *= b;
}

inline sampleCount operator / (sampleCount a, sampleCount b)
{
   return sampleCount{ a } /= b;
}

inline sampleCount operator % (sampleCount a, sampleCount b)
{
   return sampleCount{ a } %= b;
}

// ----------------------------------------------------------------------------
// Function returning the minimum of a sampleCount and a size_t,
// hiding the casts
// ----------------------------------------------------------------------------

inline size_t limitSampleBufferSize( size_t bufferSize, sampleCount limit )
{
   return
      std::min( sampleCount( bufferSize ), std::max( sampleCount(0), limit ) )
         .as_size_t();
}

// ----------------------------------------------------------------------------
// Supported sample formats
// ----------------------------------------------------------------------------
enum sampleFormat : unsigned
{
   //! The increasing sequence of these enum values must correspond to the increasing data type width
   //! These values persist in saved project files, so must not be changed in later program versions
   int16Sample = 0x00020001,
   int24Sample = 0x00040001,
   floatSample = 0x0004000F,

   //! Two synonyms for previous values that might change if more values were added
   narrowestSampleFormat = int16Sample,
   widestSampleFormat = floatSample,
};

// ----------------------------------------------------------------------------
// Provide the number of bytes a specific sample will take
// ----------------------------------------------------------------------------
#define SAMPLE_SIZE(SampleFormat) (SampleFormat >> 16)

// ----------------------------------------------------------------------------
// Generic pointer to sample data
// ----------------------------------------------------------------------------
typedef char *samplePtr;
typedef const char *constSamplePtr;

// ----------------------------------------------------------------------------
// The type for plugin IDs
// ----------------------------------------------------------------------------
typedef wxString PluginID;

// ----------------------------------------------------------------------------
// Supported channel assignments
// ----------------------------------------------------------------------------

typedef enum
{
   // Use to mark end of list
   ChannelNameEOL = -1,
   // The default channel assignment
   ChannelNameMono,
   // From this point, the channels follow the 22.2 surround sound format
   ChannelNameFrontLeft,
   ChannelNameFrontRight,
   ChannelNameFrontCenter,
   ChannelNameLowFrequency1,
   ChannelNameBackLeft,
   ChannelNameBackRight,
   ChannelNameFrontLeftCenter,
   ChannelNameFrontRightCenter,
   ChannelNameBackCenter,
   ChannelNameLowFrequency2,
   ChannelNameSideLeft,
   ChannelNameSideRight,
   ChannelNameTopFrontLeft,
   ChannelNameTopFrontRight,
   ChannelNameTopFrontCenter,
   ChannelNameTopCenter,
   ChannelNameTopBackLeft,
   ChannelNameTopBackRight,
   ChannelNameTopSideLeft,
   ChannelNameTopSideRight,
   ChannelNameTopBackCenter,
   ChannelNameBottomFrontCenter,
   ChannelNameBottomFrontLeft,
   ChannelNameBottomFrontRight,
} ChannelName, *ChannelNames;

// ----------------------------------------------------------------------------
// some frequently needed forward declarations
// ----------------------------------------------------------------------------

class ComponentInterfaceSymbol;

using EnumValueSymbol = ComponentInterfaceSymbol;
using NumericFormatSymbol = EnumValueSymbol;

using VendorSymbol = ComponentInterfaceSymbol;

using EffectFamilySymbol = ComponentInterfaceSymbol;

// LLL FIXME: Until a complete API is devised, we have to use
//            AUDACITY_DLL_API when defining API classes.  This
//            it ugly, but a part of the game.  Remove it when
//            the API is complete.


#if !defined(AUDACITY_DLL_API)
   // This was copied from "Audacity.h" so these headers wouldn't have
   // to include it.

   /* Magic for dynamic library import and export. This is unfortunately
    * compiler-specific because there isn't a standard way to do it. Currently it
    * works with the Visual Studio compiler for windows, and for GCC 4+. Anything
    * else gets all symbols made public, which gets messy */
   /* The Visual Studio implementation */
   #ifdef _MSC_VER
      #ifndef AUDACITY_DLL_API
         #ifdef BUILDING_AUDACITY
            #define AUDACITY_DLL_API _declspec(dllexport)
         #else
            #ifdef _DLL
               #define AUDACITY_DLL_API _declspec(dllimport)
            #else
               #define AUDACITY_DLL_API
            #endif
         #endif
      #endif
   #endif //_MSC_VER

   /* The GCC-elf implementation */
   #ifdef HAVE_VISIBILITY // this is provided by the configure script, is only
   // enabled for suitable GCC versions
   /* The incantation is a bit weird here because it uses ELF symbol stuff. If we
    * make a symbol "default" it makes it visible (for import or export). Making it
    * "hidden" means it is invisible outside the shared object. */
      #ifndef AUDACITY_DLL_API
         #ifdef BUILDING_AUDACITY
            #define AUDACITY_DLL_API __attribute__((visibility("default")))
         #else
            #define AUDACITY_DLL_API __attribute__((visibility("default")))
         #endif
      #endif
   #endif

   /* The GCC-win32 implementation */
   // bizzarely, GCC-for-win32 supports Visual Studio style symbol visibility, so
   // we use that if building on Cygwin
   #if defined __CYGWIN__ && defined __GNUC__
      #ifndef AUDACITY_DLL_API
         #ifdef BUILDING_AUDACITY
            #define AUDACITY_DLL_API _declspec(dllexport)
         #else
            #ifdef _DLL
               #define AUDACITY_DLL_API _declspec(dllimport)
            #else
               #define AUDACITY_DLL_API
            #endif
         #endif
      #endif
   #endif
#endif

#endif // __AUDACITY_TYPES_H__
