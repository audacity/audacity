/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistFormatting.h

  Dominic Mazzoni

  Paul Licameli split from NyquistControls.h

**********************************************************************/

#ifndef __AUDACITY_EFFECT_NYQUIST_FORMATTING__
#define __AUDACITY_EFFECT_NYQUIST_FORMATTING__

#include "FileNames.h"
#include <iterator>
#include <type_traits>
#include <variant>

// Make sure to use the same typedefs as in xlisp for its data types
#include "../../../lib-src/libnyquist/nyquist/xlisp/xlisp.h"

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

struct Value;

//! A span of Values
struct Multiple {
   const Value *mBegin{};
   const size_t mSize{};

   Multiple() = default;
   Multiple(const Value *begin, size_t size)
      : mBegin{ begin }, mSize{ size }
   {}
   //! @pre end >= begin
   inline Multiple(const Value *begin, const Value *end);

   //! Construct from a contiguous container of const Value
   template<typename Container,
      // Deduced only for concept check
      typename Iterator = decltype(std::declval<const Container>().begin()),
      typename Traits = std::iterator_traits<Iterator>,
      typename = std::enable_if_t<
         std::is_same_v<const Value &, typename Traits::reference> &&
         // C++20 contiguous_iterator_tag
         std::is_base_of_v<std::random_access_iterator_tag,
            typename Traits::iterator_category>
      >
   >
   Multiple(const Container &c)
      : Multiple{ c.size() ? &*c.begin() : nullptr, c.size() } {}
};

//! Can construct implicitly from a std::vector or initializer_list
struct List : Multiple { using Multiple::Multiple; };

// Less often used than List and requires explicit construction
struct Vector : Multiple {
   template<typename Container>
   explicit Vector(const Container &c) : Multiple{ c } {}
};

struct Eval {
   explicit Eval(wxString form) : form{ form } {}
   explicit inline Eval(const Value& value);
   wxString form;
};

//! Alternatives that can be formatted for the Lisp reader
using ValueBase = std::variant<
   const std::monostate, // to make a symbol unbound
   const nullptr_t, // nil, distinct from unbound
   const bool, // Lisp t or nil
   const char,
   const FIXTYPE,
   const FLOTYPE,
   const char *const,
   const List, // construct list value as (list e1 e1 ...)
   const Vector, // construct vector value as (vector e1 e1 ...)
   const Eval  // just copy a string that is already formatted as a Lisp form
>;

struct Value : ValueBase {
   using ValueBase::ValueBase;

   //! Construct a value or a nil
   template<typename T>
   Value(std::optional<T> opt)
      : ValueBase(opt.has_value()
         ? ValueBase(*opt) : ValueBase(nullptr)
      )
   {}

   //! Can also construct a nil directly from std::nullopt
   Value(std::nullopt_t) : ValueBase{ nullptr } {}

   //! Allow implicit conversion
   Value(size_t size) : Value{ FIXTYPE(size) } {}

   //! Convert to a string that the XLisp reader understands
   wxString Format() const;
};

Eval::Eval(const Value &value) : Eval{ value.Format() } {}

Multiple::Multiple(const Value *begin, const Value *end)
   : Multiple{ begin, (size_t)(end - begin) }
{
   assert(end >= begin);
}

//! Make range-for work with Multiple
inline const Value *begin(Multiple multiple) { return multiple.mBegin; }
inline const Value *end(Multiple multiple)
   { return multiple.mBegin + multiple.mSize; }

//! Names a Lisp symbol
struct Symbol {
   Symbol() = default;
   Symbol(const wxString &name);
   Symbol(const char *name) : Symbol{ wxString{name} } {}
   Symbol(const wxChar *name) : Symbol{ wxString{name} } {}
   wxString mName;
};

struct Assignment {
   //! Generate a setf form
   Assignment(const Symbol &symbol, Value value)
      : value{ value }, symbol{ &symbol }, property{ nullptr }
   {}
   //! Generate a putprop form
   Assignment(const Symbol &symbol, Value value,
      const Symbol &property
   )  : value{ value }, symbol{ &symbol }, property{ &property }
   {}

   Assignment(Symbol&&, Value) = delete;
   Assignment(Symbol&&, Value, Symbol&&) = delete;

   const Value value;
   const Symbol *symbol;
   const Symbol *property;
};

//! Accumulates assignments to be made to Lisp variables and symbol properties
class Assignments {
public:
   using Arguments = std::initializer_list<Assignment>;

   Assignments() = default;
   Assignments(Arguments list);

   //! Accumulate one assignment to a variable
   void Append(Assignment assignment);

   //! Accumulate many assignments
   void Append(Arguments list)
      { mCommand += Assignments{ list }.Move(); }

   //! Access the accumulated string that Lisp can interpret as assignments
   const wxString &Get() const { return mCommand; }
   //! Move the string out
   wxString Move() && {
      wxString result;
      result.swap(mCommand);
      return result;
   }
   operator wxString() && { return std::move(*this).Move(); }
private:
   wxString mCommand;
};

}
#endif
