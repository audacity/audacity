/**********************************************************************

  Audacity: A Digital Audio Editor

  Keyboard.h

  Dominic Mazzoni
  Brian Gunlogson

**********************************************************************/

#ifndef __AUDACITY_KEYBOARD__
#define __AUDACITY_KEYBOARD__

#include <wx/defs.h>
#include <wx/string.h> // to inherit

class wxKeyEvent;

struct NormalizedKeyString : private wxString
{
   NormalizedKeyString() = default;

   explicit NormalizedKeyString( const wxString &str );

   wxString Display(bool usesSpecialChars = false) const;

   const wxString &Raw() const { return *this; }

   bool NoCaseEqual( const NormalizedKeyString &other ) const
   { return 0 == this->Raw() .CmpNoCase( other.Raw() ); }

   using wxString::empty;
};

inline bool operator ==
( const NormalizedKeyString &a, const NormalizedKeyString &b)
{ return a.Raw () == b.Raw(); }

inline bool operator !=
( const NormalizedKeyString &a, const NormalizedKeyString &b)
{ return a.Raw () != b.Raw(); }

inline bool operator <
( const NormalizedKeyString &a, const NormalizedKeyString &b)
{ return a.Raw () < b.Raw(); }

NormalizedKeyString KeyEventToKeyString(const wxKeyEvent & keyEvent);

#endif
