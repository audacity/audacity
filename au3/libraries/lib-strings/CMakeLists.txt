#[[
String wrapping types and internationalization support.

Identifier and specializations of TaggedIdentifier generate types of strings
used for different internal purposes, not meant for users to see (except
sometimes in macro programming), and the types do not implicitly interconvert.

TranslatableString holds the English msgid of a string meant for a user to see,
and can also bind formatting arguments.  It can generate a translation later,
even if the global choice of locale changes during its lifetime.  It does not
implicitly interconvert with wxString.

This library depends only on the wxBase subset of wxWidgets.
]]

set( SOURCES
   Base64.h
   Base64.cpp
   FutureStrings.h
   Identifier.cpp
   Identifier.h
   Internat.cpp
   Internat.h
   Languages.cpp
   Languages.h
   TranslatableString.cpp
   TranslatableString.h
   wxArrayStringEx.h
   wxArrayStringEx.cpp
)
set( LIBRARIES
   wxBase
)
audacity_library( lib-strings "${SOURCES}" "${LIBRARIES}"
   "" ""
)
