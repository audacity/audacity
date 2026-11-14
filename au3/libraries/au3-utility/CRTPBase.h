/**********************************************************************

 Audacity: A Digital Audio Editor

 @file CRTPBase.h

 Paul Licameli

 **********************************************************************/
#ifndef __AUDACITY_CRTP_BASE__
#define __AUDACITY_CRTP_BASE__

#ifdef _WIN32
/*!
 When using Coplien's "Curiously Recurring Template Pattern" (CRTP), in which
 a derived class is a template argument of its own base class, this macro
 inserts the recommended annotation to make it link correctly on Windows.

 See:
 https://learn.microsoft.com/en-us/cpp/cpp/general-rules-and-limitations?view=msvc-170

 The first macro argument defines a type alias for the class described by
 the other arguments, which is a specialization of a template; typically the
 first template argument has been forward-declared, and then the complete
 definition uses the alias as the base class.

 A typical template specialization like `T<X, Y, Z>` parses as multiple macro
 arguments, so made it work with variadic macros.

 Write a semicolon after the macro call.
 */
   #define CRTP_BASE(alias, struct_or_class, ...) \
    template struct_or_class __declspec(dllexport) __VA_ARGS__; \
    using alias = __VA_ARGS__
#else
   #define CRTP_BASE(alias, struct_or_class, ...) \
    using alias = __VA_ARGS__
#endif

#endif
