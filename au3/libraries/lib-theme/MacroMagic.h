/**********************************************************************

  Audacity: A Digital Audio Editor

  MacroMagic.h

  James Crook

  Audacity is free software.
  This file is licensed under the wxWidgets license, see License.txt

********************************************************************//*!

\file MacroMagic.h

This file allows the same macros to do multiple duty by undefining
and redefining the macros.

For example DEFINE_IMAGE will generate:

  - extern int name;
  - int name = -1;
  - RegisterImage( allNames, myFlags, name, initialiser, textual_name);

On three different passes.  We control which by defining one of
THEME_INITS or THEME_DECLARATIONS or neither of these.


*//*******************************************************************/

using teBmps = int; /// The index of a bitmap resource in Theme Resources.

// undefine the macros.
#undef DEFINE_IMAGE
#undef DEFINE_COLOUR
#undef SET_THEME_FLAGS

#define THEME_EXTERNS

#ifdef THEME_INITS
#define DEFINE_IMAGE(name, initialiser, textual_name) \
    theTheme.RegisterImage(allNames, myFlags, name, initialiser, textual_name);
#define DEFINE_COLOUR(name, initialiser, textual_name) \
    theTheme.RegisterColour(allNames, name, initialiser, textual_name);
#define SET_THEME_FLAGS(flags) (myFlags = flags);
#undef THEME_DECLARATIONS
#undef THEME_EXTERNS
#endif

#ifdef THEME_DECLARATIONS
#define DEFINE_IMAGE(name, initialiser, textual_name)  THEME_API teBmps name=-1;
#define DEFINE_COLOUR(name, initialiser, textual_name) THEME_API int name=-1;
#define SET_THEME_FLAGS(flags)
#undef THEME_INITS
#undef THEME_EXTERNS
#endif

#ifdef THEME_EXTERNS
#define DEFINE_IMAGE(name, initialiser, textual_name)  extern THEME_API teBmps name;
#define DEFINE_COLOUR(name, initialiser, textual_name) extern THEME_API int name;
#define SET_THEME_FLAGS(flags)
#endif
