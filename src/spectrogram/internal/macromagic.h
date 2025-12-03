/*
 * Audacity: A Digital Audio Editor
 */

using teBmps = int; /// The index of a bitmap resource in SpectrogramColorRegister Resources.

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
#define DEFINE_IMAGE(name, initialiser, textual_name)  teBmps name=-1;
#define DEFINE_COLOUR(name, initialiser, textual_name) int name=-1;
#define SET_THEME_FLAGS(flags)
#undef THEME_INITS
#undef THEME_EXTERNS
#endif

#ifdef THEME_EXTERNS
#define DEFINE_IMAGE(name, initialiser, textual_name)  extern teBmps name;
#define DEFINE_COLOUR(name, initialiser, textual_name) extern int name;
#define SET_THEME_FLAGS(flags)
#endif
