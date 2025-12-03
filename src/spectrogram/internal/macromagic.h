/*
 * Audacity: A Digital Audio Editor
 */

using teBmps = int; /// The index of a bitmap resource in SpectrogramColorRegister Resources.

// undefine the macros.
#undef DEFINE_IMAGE
#undef DEFINE_COLOUR
#undef SET_SPECTROGRAM_COLORS_FLAGS

#define SPECTROGRAM_COLORS_EXTERNS

#ifdef SPECTROGRAM_COLORS_INITS
#define DEFINE_IMAGE(name, initialiser, textual_name) \
    spectrogramColorRegister.RegisterImage(allNames, myFlags, name, initialiser, textual_name);
#define DEFINE_COLOUR(name, initialiser, textual_name) \
    spectrogramColorRegister.RegisterColour(allNames, name, initialiser, textual_name);
#define SET_SPECTROGRAM_COLORS_FLAGS(flags) (myFlags = flags);
#undef SPECTROGRAM_COLORS_DECLARATIONS
#undef SPECTROGRAM_COLORS_EXTERNS
#endif

#ifdef SPECTROGRAM_COLORS_DECLARATIONS
#define DEFINE_IMAGE(name, initialiser, textual_name)  teBmps name=-1;
#define DEFINE_COLOUR(name, initialiser, textual_name) int name=-1;
#define SET_SPECTROGRAM_COLORS_FLAGS(flags)
#undef SPECTROGRAM_COLORS_INITS
#undef SPECTROGRAM_COLORS_EXTERNS
#endif

#ifdef SPECTROGRAM_COLORS_EXTERNS
#define DEFINE_IMAGE(name, initialiser, textual_name)  extern teBmps name;
#define DEFINE_COLOUR(name, initialiser, textual_name) extern int name;
#define SET_SPECTROGRAM_COLORS_FLAGS(flags)
#endif
