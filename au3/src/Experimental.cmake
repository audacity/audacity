#[[

  Audacity: A Digital Audio Editor

  Experimental.cmake

  Dominic Mazzoni
  James Crook

  Used at configuration time to generate compiler options that enable or disable
  experimental features.

  When the features become mainstream the options will then be retired.

  JKC: This file solves a problem of how to avoid forking the
  code base when working on new features e.g:
    - Additional displays in Audacity
    - Modular architecture.
  Add options in here for the new features, and make your code
  conditional on them with #ifdef.

  For each name in the list, EXPERIMENTAL_{name} is the token to test with
  #ifdef.

  All the options are positive, i.e., when not commented out,
  they enable the feature.

]]

set( EXPERIMENTAL_OPTIONS_LIST

   # Edward Hui 1 Jul 2021
   #BRUSH_TOOL

   #MIDI_IN

   # JKC, 17 Aug 2017
   # Enables the MIDI note stretching feature, which currently
   # a) Is broken on Linux (Bug 1646)
   # b) Crashes with Sync-Lock (Bug 1719)
   # c) Needs UI design review.
   #MIDI_STRETCHING

   # USE_MIDI must be defined in order for SCOREALIGN to work
   #SCOREALIGN

   #Automatically tries to find an acceptable input volume
   #AUTOMATED_INPUT_LEVEL_ADJUSTMENT

   # Define to enable the device change handler
   #DEVICE_CHANGE_HANDLER

   # Paul Licameli (PRL) 28 Dec 2017
   # Easy drag-and-drop to add Nyquist, LADSPA, and VST plug-ins
   #DRAG_DROP_PLUG_INS

   # PRL 5 Jan 2018
   # Easy change of keystroke bindings for menu items
   #
   # PRL disabled 31 Aug 2021 because, at least on Mac, it misfires the
   # preference dialog whenever any keystroke shortcuts with Shift+ are used
   # EASY_CHANGE_KEY_BINDINGS
)

# Now define the flags
list( TRANSFORM EXPERIMENTAL_OPTIONS_LIST PREPEND "EXPERIMENTAL_"  )
add_compile_definitions( ${EXPERIMENTAL_OPTIONS_LIST} )
