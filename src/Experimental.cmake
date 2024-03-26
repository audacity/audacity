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
   # ACH 08 Jan 2014
   # EQ accelerated code
   #EQ_SSE_THREADED

   # USE_MIDI must be defined in order for SCOREALIGN to work
   #SCOREALIGN

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
