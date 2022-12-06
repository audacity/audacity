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
)

# Now define the flags
list( TRANSFORM EXPERIMENTAL_OPTIONS_LIST PREPEND "EXPERIMENTAL_"  )
add_compile_definitions( ${EXPERIMENTAL_OPTIONS_LIST} )
