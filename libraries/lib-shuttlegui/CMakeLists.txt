#[[
A not really declarative system for describing dialog layouts, and also
alternative visitations of the layout description to construct a dialog, or to
shuttle values between its controls and program variables or preferences.

Some properties that this library lacks:

Declarativity instead of procedurality.  (See how examples of use still repeat
`S.` everywhere.  One should instead just construct a description object and
then pass it once to an interpreter object.)

Toolkit neutrality between wxWidgets and Qt (or other?)

Open-endedness of control types -- that is, non-intrusive extensions of the
system for more types

Recombination of control types with different abstract target variables that
can change the implementation of value storage (thus reducing the number of
functions the interface would require; no "Tie..." functions distinct from
"Add...")

Dialog definitions using Registry so that extensions of dialogs might be
defined, scattered, in plug-ins instead of hard-coded

Non-intrusive definition of more kinds of visitations of the dialog description
]]

set( SOURCES
   ReadOnlyText.h
   ShuttleGui.cpp
   ShuttleGui.h
   wxTextCtrlWrapper.h
)
set( LIBRARIES
   lib-command-parameters-interface
   PRIVATE
   lib-theme-interface
   lib-wx-wrappers-interface
)
audacity_library( lib-shuttlegui "${SOURCES}" "${LIBRARIES}"
   "" ""
)
