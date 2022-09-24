/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistProperties.h

  Paul Licameli split from NyquistProgram.cpp

**********************************************************************/
#ifndef __AUDACITY_EFFECT_NYQUIST_PROPERTIES__
#define __AUDACITY_EFFECT_NYQUIST_PROPERTIES__

#include "NyquistFormatting.h"
enum EffectType : int;

namespace NyquistProperties {
FilePaths GetNyquistSearchPath();

wxString Global();

//! Assignment to fix the sixteenth note because of a variable name collision
//! in older versions of Nyquist
extern const NyquistFormatting::Assignment restoreSixteenth;

wxString TrackNameAssignment(EffectType type, int version);

// Format some assignments for the Lisp reader.  If not tracing,
// explicitly disable backtrace and prevent values
// from being carried through to the output.
// This should be the final command before evaluating the Nyquist script.
wxString TraceAssignments(bool trace, bool external);

// If tracing, this takes precedence over TraceAssignments
wxString SalCommand(bool trace, bool compiler, const wxString &program);
}
#endif
