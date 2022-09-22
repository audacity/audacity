/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistProperties.h

  Paul Licameli split from NyquistProgram.cpp

**********************************************************************/
#ifndef __AUDACITY_EFFECT_NYQUIST_PROPERTIES__
#define __AUDACITY_EFFECT_NYQUIST_PROPERTIES__
class wxString;

namespace NyquistProperties {

// Format some assignments for the Lisp reader.  If not tracing,
// explicitly disable backtrace and prevent values
// from being carried through to the output.
// This should be the final command before evaluating the Nyquist script.
wxString TraceAssignments(bool trace, bool external);

// If tracing, this takes precedence over TraceAssignments
wxString SalCommand(bool trace, bool compiler, const wxString &program);
}
#endif
