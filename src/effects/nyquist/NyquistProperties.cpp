/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistProperties.cpp

  Paul Licameli split from NyquistProgram.cpp

**********************************************************************/
#include "NyquistProperties.h"
#include "NyquistFormatting.h"

namespace {
// TODO: don't duplicate literals from xlinit.c sources, but put in a header
const NyquistFormatting::Symbol tracenable = "*tracenable*";
const NyquistFormatting::Symbol breakenable = "*breakenable*";
const NyquistFormatting::Symbol sal_traceback = "*sal-traceback*";
const NyquistFormatting::Symbol sal_compiler_debug = "*sal-compiler-debug*";
const NyquistFormatting::Symbol sal_call_stack = "*sal-call-stack*";
const NyquistFormatting::Symbol aud_result = "aud:result";
}

wxString NyquistProperties::TraceAssignments(bool trace, bool external)
{
   NyquistFormatting::Assignments result{ { tracenable, trace } };
   if (trace && external)
      result.Append({ breakenable, true });
   return std::move(result);
}

wxString NyquistProperties::SalCommand(
   bool trace, bool compiler, const wxString &program)
{
   NyquistFormatting::Assignments assignments;
   if (trace)
      // since we're about to evaluate SAL, remove LISP trace enable and
      // break enable (which stops SAL processing) and turn on SAL stack
      // trace
      assignments.Append({ { tracenable, false }, { breakenable, false },
         { sal_traceback, true } });
   if (compiler)
      assignments.Append({ sal_compiler_debug, true });
   assignments.Append({
      { sal_call_stack, nullptr },
      // if we do not set this here and an error occurs in main, another
      // error will be raised when we try to return the value of aud:result
      // which is unbound
      { aud_result, nullptr } });
   // mCmd was not given in Lisp syntax but SAL;
   // append one assignment to it in that syntax
   const auto str = NyquistFormatting::EscapeString(program) +
      // this is tricky: we need SAL to call main so that we can get a
      // SAL traceback in the event of an error (sal-compile catches the
      // error and calls sal-error-output), but SAL does not return values.
      // We will catch the value in a special global aud:result and if no
      // error occurs, we will grab the value with a LISP expression
      wxT("\nset aud:result = main()\n");
   // Compose a Lisp command that interprets SAL
   auto command =
      wxString("(sal-compile-audacity \"") + str + wxT("\" t t nil)\n")
   // Capture the value returned by main (saved in aud:result), but
   // set aud:result to nil so sound results can be evaluated without
   // retaining audio in memory
   // (That is, aud:result can be garbage collected after NyquistProgram is
   // finished with it)
   // (Note Lisp prog1 evaluates a sequence of forms, returning result of first,
   // doing the rest only for side-effect)
      + wxT("(prog1 aud:result (setf aud:result nil))\n");
   return std::move(assignments) + command;
}
