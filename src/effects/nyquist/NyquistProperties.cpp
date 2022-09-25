/**********************************************************************

  Audacity: A Digital Audio Editor

  @file NyquistProperties.cpp

  Paul Licameli split from NyquistProgram.cpp

**********************************************************************/
#include "NyquistProperties.h"
#include "../EffectBase.h"
#include "EffectInterface.h"
#include "../../LabelTrack.h"
#include "Languages.h"
#include "../../NoteTrack.h"
#include "Project.h"
#include "ProjectRate.h"
#include "TempDirectory.h"
#include "../../TimeTrack.h"
#include "../../WaveTrack.h"
#include "nyx.h"
#include <wx/numformatter.h>
#include <wx/stdpaths.h>

FilePaths NyquistProperties::GetNyquistSearchPath()
{
   const auto &audacityPathList = FileNames::AudacityPathList();
   FilePaths pathList;

   for (size_t i = 0; i < audacityPathList.size(); i++)
   {
      wxString prefix = audacityPathList[i] + wxFILE_SEP_PATH;
      FileNames::AddUniquePathToPathList(prefix + wxT("nyquist"), pathList);
      FileNames::AddUniquePathToPathList(prefix + wxT("plugins"), pathList);
      FileNames::AddUniquePathToPathList(prefix + wxT("plug-ins"), pathList);
   }
   pathList.push_back(FileNames::PlugInDir());

   return pathList;
}

wxString NyquistProperties::Global()
{
   using List = std::initializer_list<const NyquistFormatting::Value>;
   using Vec = std::vector<const NyquistFormatting::Value>;
   using NyquistFormatting::Eval;
   const NyquistFormatting::Symbol audacity{ "*audacity*" };
   const NyquistFormatting::Symbol separator{ "*decimal-separator*" };
   const NyquistFormatting::Symbol dir{ "*system-dir*" };
   const NyquistFormatting::Symbol time{ "*system-time*" };

   // Date and time:
   wxDateTime now = wxDateTime::Now();
   int year = now.GetYear();
   int doy = now.GetDayOfYear();
   int dom = now.GetDay();
   // enumerated constants
   wxDateTime::Month month = now.GetMonth();
   wxDateTime::WeekDay day = now.GetWeekDay();

   return NyquistFormatting::Assignments{
      { audacity, List{
         AUDACITY_VERSION, AUDACITY_RELEASE, AUDACITY_REVISION
      }, "version" },
      { audacity, []{
         auto lang = gPrefs->Read(wxT("/Locale/Language"), wxT(""));
         return (lang.empty())
            ? Languages::GetSystemLanguageCode(FileNames::AudacityPathList())
            : lang;
      }(), "language" },

      { separator, (char)wxNumberFormatter::GetDecimalSeparator() },

      { dir, FileNames::BaseDir(), "base" },
      { dir, FileNames::DataDir(), "data" },
      { dir, FileNames::HtmlHelpDir().RemoveLast(), "help" },
      { dir, TempDirectory::TempDir(), "temp" },
      { dir, wxStandardPaths::Get().GetTempDir(), "sys-temp" },
      { dir, wxStandardPaths::Get().GetDocumentsDir(), "documents" },
      { dir, wxGetHomeDir(), "home" },
      { dir, []{
         auto path = GetNyquistSearchPath();
         return Vec(path.begin(), path.end());
      }(), "plugin"},
      { dir, FileNames::PlugInDir(), "user-plugin" },
      //! Synonymous property names
      { dir, Eval{ "(get '" + dir.mName + " 'plugin)" }, "plug-in" },
      { dir, Eval{ "(get '" + dir.mName + " 'user-plugin)" },
         "user-plug-in" },

      // Date/time as a list: year, day of year, hour, minute, seconds
      { time, List{
         year, doy, now.GetHour(), now.GetMinute(), now.GetSecond() } },
      { time, now.FormatDate(), "date" },
      { time, now.FormatTime(), "time" },
      { time, now.FormatISODate(), "iso-date" },
      { time, now.FormatISOTime(), "iso-time" },
      { time, year, "year" },
      { time, dom, "day" }, // day of month
      { time, month, "month" },
      { time, now.GetMonthName(month), "month-name" },
      { time, now.GetWeekDayName(day), "day-name" },
   };
}

wxString NyquistProperties::Project(
   const AudacityProject &project, bool isPreviewing)
{
   const NyquistFormatting::Symbol proj{ "*project*" };
   const NyquistFormatting::Symbol previewp{ "*previewp*" };
   auto countRange = TrackList::Get(project).Leaders();
   return NyquistFormatting::Assignments{
      { proj, AllProjects{}.size(), "projects" },
      { proj, project.GetProjectName(), "name" },
      { proj, ProjectRate::Get(project).GetRate(), "rate" },

      { proj, countRange.size(), "tracks" },
      { proj, countRange.Filter<const WaveTrack>().size(), "wavetracks" },
      { proj, countRange.Filter<const LabelTrack>().size(), "labeltracks" },
#if defined(USE_MIDI)
      { proj, countRange.Filter<const NoteTrack>().size(), "miditracks" },
#endif
      { proj, countRange.Filter<const TimeTrack>().size(), "timetracks" },

      { proj, EffectsPreviewLen.Read(), "preview-duration" },

      // *previewp* is true when previewing (better than relying on track view).
      { previewp, isPreviewing },
   };
}

namespace {
// TODO: don't duplicate literals from xlinit.c sources, but put in a header
const NyquistFormatting::Symbol tracenable = "*tracenable*";
const NyquistFormatting::Symbol breakenable = "*breakenable*";
const NyquistFormatting::Symbol sal_traceback = "*sal-traceback*";
const NyquistFormatting::Symbol sal_compiler_debug = "*sal-compiler-debug*";
const NyquistFormatting::Symbol sal_call_stack = "*sal-call-stack*";
const NyquistFormatting::Symbol aud_result = "aud:result";

// Lisp symbol bound to the input track object
// Before version 4, "s" was used, but that collided with a constant defined
// in nyquist/nyquist.lsp (for sixteenth note) with value 0.25
const NyquistFormatting::Symbol oldTrack = "s";
// version 4 and later
const NyquistFormatting::Symbol newTrack = "*track*";

const NyquistFormatting::Symbol selection{ "*selection*" };
}

const NyquistFormatting::Assignment
NyquistProperties::restoreSixteenth{ oldTrack, 0.25 };

wxString NyquistProperties::Selection(const AudacityProject &project,
   const double t0, const double t1, const double f0, const double f1,
   const size_t numSelectedChannels)
{
   using Vec = std::vector<const NyquistFormatting::Value>;
   using std::optional;
   using std::nullopt;
   const auto hasF0 = (f0 >= 0.0);
   const auto hasF1 = (f1 >= 0.0);
   return NyquistFormatting::Assignments{
      { selection, t0, "start" },
      { selection, t1, "end" },
      // List of track positions of selected audio tracks
      { selection, [&]{
         int numTracks = 0;
         std::vector<int> positions;
         auto countRange = TrackList::Get(project).Leaders();
         for (auto t : countRange) {
            t->TypeSwitch( [&](const WaveTrack *) {
               if (t->GetSelected())
                  positions.push_back(1 + numTracks);
            });
            numTracks++;
         }
         return Vec(positions.begin(), positions.end());
      }(), "tracks"},
      { selection, numSelectedChannels, "channels" },

#if defined(EXPERIMENTAL_SPECTRAL_EDITING)
      { selection, hasF0 ? optional{f0} : nullopt, "low-hz" },
      { selection,
         // geometric mean
         hasF0 && hasF1 ? optional{sqrt(f0 * f1)} : nullopt,
         "center-hz" },
      { selection, hasF1 ? optional{f1} : nullopt, "high-hz" },
      { selection, [&]() -> optional<double> {
         // with very small values, bandwidth calculation may be inf.
         // (Observed on Linux)
         if ((f0 > 0.0) && (f1 >= f0)) {
            auto bw = log(f1 / f0) / log(2.0);
            if (!std::isinf(bw))
               return bw;
         }
         return nullopt;
      }(), "bandwidth" },
#endif
   };
}

wxString NyquistProperties::TrackNameAssignment(EffectType type, int version)
{
   NyquistFormatting::Assignments assignments;
   // A tool may be using AUD-DO which will potentially invalidate *TRACK*
   // so tools do not get *TRACK*.
   if (type == EffectTypeTool)
      assignments.Append(NyquistProperties::restoreSixteenth); // No Track.
   else {
      const NyquistFormatting::Symbol &track =
         version >= 4 ? newTrack : oldTrack;
      nyx_set_audio_name(track.mName);
      if (version >= 4)
         assignments.Append(NyquistProperties::restoreSixteenth);
      else
         assignments.Append({ track, {} }); // unbound
   }
   return std::move(assignments);
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
