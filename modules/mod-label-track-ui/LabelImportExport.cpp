/**********************************************************************

  Audacity: A Digital Audio Editor

  @file LabelImportExport.cpp

  Paul Licameli split from LabelTrack.cpp

**********************************************************************/

#include "LabelImportExport.h"
#include "LabelTrack.h"
#include <float.h>
#include <wx/tokenzr.h>
#include "prefs/ImportExportPrefs.h"
#include "widgets/AudacityMessageBox.h"

#include <wx/textfile.h>

namespace {
struct BadFormatException {};
}

LabelStruct ImportLabelStruct(wxTextFile &file, int &index)
{
   SelectedRegion sr;
   wxString title;
   static const wxString continuation{ wxT("\\") };

   wxString firstLine = file.GetLine(index++);

   {
      // Assume tab is an impossible character within the exported text
      // of the label, so can be only a delimiter.  But other white space may
      // be part of the label text.
      wxStringTokenizer toker { firstLine, wxT("\t") };

      //get the timepoint of the left edge of the label.
      auto token = toker.GetNextToken();

      double t0;
      if (!Internat::CompatibleToDouble(token, &t0))
         throw BadFormatException{};

      token = toker.GetNextToken();

      double t1;
      if (!Internat::CompatibleToDouble(token, &t1))
         //s1 is not a number.
         t1 = t0;  //This is a one-sided label; t1 == t0.
      else
         token = toker.GetNextToken();

      sr.setTimes( t0, t1 );
      
      title = token;
   }

   // Newer selection fields are written on additional lines beginning with
   // '\' which is an impossible numerical character that older versions of
   // audacity will ignore.  Test for the presence of such a line and then
   // parse it if we can.

   // There may also be additional continuation lines from future formats that
   // we ignore.

   // Advance index over all continuation lines first, before we might throw
   // any exceptions.
   int index2 = index;
   while (index < (int)file.GetLineCount() &&
          file.GetLine(index).StartsWith(continuation))
      ++index;

   if (index2 < index) {
      wxStringTokenizer toker { file.GetLine(index2++), wxT("\t") };
      auto token = toker.GetNextToken();
      if (token != continuation)
         throw BadFormatException{};

      token = toker.GetNextToken();
      double f0;
      if (!Internat::CompatibleToDouble(token, &f0))
         throw BadFormatException{};

      token = toker.GetNextToken();
      double f1;
      if (!Internat::CompatibleToDouble(token, &f1))
         throw BadFormatException{};

      sr.setFrequencies(f0, f1);
   }

   return LabelStruct{ sr, title };
}

void ExportLabelStruct(const LabelStruct &label, wxTextFile &file)
{
   file.AddLine(wxString::Format(wxT("%s\t%s\t%s"),
      Internat::ToString(label.getT0(), FLT_DIG),
      Internat::ToString(label.getT1(), FLT_DIG),
      label.title
   ));

   // Do we need more lines?
   auto f0 = label.selectedRegion.f0();
   auto f1 = label.selectedRegion.f1();
   if ((f0 == SelectedRegion::UndefinedFrequency &&
      f1 == SelectedRegion::UndefinedFrequency) ||
      ImportExportPrefs::LabelStyleSetting.ReadEnum())
      return;

   // Write a \ character at the start of a second line,
   // so that earlier versions of Audacity ignore it.
   file.AddLine(wxString::Format(wxT("\\\t%s\t%s"),
      Internat::ToString(f0, FLT_DIG),
      Internat::ToString(f1, FLT_DIG)
   ));

   // Additional lines in future formats should also start with '\'.
}

/// Export labels including label start and end-times.
void ExportLabelTrack(const LabelTrack &track, wxTextFile & f)
{
   // PRL: to do: export other selection fields
   for (auto &labelStruct: track.GetLabels())
      ExportLabelStruct(labelStruct, f);
}

/// Import labels, handling files with or without end-times.
void ImportLabelTrack(LabelTrack &track, wxTextFile & in)
{
   int lines = in.GetLineCount();

   track.Clear(track.GetStartTime() - 1, track.GetEndTime() + 1);

   //Currently, we expect a tag file to have two values and a label
   //on each line. If the second token is not a number, we treat
   //it as a single-value label.
   bool error = false;
   for (int index = 0; index < lines;) {
      try {
         // Let LabelStruct::Import advance index
         LabelStruct l { ImportLabelStruct(in, index) };
         track.AddLabel(l.selectedRegion, l.title);
      }
      catch(const BadFormatException&) { error = true; }
   }
   if (error)
      ::AudacityMessageBox( XO("One or more saved labels could not be read.") );
   track.SortLabels();
}
