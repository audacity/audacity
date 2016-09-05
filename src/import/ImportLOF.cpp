/**********************************************************************

  Audacity: A Digital Audio Editor

  ImportLOF.h

  David I. Murray
  Leland Lucius

*//****************************************************************//**

\class LOFImportFileHandle
\brief An ImportFileHandle for LOF data

  Supports the opening of ".lof" files which are text files that contain
  a list of individual files to open in audacity in specific formats. Files may
  be file names (in the same directory as the LOF file), absolute paths or
  relative paths relative to the directory of the LOF file.

  (In BNF) The syntax for an LOF file, denoted by <lof>:

\verbatim
  <lof> ::= [<window> | <file> | <#>]*
  <window> ::= window [<window-parameter>]* <newline>
  <window-parameter> ::= offset <time> | duration <time>
  <time> ::= [<digit>]+ [ . [<digit>]* ]
  <file> ::= file [<file-parameter>]* <newline>
  <file-parameter> ::= offset <time>
  <#> ::= <comment> <newline>
\endverbatim

  EXAMPLE LOF file:

\verbatim
  # everything following the hash character is ignored
  window # an initial window command is implicit and optional
  file "C:\folder1\sample1.wav"    # sample1.wav is displayed
  file "C:\sample2.wav" offset 5   # sample2 is displayed with a 5s offset
  File "C:\sample3.wav"            # sample3 is displayed with no offset
  File "foo.aiff" # foo is loaded from the same directory as the LOF file
  window offset 5 duration 10      # open a NEW window, zoom to display
  # 10 seconds total starting at 5 (ending at 15) seconds
  file "C:\sample3.wav" offset 2.5
\endverbatim

  SEMANTICS:

  There are two commands: "window" creates a NEW window, and "file"
  appends a track to the current window and displays the file there. The
  first file is always placed in a NEW window, whether or not an initial
  "window" command is given.

  Commands have optional keyword parameters that may be listed in any
  order. A parameter should only occur once per command. The "offset"
  parameter specifies a time offset. For windows, this is the leftmost
  time displayed in the window. For files, the offset is an amount by
  which the file is shifted in time before display (only enabled for audio;
  not midi). The offset is specified as an integer or decimal number of
  seconds, and the default value is zero.

  Windows may also have a "duration" parameter, which specifies how much
  time should be displayed in the window. The default duration is equal
  to the duration of the longest track currently displayed.

*//****************************************************************//**

\class LOFImportPlugin
\brief An ImportPlugin for LOF data

*//*******************************************************************/

#include "../Audacity.h"
#include "ImportLOF.h"

#include <wx/string.h>
#include <wx/utils.h>
#include <wx/intl.h>
#include <wx/textfile.h>
#include <wx/msgdlg.h>
#include <wx/tokenzr.h>

#ifdef USE_MIDI
#include "ImportMIDI.h"
#endif // USE_MIDI
#include "../WaveTrack.h"
#include "ImportPlugin.h"
#include "Import.h"
#include "../Internat.h"
#include "../NoteTrack.h"
#include "../Project.h"
#include "../FileFormats.h"
#include "../Prefs.h"
#include "../Internat.h"

#define BINARY_FILE_CHECK_BUFFER_SIZE 1024

#define DESC _("List of Files in basic text format")

static const wxChar *exts[] =
{
   wxT("lof")
};

class LOFImportPlugin final : public ImportPlugin
{
public:
   LOFImportPlugin()
   :  ImportPlugin(wxArrayString(WXSIZEOF(exts), exts))
   {
   }

   ~LOFImportPlugin() { }

   wxString GetPluginStringID() { return wxT("lof"); }
   wxString GetPluginFormatDescription();
   std::unique_ptr<ImportFileHandle> Open(const wxString &Filename) override;
};


class LOFImportFileHandle final : public ImportFileHandle
{
public:
   LOFImportFileHandle(const wxString & name, std::unique_ptr<wxTextFile> &&file);
   ~LOFImportFileHandle();

   wxString GetFileDescription();
   ByteCount GetFileUncompressedBytes() override;
   int Import(TrackFactory *trackFactory, TrackHolders &outTracks,
              Tags *tags) override;

   wxInt32 GetStreamCount(){ return 1; }

   const wxArrayString &GetStreamInfo() override
   {
      static wxArrayString empty;
      return empty;
   }

   void SetStreamUsage(wxInt32 WXUNUSED(StreamID), bool WXUNUSED(Use)){}

private:
   // Takes a line of text in lof file and interprets it and opens files
   void lofOpenFiles(wxString* ln);
   void doDuration();
   void doScrollOffset();

   std::unique_ptr<wxTextFile> mTextFile;
   wxFileName mLOFFileName;  /**< The name of the LOF file, which is used to
                                interpret relative paths in it */
   AudacityProject *mProject;

   // In order to know whether or not to create a NEW window
   bool              windowCalledOnce;

   // In order to zoom in, it must be done after files are opened
   bool              callDurationFactor;
   double            durationFactor;

   // In order to offset scrollbar, it must be done after files are opened
   bool              callScrollOffset;
   double            scrollOffset;
};

LOFImportFileHandle::LOFImportFileHandle
   (const wxString & name, std::unique_ptr<wxTextFile> &&file)
:  ImportFileHandle(name),
   mTextFile(std::move(file))
   , mLOFFileName{name}
{
   mProject = GetActiveProject();
   windowCalledOnce = false;
   callDurationFactor = false;
   durationFactor = 1;
   callScrollOffset = false;
   scrollOffset = 0;
}

void GetLOFImportPlugin(ImportPluginList &importPluginList,
                        UnusableImportPluginList & WXUNUSED(unusableImportPluginList))
{
   importPluginList.push_back( make_movable<LOFImportPlugin>() );
}

wxString LOFImportPlugin::GetPluginFormatDescription()
{
    return DESC;
}

std::unique_ptr<ImportFileHandle> LOFImportPlugin::Open(const wxString &filename)
{
   // Check if it is a binary file
   wxFile binaryFile;
   if (!binaryFile.Open(filename))
      return nullptr; // File not found

   char buf[BINARY_FILE_CHECK_BUFFER_SIZE];
   int count = binaryFile.Read(buf, BINARY_FILE_CHECK_BUFFER_SIZE);

   for (int i = 0; i < count; i++)
   {
      // Check if this char is below the space character, but not a
      // line feed or carriage return
      if (buf[i] < 32 && buf[i] != 10 && buf[i] != 13)
      {
         // Assume it is a binary file
         binaryFile.Close();
         return nullptr;
      }
   }

   // Close it again so it can be opened as a text file
   binaryFile.Close();

   // Now open the file again as text file
   auto file = std::make_unique<wxTextFile>(filename);
   file->Open();

   if (!file->IsOpened())
      return nullptr;

   return std::make_unique<LOFImportFileHandle>(filename, std::move(file));
}

wxString LOFImportFileHandle::GetFileDescription()
{
   return DESC;
}

auto LOFImportFileHandle::GetFileUncompressedBytes() -> ByteCount
{
   return 0;
}

int LOFImportFileHandle::Import(TrackFactory * WXUNUSED(trackFactory), TrackHolders &outTracks,
                                Tags * WXUNUSED(tags))
{
   outTracks.clear();

   wxASSERT(mTextFile->IsOpened());

   if(mTextFile->Eof())
   {
      mTextFile->Close();
      return eProgressFailed;
   }

   wxString line = mTextFile->GetFirstLine();

   while (!mTextFile->Eof())
   {
      lofOpenFiles(&line);
      line = mTextFile->GetNextLine();
   }

   // for last line
   lofOpenFiles(&line);

   // set any duration/offset factors for last window, as all files were called
   doDuration();
   doScrollOffset();

   // exited ok
   if(mTextFile->Close())
      return eProgressSuccess;

   return eProgressFailed;
}

static int CountNumTracks(AudacityProject *proj)
{
   int count = 0;
   Track *t;
   TrackListIterator iter(proj->GetTracks());

   t = iter.First();

   while(t) {
      count++;
      t = iter.Next();
   }

   return count;
}

/** @brief Processes a single line from a LOF text file, doing whatever is
 * indicated on the line.
 *
 * This function should just return for lines it cannot deal with, and the
 * caller will continue to the next line of the input file
 */
void LOFImportFileHandle::lofOpenFiles(wxString* ln)
{
   wxStringTokenizer tok(*ln, wxT(" "));
   wxStringTokenizer temptok1(*ln, wxT("\""));
   wxStringTokenizer temptok2(*ln, wxT(" "));
   int tokenplace = 0;

   wxString targetfile;
   wxString tokenholder = tok.GetNextToken();

   if (tokenholder.IsSameAs(wxT("window"), false))
   {
      // set any duration/offset factors for last window, as all files were called
      doDuration();
      doScrollOffset();

      if (windowCalledOnce)
      {
         mProject = CreateNewAudacityProject();
      }

      windowCalledOnce = true;

      while (tok.HasMoreTokens())
      {
         tokenholder = tok.GetNextToken();

         if (tokenholder.IsSameAs(wxT("offset"), false))
         {
            if (tok.HasMoreTokens())
               tokenholder = tok.GetNextToken();

            if (Internat::CompatibleToDouble(tokenholder, &scrollOffset))
            {
               callScrollOffset = true;
            }
            else
            {
               /* i18n-hint: You do not need to translate "LOF" */
               wxMessageBox(_("Invalid window offset in LOF file."),
                            /* i18n-hint: You do not need to translate "LOF" */
                            _("LOF Error"), wxOK | wxCENTRE);
            }

            if (tok.HasMoreTokens())
               tokenholder = tok.GetNextToken();
         }

         if (tokenholder.IsSameAs(wxT("duration"), false))
         {
            if (tok.HasMoreTokens())
               tokenholder = tok.GetNextToken();

            if (Internat::CompatibleToDouble(tokenholder, &durationFactor))
            {
               callDurationFactor = true;
            }
            else
            {
               /* i18n-hint: You do not need to translate "LOF" */
               wxMessageBox(_("Invalid duration in LOF file."),
                            /* i18n-hint: You do not need to translate "LOF" */
                            _("LOF Error"), wxOK | wxCENTRE);
            }
         }     // End if statement

         if (tokenholder.IsSameAs(wxT("#")))
         {
            // # indicates comments; ignore line
            tok = wxStringTokenizer(wxT(""), wxT(" "));
         }
      }     // End while loop
   }        // End if statement handling "window" lines

   else if (tokenholder.IsSameAs(wxT("file"), false))
   {

      // To identify filename and open it
      tokenholder = temptok1.GetNextToken();
      targetfile = temptok1.GetNextToken();

      // If path is relative, make absolute path from LOF path
      if(!wxIsAbsolutePath(targetfile)) {
         wxFileName fName(targetfile);
         fName.Normalize(wxPATH_NORM_ALL, mLOFFileName.GetPath(wxPATH_GET_VOLUME | wxPATH_GET_SEPARATOR));
         if(fName.FileExists()) {
            targetfile = fName.GetFullPath();
         }
      }

      #ifdef USE_MIDI
      // If file is a midi
      if (targetfile.AfterLast(wxT('.')).IsSameAs(wxT("mid"), false)
          ||  targetfile.AfterLast(wxT('.')).IsSameAs(wxT("midi"), false))
      {
         mProject->DoImportMIDI(targetfile);
      }

      // If not a midi, open audio file
      else
      {
      #else // !USE_MIDI
         /* if we don't have midi support, go straight on to opening as an
          * audio file. TODO: Some sort of message here? */
      {
      #endif // USE_MIDI
         mProject->OpenFile(targetfile);
      }

      // Set tok to right after filename
      temptok2.SetString(targetfile);
      tokenplace = temptok2.CountTokens();

      for (int i = 0; i < tokenplace; i++)
         tokenholder = tok.GetNextToken();

      if (tok.HasMoreTokens())
      {
         tokenholder = tok.GetNextToken();

         if (tokenholder.IsSameAs(wxT("#")))
         {
            // # indicates comments; ignore line
            tok = wxStringTokenizer(wxT(""), wxT(" "));
         }

         if (tokenholder.IsSameAs(wxT("offset"), false))
         {
            if (tok.HasMoreTokens())
               tokenholder = tok.GetNextToken();
            double offset;

            // handle an "offset" specifier
            if (Internat::CompatibleToDouble(tokenholder, &offset))
            {
               Track *t;
               TrackListIterator iter(mProject->GetTracks());

               t = iter.First();

               for (int i = 1; i < CountNumTracks(mProject) - 1; i++)
                  t = iter.Next();

               // t is now the last track in the project, unless the import of
               // all tracks failed, in which case it will be null. In that
               // case we return because we cannot offset a non-existent track.
               if (t == NULL) return;
#ifdef USE_MIDI
               if (targetfile.AfterLast(wxT('.')).IsSameAs(wxT("mid"), false) ||
                   targetfile.AfterLast(wxT('.')).IsSameAs(wxT("midi"), false))
               {
                  wxMessageBox(_("MIDI tracks cannot be offset individually, only audio files can be."),
                               _("LOF Error"), wxOK | wxCENTRE);
               }
               else
#endif
               {
                  if (CountNumTracks(mProject) == 1)
                     t->SetOffset(offset);
                  else
                  {
                     if (t->GetLinked())
                        t->SetOffset(offset);

                     t = iter.Next();
                     t->SetOffset(offset);
                  }
               }
            } // end of converting "offset" argument
            else
            {
               /* i18n-hint: You do not need to translate "LOF" */
               wxMessageBox(_("Invalid track offset in LOF file."),
                            _("LOF Error"), wxOK | wxCENTRE);
            }
         }     // End if statement for "offset" parameters
      }     // End if statement (more tokens after file name)
   }     // End if statement "file" lines

   else if (tokenholder.IsSameAs(wxT("#")))
   {
      // # indicates comments; ignore line
      tok = wxStringTokenizer(wxT(""), wxT(" "));
   }
   else
   {
      // Couldn't parse a line
   }
}

void LOFImportFileHandle::doDuration()
{
   if (callDurationFactor)
   {
      double longestDuration = mProject->GetTracks()->GetEndTime();
      mProject->ZoomBy(longestDuration / durationFactor);
      callDurationFactor = false;
   }
}

void LOFImportFileHandle::doScrollOffset()
{
   if (callScrollOffset && (scrollOffset != 0))
   {
      mProject->TP_ScrollWindow(scrollOffset);
      callScrollOffset = false;
   }
}

LOFImportFileHandle::~LOFImportFileHandle()
{
}
