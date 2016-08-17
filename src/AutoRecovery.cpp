/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   AutoRecovery.cpp

*******************************************************************//**

\class AutoRecoveryDialog
\brief The AutoRecoveryDialog prompts the user whether to
recover previous Audacity projects that were closed incorrectly.

*//********************************************************************/

#include "AutoRecovery.h"
#include "Audacity.h"
#include "AudacityApp.h"
#include "FileNames.h"
#include "blockfile/SimpleBlockFile.h"
#include "Sequence.h"
#include "ShuttleGui.h"

#include <wx/wxprec.h>
#include <wx/filefn.h>
#include <wx/dir.h>
#include <wx/dialog.h>
#include <wx/app.h>

#include "WaveTrack.h"

enum {
   ID_RECOVER_ALL = 10000,
   ID_RECOVER_NONE,
   ID_QUIT_AUDACITY,
   ID_FILE_LIST
};

class AutoRecoveryDialog final : public wxDialogWrapper
{
public:
   AutoRecoveryDialog(wxWindow *parent);

private:
   void PopulateList();
   void PopulateOrExchange(ShuttleGui & S);

   void OnQuitAudacity(wxCommandEvent &evt);
   void OnRecoverNone(wxCommandEvent &evt);
   void OnRecoverAll(wxCommandEvent &evt);

   wxListCtrl *mFileList;

public:
   DECLARE_EVENT_TABLE()
};

AutoRecoveryDialog::AutoRecoveryDialog(wxWindow *parent) :
   wxDialogWrapper(parent, -1, _("Automatic Crash Recovery"),
            wxDefaultPosition, wxDefaultSize,
            wxDEFAULT_DIALOG_STYLE & (~wxCLOSE_BOX)) // no close box
{
   SetName(GetTitle());
   ShuttleGui S(this, eIsCreating);
   PopulateOrExchange(S);
}

BEGIN_EVENT_TABLE(AutoRecoveryDialog, wxDialogWrapper)
   EVT_BUTTON(ID_RECOVER_ALL, AutoRecoveryDialog::OnRecoverAll)
   EVT_BUTTON(ID_RECOVER_NONE, AutoRecoveryDialog::OnRecoverNone)
   EVT_BUTTON(ID_QUIT_AUDACITY, AutoRecoveryDialog::OnQuitAudacity)
END_EVENT_TABLE()

void AutoRecoveryDialog::PopulateOrExchange(ShuttleGui& S)
{
   S.SetBorder(5);
   S.StartVerticalLay();
   {
      S.AddVariableText(_("Some projects were not saved properly the last time Audacity was run.\nFortunately, the following projects can be automatically recovered:"), false);

      S.StartStatic(_("Recoverable projects"));
      {
         mFileList = S.Id(ID_FILE_LIST).AddListControlReportMode();
         /*i18n-hint: (noun).  It's the name of the project to recover.*/
         mFileList->InsertColumn(0, _("Name"));
         mFileList->SetColumnWidth(0, wxLIST_AUTOSIZE);
         PopulateList();
      }
      S.EndStatic();

      S.AddVariableText(_("After recovery, save the project to save the changes to disk."), false);

      S.StartHorizontalLay();
      {
         S.Id(ID_QUIT_AUDACITY).AddButton(_("Quit Audacity"));
         S.Id(ID_RECOVER_NONE).AddButton(_("Discard Projects"));
         S.Id(ID_RECOVER_ALL).AddButton(_("Recover Projects"));
      }
      S.EndHorizontalLay();
   }
   S.EndVerticalLay();

   Layout();
   Fit();
   SetMinSize(GetSize());

   // Sometimes it centers on wxGTK and sometimes it doesn't.
   // Yielding before centering seems to be a good workaround,
   // but will leave to implement on a rainy day.
   Center();
}

void AutoRecoveryDialog::PopulateList()
{
   mFileList->DeleteAllItems();

   wxDir dir(FileNames::AutoSaveDir());
   if (!dir.IsOpened())
      return;

   wxString filename;
   int i = 0;
   for (bool c = dir.GetFirst(&filename, wxT("*.autosave"), wxDIR_FILES);
        c; c = dir.GetNext(&filename))
        mFileList->InsertItem(i++, wxFileName{ filename }.GetName());

   mFileList->SetColumnWidth(0, wxLIST_AUTOSIZE);
}

void AutoRecoveryDialog::OnQuitAudacity(wxCommandEvent & WXUNUSED(event))
{
   EndModal(ID_QUIT_AUDACITY);
}

void AutoRecoveryDialog::OnRecoverNone(wxCommandEvent & WXUNUSED(event))
{
   int ret = wxMessageBox(
      _("Are you sure you want to discard all projects?\n\nChoosing \"Yes\" discards all projects immediately."),
      _("Confirm Discard Projects"), wxICON_QUESTION | wxYES_NO | wxNO_DEFAULT, this);

   if (ret == wxYES)
      EndModal(ID_RECOVER_NONE);
}

void AutoRecoveryDialog::OnRecoverAll(wxCommandEvent & WXUNUSED(event))
{
   EndModal(ID_RECOVER_ALL);
}

////////////////////////////////////////////////////////////////////////////

static bool HaveFilesToRecover()
{
   wxDir dir(FileNames::AutoSaveDir());
   if (!dir.IsOpened())
   {
      wxMessageBox(_("Could not enumerate files in auto save directory."),
                   _("Error"), wxICON_STOP);
      return false;
   }

   wxString filename;
   bool c = dir.GetFirst(&filename, wxT("*.autosave"), wxDIR_FILES);

   return c;
}

static bool RemoveAllAutoSaveFiles()
{
   wxArrayString files;
   wxDir::GetAllFiles(FileNames::AutoSaveDir(), &files,
                      wxT("*.autosave"), wxDIR_FILES);

   for (unsigned int i = 0; i < files.GetCount(); i++)
   {
      if (!wxRemoveFile(files[i]))
      {
         // I don't think this error message is actually useful.
         // -dmazzoni
         //wxMessageBox(wxT("Could not remove auto save file: " + files[i]),
         //             _("Error"), wxICON_STOP);
         return false;
      }
   }

   return true;
}

static bool RecoverAllProjects(AudacityProject** pproj)
{
   wxDir dir(FileNames::AutoSaveDir());
   if (!dir.IsOpened())
   {
      wxMessageBox(_("Could not enumerate files in auto save directory."),
                   _("Error"), wxICON_STOP);
      return false;
   }

   // Open a project window for each auto save file
   wxString filename;
   AudacityProject* proj = NULL;

   wxArrayString files;
   wxDir::GetAllFiles(FileNames::AutoSaveDir(), &files,
                      wxT("*.autosave"), wxDIR_FILES);

   for (unsigned int i = 0; i < files.GetCount(); i++)
   {
      if (*pproj)
      {
         // Reuse existing project window
         proj = *pproj;
         *pproj = NULL;
      } else
      {
         // Create NEW project window
         proj = CreateNewAudacityProject();
      }

      // Open project. When an auto-save file has been opened successfully,
      // the opened auto-save file is automatically deleted and a NEW one
      // is created.
      proj->OpenFile(files[i], false);
   }

   return true;
}

bool ShowAutoRecoveryDialogIfNeeded(AudacityProject** pproj,
                                    bool *didRecoverAnything)
{
   if (didRecoverAnything)
      *didRecoverAnything = false;
   if (HaveFilesToRecover())
   {
      // Under wxGTK3, the auto recovery dialog will not get
      // the focus since the project window hasn't been allowed
      // to completely initialize.
      //
      // Yielding seems to allow the initialization to complete.
      //
      // Additionally, it also corrects a sizing issue in the dialog
      // related to wxWidgets bug:
      //
      //    http://trac.wxwidgets.org/ticket/16440
      //
      // This must be done before "dlg" is declared.
      wxEventLoopBase::GetActive()->YieldFor(wxEVT_CATEGORY_UI);

      int ret = AutoRecoveryDialog{nullptr}.ShowModal();

      switch (ret)
      {
      case ID_RECOVER_NONE:
         return RemoveAllAutoSaveFiles();

      case ID_RECOVER_ALL:
         if (didRecoverAnything)
            *didRecoverAnything = true;
         return RecoverAllProjects(pproj);

      default:
         // This includes ID_QUIT_AUDACITY
         return false;
      }
   } else
   {
      // Nothing to recover, move along
      return true;
   }
}

////////////////////////////////////////////////////////////////////////////
/// Recording recovery handler

RecordingRecoveryHandler::RecordingRecoveryHandler(AudacityProject* proj)
{
   mProject = proj;
   mChannel = -1;
   mNumChannels = -1;
}

int RecordingRecoveryHandler::FindTrack() const
{
   WaveTrackArray tracks = mProject->GetTracks()->GetWaveTrackArray(false);
   int index;
   if (mAutoSaveIdent)
   {
      for (index = 0; index < (int)tracks.size(); index++)
      {
         if (tracks[index]->GetAutoSaveIdent() == mAutoSaveIdent)
         {
            break;
         }
      }
   }
   else
   {
      index = tracks.size() - mNumChannels + mChannel;
   }

   return index;
}

bool RecordingRecoveryHandler::HandleXMLTag(const wxChar *tag,
                                            const wxChar **attrs)
{
   if (wxStrcmp(tag, wxT("simpleblockfile")) == 0)
   {
      // Check if we have a valid channel and numchannels
      if (mChannel < 0 || mNumChannels < 0 || mChannel >= mNumChannels)
      {
         // This should only happen if there is a bug
         wxASSERT(false);
         return false;
      }

      WaveTrackArray tracks = mProject->GetTracks()->GetWaveTrackArray(false);
      int index = FindTrack();
      // We need to find the track and sequence where the blockfile belongs

      if (index < 0 || index >= (int)tracks.size())
      {
         // This should only happen if there is a bug
         wxASSERT(false);
         return false;
      }

      WaveTrack* track = tracks[index];
      WaveClip*  clip = track->NewestOrNewClip();
      Sequence* seq = clip->GetSequence();

      // Load the blockfile from the XML
      const auto &dirManager = mProject->GetDirManager();
      dirManager->SetLoadingFormat(seq->GetSampleFormat());

      BlockArray array;
      array.resize(1);
      dirManager->SetLoadingTarget(&array, 0);
      auto &blockFile = array[0].f;

      if (!dirManager->HandleXMLTag(tag, attrs) || !blockFile)
      {
         // This should only happen if there is a bug
         wxASSERT(false);
         return false;
      }

      seq->AppendBlockFile(blockFile);
      clip->UpdateEnvelopeTrackLen();

   } else if (wxStrcmp(tag, wxT("recordingrecovery")) == 0)
   {
      mAutoSaveIdent = 0;

      // loop through attrs, which is a null-terminated list of
      // attribute-value pairs
      long nValue;
      while(*attrs)
      {
         const wxChar *attr = *attrs++;
         const wxChar *value = *attrs++;

         if (!value)
            break;

         const wxString strValue = value;
         //this channels value does not correspond to WaveTrack::Left/Right/Mono, but which channel of the recording device
         //it came from, and thus we can't use XMLValueChecker::IsValidChannel on it.  Rather we compare to the next attribute value.
         if (wxStrcmp(attr, wxT("channel")) == 0)
         {
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&nValue) || nValue < 0)
               return false;
            mChannel = nValue;
         }
         else if (wxStrcmp(attr, wxT("numchannels")) == 0)
         {
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&nValue) ||
                  (nValue < 1))
               return false;
            if(mChannel >= nValue )
               return false;
            mNumChannels = nValue;
         }
         else if (wxStrcmp(attr, wxT("id")) == 0)
         {
            if (!XMLValueChecker::IsGoodInt(strValue) || !strValue.ToLong(&nValue) ||
                  (nValue < 1))
               return false;
            mAutoSaveIdent = nValue;
         }

      }
   }

   return true;
}

void RecordingRecoveryHandler::HandleXMLEndTag(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("simpleblockfile")) == 0)
      // Still in inner looop
      return;

   WaveTrackArray tracks = mProject->GetTracks()->GetWaveTrackArray(false);
   int index = FindTrack();
   // We need to find the track and sequence where the blockfile belongs

   if (index < 0 || index >= (int)tracks.size()) {
      // This should only happen if there is a bug
      wxASSERT(false);
   }
   else {
      WaveTrack* track = tracks[index];
      WaveClip*  clip = track->NewestOrNewClip();
      Sequence* seq = clip->GetSequence();

      seq->ConsistencyCheck(wxT("RecordingRecoveryHandler::HandleXMLEndTag"));
   }
}

XMLTagHandler* RecordingRecoveryHandler::HandleXMLChild(const wxChar *tag)
{
   if (wxStrcmp(tag, wxT("simpleblockfile")) == 0)
      return this; // HandleXMLTag also handles <simpleblockfile>

   return NULL;
}

///
/// AutoSaveFile class
///

// Simple "binary xml" format used exclusively for autosave files.
//
// It is not intended to transport these files across platform architectures,
// so endianness is not a concern.
//
// It is not intended that the user view or modify the file.
//
// It IS intended that very little work be done during auto save, so numbers
// and strings are written in their native format.  They will be converted
// during recovery.
//
// The file has 3 main sections:
//
//    ident             literal "<?xml autosave>"
//    name dictionary   dictionary of all names used in the document
//    data fields       the "encoded" XML document
//
// If a subtree is added, it will be preceeded with FT_Push to tell the decoder
// to preserve the active dictionary.  The decoder will then restore the
// dictionary when an FT_Pop is encountered.  Nesting is unlimited.
//
// To save space, each name (attribute or element) encountered is stored in
// the name dictionary and replaced with the assigned 2-byte identifier.
//
// All strings are in native unicode format, 2-byte or 4-byte.
//
// All "lengths" are 2-byte signed, so are limited to 32767 bytes long.

enum FieldTypes
{
   FT_StartTag,      // type, ID, name
   FT_EndTag,        // type, ID, name
   FT_String,        // type, ID, name, string length, string
   FT_Int,           // type, ID, value
   FT_Bool,          // type, ID, value
   FT_Long,          // type, ID, value
   FT_LongLong,      // type, ID, value
   FT_SizeT,         // type, ID, value
   FT_Float,         // type, ID, value
   FT_Double,        // type, ID, value
   FT_Data,          // type, string length, string
   FT_Raw,           // type, string length, string
   FT_Push,          // type only
   FT_Pop,           // type only
   FT_Name           // type, name length, name
};

#include <wx/arrimpl.cpp>
WX_DEFINE_OBJARRAY(IdMapArray);

AutoSaveFile::AutoSaveFile(size_t allocSize)
{
   mAllocSize = allocSize;
}

AutoSaveFile::~AutoSaveFile()
{
}

void AutoSaveFile::StartTag(const wxString & name)
{
   mBuffer.PutC(FT_StartTag);
   WriteName(name);
}

void AutoSaveFile::EndTag(const wxString & name)
{
   mBuffer.PutC(FT_EndTag);
   WriteName(name);
}

void AutoSaveFile::WriteAttr(const wxString & name, const wxChar *value)
{
   WriteAttr(name, wxString(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, const wxString & value)
{
   mBuffer.PutC(FT_String);
   WriteName(name);

   int len = value.Length() * sizeof(wxChar);

   mBuffer.Write(&len, sizeof(len));
   mBuffer.Write(value.wx_str(), len);
}

void AutoSaveFile::WriteAttr(const wxString & name, int value)
{
   mBuffer.PutC(FT_Int);
   WriteName(name);

   mBuffer.Write(&value, sizeof(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, bool value)
{
   mBuffer.PutC(FT_Bool);
   WriteName(name);

   mBuffer.Write(&value, sizeof(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, long value)
{
   mBuffer.PutC(FT_Long);
   WriteName(name);

   mBuffer.Write(&value, sizeof(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, long long value)
{
   mBuffer.PutC(FT_LongLong);
   WriteName(name);

   mBuffer.Write(&value, sizeof(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, size_t value)
{
   mBuffer.PutC(FT_SizeT);
   WriteName(name);

   mBuffer.Write(&value, sizeof(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, float value, int digits)
{
   mBuffer.PutC(FT_Float);
   WriteName(name);

   mBuffer.Write(&value, sizeof(value));
   mBuffer.Write(&digits, sizeof(digits));
}

void AutoSaveFile::WriteAttr(const wxString & name, double value, int digits)
{
   mBuffer.PutC(FT_Double);
   WriteName(name);

   mBuffer.Write(&value, sizeof(value));
   mBuffer.Write(&digits, sizeof(digits));
}

void AutoSaveFile::WriteData(const wxString & value)
{
   mBuffer.PutC(FT_Data);

   int len = value.Length() * sizeof(wxChar);

   mBuffer.Write(&len, sizeof(len));
   mBuffer.Write(value.wx_str(), len);
}

void AutoSaveFile::Write(const wxString & value)
{
   mBuffer.PutC(FT_Raw);

   int len = value.Length() * sizeof(wxChar);

   mBuffer.Write(&len, sizeof(len));
   mBuffer.Write(value.wx_str(), len);
}

void AutoSaveFile::WriteSubTree(const AutoSaveFile & value)
{
   mBuffer.PutC(FT_Push);

   wxStreamBuffer *buf = value.mDict.GetOutputStreamBuffer();
   mBuffer.Write(buf->GetBufferStart(), buf->GetIntPosition());

   buf = value.mBuffer.GetOutputStreamBuffer();
   mBuffer.Write(buf->GetBufferStart(), buf->GetIntPosition());

   mBuffer.PutC(FT_Pop);
}

bool AutoSaveFile::Write(wxFFile & file) const
{
   bool success = file.Write(AutoSaveIdent, strlen(AutoSaveIdent)) == strlen(AutoSaveIdent);
   if (success)
   {
      success = Append(file);
   }

   return success;
}

bool AutoSaveFile::Append(wxFFile & file) const
{
   wxStreamBuffer *buf = mDict.GetOutputStreamBuffer();

   bool success = file.Write(buf->GetBufferStart(), buf->GetIntPosition()) == buf->GetIntPosition();
   if (success)
   {
      buf = mBuffer.GetOutputStreamBuffer();
      success = file.Write(buf->GetBufferStart(), buf->GetIntPosition()) == buf->GetIntPosition();
   }

   return success;
}

void AutoSaveFile::CheckSpace(wxMemoryOutputStream & os)
{
   wxStreamBuffer *buf = os.GetOutputStreamBuffer();
   size_t left = buf->GetBytesLeft();
   if (left == 0)
   {
      size_t origPos = buf->GetIntPosition();
      char *temp = new char[mAllocSize];
      buf->Write(temp, mAllocSize);
      delete[] temp;
      buf->SetIntPosition(origPos);
   }
}

void AutoSaveFile::WriteName(const wxString & name)
{
   wxASSERT(name.Length() * sizeof(wxChar) <= SHRT_MAX);
   short len = name.Length() * sizeof(wxChar);
   short id;

   if (mNames.count(name))
   {
      id = mNames[name];
   }
   else
   {
      id = mNames.size();
      mNames[name] = id;

      CheckSpace(mDict);
      mDict.PutC(FT_Name);
      mDict.Write(&id, sizeof(id));
      mDict.Write(&len, sizeof(len));
      mDict.Write(name.wx_str(), len);
   }

   CheckSpace(mBuffer);
   mBuffer.Write(&id, sizeof(id));
}

bool AutoSaveFile::IsEmpty() const
{
   return mBuffer.GetLength() == 0;
}

bool AutoSaveFile::Decode(const wxString & fileName)
{
   char ident[sizeof(AutoSaveIdent)];
   size_t len = strlen(AutoSaveIdent);

   const wxFileName fn(fileName);
   const wxString fnPath{fn.GetFullPath()};
   wxFFile file;

   if (!file.Open(fnPath, wxT("rb")))
   {
      return false;
   }

   if (file.Read(&ident, len) != len || strncmp(ident, AutoSaveIdent, len) != 0)
   {
      // It could be that the file has already been decoded or that it is one
      // from 2.1.0 or earlier.  In the latter case, we need to ensure the
      // closing </project> tag is preset.

      // Close the file so we can reopen it in read/write mode
      file.Close();

      // Add </project> tag, if necessary
      if (!file.Open(fnPath, wxT("r+b")))
      {
         // Really shouldn't happen, but let the caller deal with it
         return false;
      }

      // Read the last 16 bytes of the file and check if they contain
      // "</project>" somewhere.
      const int bufsize = 16;
      char buf[bufsize + 1];

      // FIXME: TRAP_ERR AutoSaveFile::Decode reports OK even when wxFFile errors.
      // Might be incompletely written file, but not clear that is OK to be
      // silent about.
      if (file.SeekEnd(-bufsize))
      {
         if (file.Read(buf, bufsize) == bufsize)
         {
            buf[bufsize] = 0;
            if (strstr(buf, "</project>") == 0)
            {
               // End of file does not contain closing </project> tag, so add it
               if (file.Seek(0, wxFromEnd))
               {
                  strcpy(buf, "</project>\n");
                  file.Write(buf, strlen(buf));
               }
            }
         }
      }

      file.Close();

      return true;
   }

   len = file.Length() - len;
   char *buf = new char[len];

   if (file.Read(buf, len) != len)
   {
      delete[] buf;
      return false;
   }

   wxMemoryInputStream in(buf, len);

   file.Close();

   // Decode to a temporary file to preserve the orignal.
   wxString tempName = fn.CreateTempFileName(fnPath);
   bool opened = false;

   XMLFileWriter out;

   // JKC: ANSWER-ME: Is the try catch actually doing anything?
   // If it is useful, why are we not using it everywhere?
   // If it isn't useful, why are we doing it here?
   try
   {
      out.Open(tempName, wxT("wb"));
      opened = out.IsOpened();
   }
   catch (const XMLFileWriterException&)
   {
   }

   if (!opened)
   {
      delete[] buf;

      wxRemoveFile(tempName);

      return false;
   }

   mIds.clear();

   while (!in.Eof() && !out.Error())
   {
      short id;

      switch (in.GetC())
      {
         case FT_Push:
         {
            mIdStack.Add(mIds);
            mIds.clear();
         }
         break;

         case FT_Pop:
         {
            mIds = mIdStack[mIdStack.GetCount() - 1];
            mIdStack.RemoveAt(mIdStack.GetCount() - 1);
         }
         break;

         case FT_Name:
         {
            short len;

            in.Read(&id, sizeof(id));
            in.Read(&len, sizeof(len));
            wxChar *name = new wxChar[len / sizeof(wxChar)];
            in.Read(name, len);

            mIds[id] = wxString(name, len / sizeof(wxChar));
            delete[] name;
         }
         break;

         case FT_StartTag:
         {
            in.Read(&id, sizeof(id));

            out.StartTag(mIds[id]);
         }
         break;

         case FT_EndTag:
         {
            in.Read(&id, sizeof(id));

            out.EndTag(mIds[id]);
         }
         break;

         case FT_String:
         {
            int len;

            in.Read(&id, sizeof(id));
            in.Read(&len, sizeof(len));
            wxChar *val = new wxChar[len / sizeof(wxChar)];
            in.Read(val, len);

            out.WriteAttr(mIds[id], wxString(val, len / sizeof(wxChar)));
            delete[] val;
         }
         break;

         case FT_Float:
         {
            float val;
            int dig;

            in.Read(&id, sizeof(id));
            in.Read(&val, sizeof(val));
            in.Read(&dig, sizeof(dig));

            out.WriteAttr(mIds[id], val, dig);
         }
         break;

         case FT_Double:
         {
            double val;
            int dig;

            in.Read(&id, sizeof(id));
            in.Read(&val, sizeof(val));
            in.Read(&dig, sizeof(dig));

            out.WriteAttr(mIds[id], val, dig);
         }
         break;

         case FT_Int:
         {
            int val;

            in.Read(&id, sizeof(id));
            in.Read(&val, sizeof(val));

            out.WriteAttr(mIds[id], val);
         }
         break;

         case FT_Bool:
         {
            bool val;

            in.Read(&id, sizeof(id));
            in.Read(&val, sizeof(val));

            out.WriteAttr(mIds[id], val);
         }
         break;

         case FT_Long:
         {
            long val;

            in.Read(&id, sizeof(id));
            in.Read(&val, sizeof(val));

            out.WriteAttr(mIds[id], val);
         }
         break;

         case FT_LongLong:
         {
            long long val;

            in.Read(&id, sizeof(id));
            in.Read(&val, sizeof(val));

            out.WriteAttr(mIds[id], val);
         }
         break;

         case FT_SizeT:
         {
            size_t val;

            in.Read(&id, sizeof(id));
            in.Read(&val, sizeof(val));

            out.WriteAttr(mIds[id], val);
         }
         break;

         case FT_Data:
         {
            int len;

            in.Read(&len, sizeof(len));
            wxChar *val = new wxChar[len / sizeof(wxChar)];
            in.Read(val, len);

            out.WriteData(wxString(val, len / sizeof(wxChar)));
            delete[] val;
         }
         break;

         case FT_Raw:
         {
            int len;

            in.Read(&len, sizeof(len));
            wxChar *val = new wxChar[len / sizeof(wxChar)];
            in.Read(val, len);

            out.Write(wxString(val, len / sizeof(wxChar)));
            delete[] val;
         }
         break;

         default:
            wxASSERT(true);
         break;
      }
   }

   delete[] buf;

   bool error = out.Error();
 
   out.Close();

   // Bail if decoding failed.
   if (error)
   {
      // File successfully decoded
      wxRemoveFile(tempName);

      return false;
   }

   // Decoding was successful, so remove the original file and replace with decoded one.
   if (wxRemoveFile(fileName))
   {
      if (!wxRenameFile(tempName, fileName))
      {
         return false;
      }
   }

   return true;
}
