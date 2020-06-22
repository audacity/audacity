/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   AutoRecovery.cpp

*******************************************************************//**

\class AutoRecoveryDialog
\brief The AutoRecoveryDialog prompts the user whether to
recover previous Audacity projects that were closed incorrectly.

\class AutoSaveFile
\brief a class wrapping reading and writing of arbitrary data in 
text or binary format to a file.

*//********************************************************************/

#include "Audacity.h"
#include "AutoRecovery.h"
#include "DirManager.h"
#include "blockfile/SimpleBlockFile.h"
#include "Sequence.h"

#include <wx/wxprec.h>
#include <wx/filefn.h>
#include <wx/dir.h>
#include <wx/dialog.h>
#include <wx/app.h>

#include "WaveClip.h"
#include "WaveTrack.h"

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
   auto tracks = TrackList::Get( *mProject ).Any< const WaveTrack >();
   int index = 0;
   if (mAutoSaveIdent)
   {
      auto iter = tracks.begin(), end = tracks.end();
      for (; iter != end; ++iter, ++index)
      {
         if ((*iter)->GetAutoSaveIdent() == mAutoSaveIdent)
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

      auto tracks = TrackList::Get( *mProject ).Any< WaveTrack >();
      int index = FindTrack();
      // We need to find the track and sequence where the blockfile belongs

      if (index < 0 || index >= (int)tracks.size())
      {
         // This should only happen if there is a bug
         wxASSERT(false);
         return false;
      }

      auto iter = tracks.begin();
      std::advance( iter, index );
      WaveTrack* track = *iter;
      WaveClip*  clip = track->NewestOrNewClip();
      Sequence* seq = clip->GetSequence();

      // Load the blockfile from the XML
      auto &dirManager = DirManager::Get( *mProject );
      dirManager.SetLoadingFormat(seq->GetSampleFormat());

      BlockFilePtr blockFile;
      dirManager.SetLoadingTarget(
         [&]() -> BlockFilePtr& { return blockFile; } );

      if (!dirManager.HandleXMLTag(tag, attrs) || !blockFile)
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
      // Still in inner loop
      return;

   auto tracks = TrackList::Get( *mProject ).Any< WaveTrack >();
   int index = FindTrack();
   // We need to find the track and sequence where the blockfile belongs

   if (index < 0 || index >= (int)tracks.size()) {
      // This should only happen if there is a bug
      wxASSERT(false);
   }
   else {
      auto iter = tracks.begin();
      std::advance( iter, index );
      WaveTrack* track = *iter;
      WaveClip*  clip = track->NewestOrNewClip();
      Sequence* seq = clip->GetSequence();

      seq->ConsistencyCheck
         (wxT("RecordingRecoveryHandler::HandleXMLEndTag"), false);
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
   FT_StartTag,      // type, ID
   FT_EndTag,        // type, ID
   FT_String,        // type, ID, string length, string
   FT_Int,           // type, ID, value
   FT_Bool,          // type, ID, value
   FT_Long,          // type, ID, value
   FT_LongLong,      // type, ID, value
   FT_SizeT,         // type, ID, value
   FT_Float,         // type, ID, value, digits
   FT_Double,        // type, ID, value, digits
   FT_Data,          // type, string length, string
   FT_Raw,           // type, string length, string
   FT_Push,          // type only
   FT_Pop,           // type only
   FT_Name           // type, ID, name length, name
};

TranslatableString AutoSaveFile::FailureMessage( const FilePath &/*filePath*/ )
{
   return 
XO("This recovery file was saved by Audacity 2.3.0 or before.\n"
   "You need to run that version of Audacity to recover the project." );
}

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

   int len = value.length() * sizeof(wxChar);

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

   int len = value.length() * sizeof(wxChar);

   mBuffer.Write(&len, sizeof(len));
   mBuffer.Write(value.wx_str(), len);
}

void AutoSaveFile::Write(const wxString & value)
{
   mBuffer.PutC(FT_Raw);

   int len = value.length() * sizeof(wxChar);

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
      ArrayOf<char> temp{ mAllocSize };
      buf->Write(temp.get(), mAllocSize);
      buf->SetIntPosition(origPos);
   }
}

void AutoSaveFile::WriteName(const wxString & name)
{
   wxASSERT(name.length() * sizeof(wxChar) <= SHRT_MAX);
   short len = name.length() * sizeof(wxChar);
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

bool AutoSaveFile::Decode(const FilePath & fileName)
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
   using Chars = ArrayOf < char >;
   using WxChars = ArrayOf < wxChar >;
   Chars buf{ len };
   if (file.Read(buf.get(), len) != len)
   {
      return false;
   }

   wxMemoryInputStream in(buf.get(), len);

   file.Close();

   // JKC: ANSWER-ME: Is the try catch actually doing anything?
   // If it is useful, why are we not using it everywhere?
   // If it isn't useful, why are we doing it here?
   // PRL: Yes, now we are doing GuardedCall everywhere that XMLFileWriter is
   // used.
   return GuardedCall< bool >( [&] {
      XMLFileWriter out{ fileName, XO("Error Decoding File") };

      IdMap mIds;
      std::vector<IdMap> mIdStack;

      mIds.clear();
   
      struct Error{};
      auto Lookup = [&mIds]( short id ) -> const wxString & {
         auto iter = mIds.find( id );
         if ( iter == mIds.end() )
            throw Error{};
         return iter->second;
      };

      try { while ( !in.Eof() ) {
         short id;

         switch (in.GetC())
         {
            case FT_Push:
            {
               mIdStack.push_back(mIds);
               mIds.clear();
            }
            break;

            case FT_Pop:
            {
               mIds = mIdStack.back();
               mIdStack.pop_back();
            }
            break;

            case FT_Name:
            {
               short len;

               in.Read(&id, sizeof(id));
               in.Read(&len, sizeof(len));
               WxChars name{ len / sizeof(wxChar) };
               in.Read(name.get(), len);

               mIds[id] = wxString(name.get(), len / sizeof(wxChar));
            }
            break;

            case FT_StartTag:
            {
               in.Read(&id, sizeof(id));

               out.StartTag(Lookup(id));
            }
            break;

            case FT_EndTag:
            {
               in.Read(&id, sizeof(id));

               out.EndTag(Lookup(id));
            }
            break;

            case FT_String:
            {
               int len;

               in.Read(&id, sizeof(id));
               in.Read(&len, sizeof(len));
               WxChars val{ len / sizeof(wxChar) };
               in.Read(val.get(), len);

               out.WriteAttr(Lookup(id), wxString(val.get(), len / sizeof(wxChar)));
            }
            break;

            case FT_Float:
            {
               float val;
               int dig;

               in.Read(&id, sizeof(id));
               in.Read(&val, sizeof(val));
               in.Read(&dig, sizeof(dig));

               out.WriteAttr(Lookup(id), val, dig);
            }
            break;

            case FT_Double:
            {
               double val;
               int dig;

               in.Read(&id, sizeof(id));
               in.Read(&val, sizeof(val));
               in.Read(&dig, sizeof(dig));

               out.WriteAttr(Lookup(id), val, dig);
            }
            break;

            case FT_Int:
            {
               int val;

               in.Read(&id, sizeof(id));
               in.Read(&val, sizeof(val));

               out.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_Bool:
            {
               bool val;

               in.Read(&id, sizeof(id));
               in.Read(&val, sizeof(val));

               out.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_Long:
            {
               long val;

               in.Read(&id, sizeof(id));
               in.Read(&val, sizeof(val));

               out.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_LongLong:
            {
               long long val;

               in.Read(&id, sizeof(id));
               in.Read(&val, sizeof(val));

               out.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_SizeT:
            {
               size_t val;

               in.Read(&id, sizeof(id));
               in.Read(&val, sizeof(val));

               out.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_Data:
            {
               int len;

               in.Read(&len, sizeof(len));
               WxChars val{ len / sizeof(wxChar) };
               in.Read(val.get(), len);

               out.WriteData(wxString(val.get(), len / sizeof(wxChar)));
            }
            break;

            case FT_Raw:
            {
               int len;

               in.Read(&len, sizeof(len));
               WxChars val{ len / sizeof(wxChar) };
               in.Read(val.get(), len);

               out.Write(wxString(val.get(), len / sizeof(wxChar)));
            }
            break;

            default:
               wxASSERT(true);
            break;
         }
      } }
      catch( const Error & )
      {
         // return before committing, so we do not overwrite the recovery file!
         return false;
      }

      out.Commit();

      return true;
   } );
}
