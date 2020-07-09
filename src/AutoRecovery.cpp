/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   AutoRecovery.cpp

*******************************************************************//**

\class AutoSaveFile
\brief a class wrapping reading and writing of arbitrary data in 
text or binary format to a file.

*//********************************************************************/

#include "Audacity.h"
#include "AutoRecovery.h"

#include <wx/ustring.h>

///
/// AutoSaveFile class
///

// Simple "binary xml" format used exclusively for autosave documents.
//
// It is not intended that the user view or modify the file.
//
// It IS intended that very little work be done during auto save, so numbers
// and strings are written in their native format.  They will be converted
// during recovery.
//
// The file has 3 main sections:
//
//    character size    1 (UTF-8), 2 (UTF-16) or 4 (UTF-32)
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
// All name "lengths" are 2-byte signed, so are limited to 32767 bytes long.
// All string/data "lengths" are 4-byte signed.

enum FieldTypes
{
   FT_CharSize,      // type, ID, value
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

// Static so that the dict can be reused each time.
//
// If entries get added later, like when an envelope node (for example)
// is writen and then the envelope is later removed, the dict will still
// contain the envelope name, but that's not a problem.

NameMap AutoSaveFile::mNames;
wxMemoryBuffer AutoSaveFile::mDict;

TranslatableString AutoSaveFile::FailureMessage( const FilePath &/*filePath*/ )
{
   return 
XO("This recovery file was saved by Audacity 2.3.0 or before.\n"
   "You need to run that version of Audacity to recover the project." );
}

AutoSaveFile::AutoSaveFile(size_t allocSize)
{
   mDict.SetBufSize(allocSize);
   mBuffer.SetBufSize(allocSize);

   // Store the size of "wxChar" so we can convert during recovery in
   // case the file is used on a system with a different character size.
   char size = sizeof(wxChar);
   mDict.AppendByte(FT_CharSize);
   mDict.AppendData(&size, sizeof(size));

   mDictChanged = false;
}

AutoSaveFile::~AutoSaveFile()
{
}

void AutoSaveFile::StartTag(const wxString & name)
{
   mBuffer.AppendByte(FT_StartTag);
   WriteName(name);
}

void AutoSaveFile::EndTag(const wxString & name)
{
   mBuffer.AppendByte(FT_EndTag);
   WriteName(name);
}

void AutoSaveFile::WriteAttr(const wxString & name, const wxChar *value)
{
   WriteAttr(name, wxString(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, const wxString & value)
{
   mBuffer.AppendByte(FT_String);
   WriteName(name);

   int len = value.length() * sizeof(wxChar);

   mBuffer.AppendData(&len, sizeof(len));
   mBuffer.AppendData(value.wx_str(), len);
}

void AutoSaveFile::WriteAttr(const wxString & name, int value)
{
   mBuffer.AppendByte(FT_Int);
   WriteName(name);

   mBuffer.AppendData(&value, sizeof(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, bool value)
{
   mBuffer.AppendByte(FT_Bool);
   WriteName(name);

   mBuffer.AppendData(&value, sizeof(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, long value)
{
   mBuffer.AppendByte(FT_Long);
   WriteName(name);

   mBuffer.AppendData(&value, sizeof(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, long long value)
{
   mBuffer.AppendByte(FT_LongLong);
   WriteName(name);

   mBuffer.AppendData(&value, sizeof(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, size_t value)
{
   mBuffer.AppendByte(FT_SizeT);
   WriteName(name);

   mBuffer.AppendData(&value, sizeof(value));
}

void AutoSaveFile::WriteAttr(const wxString & name, float value, int digits)
{
   mBuffer.AppendByte(FT_Float);
   WriteName(name);

   mBuffer.AppendData(&value, sizeof(value));
   mBuffer.AppendData(&digits, sizeof(digits));
}

void AutoSaveFile::WriteAttr(const wxString & name, double value, int digits)
{
   mBuffer.AppendByte(FT_Double);
   WriteName(name);

   mBuffer.AppendData(&value, sizeof(value));
   mBuffer.AppendData(&digits, sizeof(digits));
}

void AutoSaveFile::WriteData(const wxString & value)
{
   mBuffer.AppendByte(FT_Data);

   int len = value.length() * sizeof(wxChar);

   mBuffer.AppendData(&len, sizeof(len));
   mBuffer.AppendData(value.wx_str(), len);
}

void AutoSaveFile::Write(const wxString & value)
{
   mBuffer.AppendByte(FT_Raw);

   int len = value.length() * sizeof(wxChar);

   mBuffer.AppendData(&len, sizeof(len));
   mBuffer.AppendData(value.wx_str(), len);
}

void AutoSaveFile::WriteSubTree(const AutoSaveFile & value)
{
   mBuffer.AppendByte(FT_Push);

   mBuffer.AppendData(value.mDict.GetData(), value.mDict.GetDataLen());
   mBuffer.AppendData(value.mBuffer.GetData(), value.mBuffer.GetDataLen());

   mBuffer.AppendByte(FT_Pop);
}

void AutoSaveFile::WriteName(const wxString & name)
{
   wxASSERT(name.length() * sizeof(wxChar) <= SHRT_MAX);
   short id;

   auto nameiter = mNames.find(name);
   if (nameiter != mNames.end())
   {
      id = nameiter->second;
   }
   else
   {
      short len = name.length() * sizeof(wxChar);

      id = mNames.size();
      mNames[name] = id;

      mDict.AppendByte(FT_Name);
      mDict.AppendData(&id, sizeof(id));
      mDict.AppendData(&len, sizeof(len));
      mDict.AppendData(name.wx_str(), len);

      mDictChanged = true;
   }

   mBuffer.AppendData(&id, sizeof(id));
}

const wxMemoryBuffer &AutoSaveFile::GetDict() const
{
   return mDict;
}

const wxMemoryBuffer &AutoSaveFile::GetData() const
{
   return mBuffer;
}

bool AutoSaveFile::IsEmpty() const
{
   return mBuffer.GetDataLen() == 0;
}

bool AutoSaveFile::DictChanged() const
{
   return mDictChanged;
}

// See ProjectFileIO::CheckForOrphans() for explanation of the blockids arg
wxString AutoSaveFile::Decode(const wxMemoryBuffer &buffer, BlockIDs &blockids)
{
   wxMemoryInputStream in(buffer.GetData(), buffer.GetDataLen());

   XMLStringWriter out;

   std::vector<char> bytes;
   IdMap mIds;
   std::vector<IdMap> mIdStack;
   char mCharSize = 0;

   mIds.clear();

   struct Error{}; // exception type for short-range try/catch
   auto Lookup = [&mIds]( short id ) -> const wxString &
   {
      auto iter = mIds.find( id );
      if (iter == mIds.end())
      {
         throw Error{};
      }
      return iter->second;
   };

   auto Convert = [&mCharSize](char *in, int len) -> wxString
   {
      wxUString str;

      switch (mCharSize)
      {
         case 1:
            str.assignFromUTF8(in, len);
         break;

         case 2:
            str.assignFromUTF16((wxChar16 *) in, len / 2);
         break;

         case 4:
            str = wxU32CharBuffer::CreateNonOwned((wxChar32 *) in, len / 4);
         break;

         default:
            wxASSERT_MSG(false, wxT("Characters size not 1, 2, or 4"));
         break;
      }

      return str;
   };

   try
   {
      while (!in.Eof())
      {
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
               bytes.reserve(len);
               in.Read(bytes.data(), len);

               mIds[id] = Convert(bytes.data(), len);
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
               bytes.reserve(len);
               in.Read(bytes.data(), len);

               out.WriteAttr(Lookup(id), Convert(bytes.data(), len));
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

               // Look for and save the "blockid" values to support orphan
               // block checking.  This should be removed once autosave and
               // related blocks become part of the same transaction.
               const wxString &name = Lookup(id);
               if (name.IsSameAs(wxT("blockid")))
               {
                  blockids.insert(val);
               }

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
               bytes.reserve(len);
               in.Read(bytes.data(), len);

               out.WriteData(Convert(bytes.data(), len));
            }
            break;

            case FT_Raw:
            {
               int len;

               in.Read(&len, sizeof(len));
               bytes.reserve(len);
               in.Read(bytes.data(), len);

               out.Write(Convert(bytes.data(), len));
            }
            break;

            case FT_CharSize:
            {
               in.Read(&mCharSize, sizeof(mCharSize));
            }
            break;

            default:
               wxASSERT(true);
            break;
         }
      }
   }
   catch( const Error& )
   {
      // Autosave was corrupt, or platform differences in size or endianness
      // were not well canonicalized
      return {};
   }

   return out;
}
