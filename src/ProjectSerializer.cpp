/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2.  See License.txt.

   ProjectSerializer.cpp

*******************************************************************//**

\class ProjectSerializer
\brief a class used to (de)serialize the project catalog

*//********************************************************************/

#include "Audacity.h"
#include "ProjectSerializer.h"

#include <algorithm>
#include <cstdint>
#include <mutex>
#include <wx/ustring.h>

///
/// ProjectSerializer class
///

// Simple "binary xml" format used exclusively for project documents.
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
// If a subtree is added, it will be preceded with FT_Push to tell the decoder
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
// is written and then the envelope is later removed, the dict will still
// contain the envelope name, but that's not a problem.

NameMap ProjectSerializer::mNames;
wxMemoryBuffer ProjectSerializer::mDict;

TranslatableString ProjectSerializer::FailureMessage( const FilePath &/*filePath*/ )
{
   return 
XO("This recovery file was saved by Audacity 2.3.0 or before.\n"
   "You need to run that version of Audacity to recover the project." );
}

namespace {
   // Aliases for the FIXED-WIDTH integer types that are used in the file
   // format.
   
   // Chosen so that among the four build types (32 bit Windows, 64
   // bit Windows, 64 bit Mac clang, Linux g++) presently done (3.0.0
   // development), we use the narrowest width of the type on any of them, so
   // that anything saved on one build will be read back identically on all
   // builds. (Although this means that very large values on some systems might
   // be saved and then read back with loss.)

   // In fact the only types for which this matters are long (only 32 bits on
   // 32 and 64 bit Windows) and size_t (only 32 bits on 32 bit Windows).

   using UShort = std::uint16_t;
   using Int = std::int32_t;

   using Long = std::int32_t;   // To save long values
   using ULong = std::uint32_t; // To save size_t values

   using LongLong = std::int64_t;

   // Detect this computer's endianness
   bool IsLittleEndian()
   {
      const std::uint32_t x = 1u;
      return
         static_cast<const unsigned char*>(static_cast<const void*>(&x))[0];
      // We will assume the same for other widths!
   }
   // In C++20 this could be
   // constexpr bool IsLittleEndian = (std::endian::native == std::endian::little);
   // static_assert( IsLittleEndian || (std::endian::native == std::endian::big),
   //    "Oh no!  I'm mixed-endian!" );

   // Functions that can read and write native integer types to a canonicalized
   // little-endian file format.  (We don't bother to do the same for floating
   // point numbers.)

   // Write native little-endian to little-endian file format
   template< typename Number >
   void WriteLittleEndian( wxMemoryBuffer &out, Number value )
   {
      out.AppendData( &value, sizeof(value) );
   }

   // Write native big-endian to little-endian file format
   template< typename Number >
   void WriteBigEndian( wxMemoryBuffer &out, Number value )
   {
      auto begin = static_cast<unsigned char*>( static_cast<void*>( &value ) );
      std::reverse( begin, begin + sizeof( value ) );
      out.AppendData( &value, sizeof(value) );
   }

   // Read little-endian file format to native little-endian
   template< typename Number >
   Number ReadLittleEndian( wxMemoryInputStream &in )
   {
      Number result;
      in.Read( &result, sizeof(result) );
      return result;
   }

   // Read little-endian file format to native big-endian
   template< typename Number >
   Number ReadBigEndian( wxMemoryInputStream &in )
   {
      Number result;
      in.Read( &result, sizeof(result) );
      auto begin = static_cast<unsigned char*>( static_cast<void*>( &result ) );
      std::reverse( begin, begin + sizeof( result ) );
      return result;
   }

   // Choose between implementations!
   static const auto WriteUShort =   IsLittleEndian()
      ? &WriteLittleEndian<UShort>   : &WriteBigEndian<UShort>;
   static const auto WriteInt =      IsLittleEndian()
      ? &WriteLittleEndian<Int>      : &WriteBigEndian<Int>;
   static const auto WriteLong =     IsLittleEndian()
      ? &WriteLittleEndian<Long>     : &WriteBigEndian<Long>;
   static const auto WriteULong =    IsLittleEndian()
      ? &WriteLittleEndian<ULong>    : &WriteBigEndian<ULong>;
   static const auto WriteLongLong = IsLittleEndian()
      ? &WriteLittleEndian<LongLong> : &WriteBigEndian<LongLong>;

   static const auto ReadUShort =   IsLittleEndian()
      ? &ReadLittleEndian<UShort>   : &ReadBigEndian<UShort>;
   static const auto ReadInt =      IsLittleEndian()
      ? &ReadLittleEndian<Int>      : &ReadBigEndian<Int>;
   static const auto ReadLong =     IsLittleEndian()
      ? &ReadLittleEndian<Long>     : &ReadBigEndian<Long>;
   static const auto ReadULong =    IsLittleEndian()
      ? &ReadLittleEndian<ULong>    : &ReadBigEndian<ULong>;
   static const auto ReadLongLong = IsLittleEndian()
      ? &ReadLittleEndian<LongLong> : &ReadBigEndian<LongLong>;

   // Functions to read and write certain lengths -- maybe we will change
   // our choices for widths or signedness?

   using Length = Int;  // Instead, as wide as size_t?
   static const auto WriteLength = WriteInt;
   static const auto ReadLength = ReadInt;

   using Digits = Int;  // Instead, just an unsigned char?
   static const auto WriteDigits = WriteInt;
   static const auto ReadDigits = ReadInt;
}

ProjectSerializer::ProjectSerializer(size_t allocSize)
{
   mDict.SetBufSize(allocSize);
   mBuffer.SetBufSize(allocSize);

   static std::once_flag flag;
   std::call_once(flag, []{
      // Just once per run, store header information in the unique static
      // dictionary that will be written into each project that is saved.
      // Store the size of "wxStringCharType" so we can convert during recovery
      // in case the file is used on a system with a different character size.
      char size = sizeof(wxStringCharType);
      mDict.AppendByte(FT_CharSize);
      mDict.AppendData(&size, 1);
   });

   mDictChanged = false;
}

ProjectSerializer::~ProjectSerializer()
{
}

void ProjectSerializer::StartTag(const wxString & name)
{
   mBuffer.AppendByte(FT_StartTag);
   WriteName(name);
}

void ProjectSerializer::EndTag(const wxString & name)
{
   mBuffer.AppendByte(FT_EndTag);
   WriteName(name);
}

void ProjectSerializer::WriteAttr(const wxString & name, const wxChar *value)
{
   WriteAttr(name, wxString(value));
}

void ProjectSerializer::WriteAttr(const wxString & name, const wxString & value)
{
   mBuffer.AppendByte(FT_String);
   WriteName(name);

   const Length len = value.length() * sizeof(wxStringCharType);
   WriteLength( mBuffer, len );
   mBuffer.AppendData(value.wx_str(), len);
}

void ProjectSerializer::WriteAttr(const wxString & name, int value)
{
   mBuffer.AppendByte(FT_Int);
   WriteName(name);

   WriteInt( mBuffer, value );
}

void ProjectSerializer::WriteAttr(const wxString & name, bool value)
{
   mBuffer.AppendByte(FT_Bool);
   WriteName(name);

   mBuffer.AppendByte(value);
}

void ProjectSerializer::WriteAttr(const wxString & name, long value)
{
   mBuffer.AppendByte(FT_Long);
   WriteName(name);

   WriteLong( mBuffer, value );
}

void ProjectSerializer::WriteAttr(const wxString & name, long long value)
{
   mBuffer.AppendByte(FT_LongLong);
   WriteName(name);

   WriteLongLong( mBuffer, value );
}

void ProjectSerializer::WriteAttr(const wxString & name, size_t value)
{
   mBuffer.AppendByte(FT_SizeT);
   WriteName(name);

   WriteULong( mBuffer, value );
}

void ProjectSerializer::WriteAttr(const wxString & name, float value, int digits)
{
   mBuffer.AppendByte(FT_Float);
   WriteName(name);

   mBuffer.AppendData(&value, sizeof(value));
   WriteDigits( mBuffer, digits );
}

void ProjectSerializer::WriteAttr(const wxString & name, double value, int digits)
{
   mBuffer.AppendByte(FT_Double);
   WriteName(name);

   mBuffer.AppendData(&value, sizeof(value));
   WriteDigits( mBuffer, digits );
}

void ProjectSerializer::WriteData(const wxString & value)
{
   mBuffer.AppendByte(FT_Data);

   Length len = value.length() * sizeof(wxStringCharType);
   WriteLength( mBuffer, len );
   mBuffer.AppendData(value.wx_str(), len);
}

void ProjectSerializer::Write(const wxString & value)
{
   mBuffer.AppendByte(FT_Raw);
   Length len = value.length() * sizeof(wxStringCharType);
   WriteLength( mBuffer, len );
   mBuffer.AppendData(value.wx_str(), len);
}

void ProjectSerializer::WriteSubTree(const ProjectSerializer & value)
{
   mBuffer.AppendByte(FT_Push);

   mBuffer.AppendData(value.mDict.GetData(), value.mDict.GetDataLen());
   mBuffer.AppendData(value.mBuffer.GetData(), value.mBuffer.GetDataLen());

   mBuffer.AppendByte(FT_Pop);
}

void ProjectSerializer::WriteName(const wxString & name)
{
   wxASSERT(name.length() * sizeof(wxStringCharType) <= SHRT_MAX);
   UShort id;

   auto nameiter = mNames.find(name);
   if (nameiter != mNames.end())
   {
      id = nameiter->second;
   }
   else
   {
      // mNames is static.  This appends each name to static mDict only once
      // in each run.
      UShort len = name.length() * sizeof(wxStringCharType);

      id = mNames.size();
      mNames[name] = id;

      mDict.AppendByte(FT_Name);
      WriteUShort( mDict, id );
      WriteUShort( mDict, len );
      mDict.AppendData(name.wx_str(), len);

      mDictChanged = true;
   }

   WriteUShort( mBuffer, id );
}

const wxMemoryBuffer &ProjectSerializer::GetDict() const
{
   return mDict;
}

const wxMemoryBuffer &ProjectSerializer::GetData() const
{
   return mBuffer;
}

bool ProjectSerializer::IsEmpty() const
{
   return mBuffer.GetDataLen() == 0;
}

bool ProjectSerializer::DictChanged() const
{
   return mDictChanged;
}

// See ProjectFileIO::LoadProject() for explanation of the blockids arg
wxString ProjectSerializer::Decode(const wxMemoryBuffer &buffer)
{
   wxMemoryInputStream in(buffer.GetData(), buffer.GetDataLen());

   XMLStringWriter out;

   std::vector<char> bytes;
   IdMap mIds;
   std::vector<IdMap> mIdStack;
   char mCharSize = 0;

   mIds.clear();

   struct Error{}; // exception type for short-range try/catch
   auto Lookup = [&mIds]( UShort id ) -> const wxString &
   {
      auto iter = mIds.find( id );
      if (iter == mIds.end())
      {
         throw Error{};
      }
      return iter->second;
   };

   auto ReadString = [&mCharSize, &in, &bytes](int len) -> wxString
   {
      bytes.reserve( len + 4 );
      auto data = bytes.data();
      in.Read( data, len );
      // Make a null terminator of the widest type
      memset( data + len, '\0', 4 );
      wxUString str;
      
      switch (mCharSize)
      {
         case 1:
            str.assignFromUTF8(data, len);
         break;

         case 2:
            str.assignFromUTF16((wxChar16 *) data, len / 2);
         break;

         case 4:
            str = wxU32CharBuffer::CreateNonOwned((wxChar32 *) data, len / 4);
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
         UShort id;

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
               id = ReadUShort( in );
               auto len = ReadUShort( in );
               mIds[id] = ReadString(len);
            }
            break;

            case FT_StartTag:
            {
               id = ReadUShort( in );

               out.StartTag(Lookup(id));
            }
            break;

            case FT_EndTag:
            {
               id = ReadUShort( in );

               out.EndTag(Lookup(id));
            }
            break;

            case FT_String:
            {
               id = ReadUShort( in );
               int len = ReadLength( in );
               out.WriteAttr(Lookup(id), ReadString(len));
            }
            break;

            case FT_Float:
            {
               float val;

               id = ReadUShort( in );
               in.Read(&val, sizeof(val));
               int dig = ReadDigits( in );

               out.WriteAttr(Lookup(id), val, dig);
            }
            break;

            case FT_Double:
            {
               double val;

               id = ReadUShort( in );
               in.Read(&val, sizeof(val));
               int dig = ReadDigits( in );

               out.WriteAttr(Lookup(id), val, dig);
            }
            break;

            case FT_Int:
            {
               id = ReadUShort( in );
               int val = ReadInt( in );

               out.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_Bool:
            {
               unsigned char val;

               id = ReadUShort( in );
               in.Read(&val, 1);

               out.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_Long:
            {
               id = ReadUShort( in );
               long val = ReadLong( in );

               out.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_LongLong:
            {
               id = ReadUShort( in );
               long long val = ReadLongLong( in );
               out.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_SizeT:
            {
               id = ReadUShort( in );
               size_t val = ReadULong( in );

               out.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_Data:
            {
               int len = ReadLength( in );
               out.WriteData(ReadString(len));
            }
            break;

            case FT_Raw:
            {
               int len = ReadLength( in );
               out.Write(ReadString(len));
            }
            break;

            case FT_CharSize:
            {
               in.Read(&mCharSize, 1);
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
      // Document was corrupt, or platform differences in size or endianness
      // were not well canonicalized
      return {};
   }

   return out;
}
