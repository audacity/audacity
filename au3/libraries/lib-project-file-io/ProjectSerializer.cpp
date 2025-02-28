/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   ProjectSerializer.cpp

*******************************************************************//**

\class ProjectSerializer
\brief a class used to (de)serialize the project catalog

*//********************************************************************/

#include "ProjectSerializer.h"

#include <algorithm>
#include <cstdint>
#include <mutex>
#include <wx/ustring.h>
#include <codecvt>
#include <locale>
#include <deque>

#include <wx/log.h>

#include "BufferedStreamReader.h"
#include "MemoryX.h"

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
    FT_CharSize,     // type, ID, value
    FT_StartTag,     // type, ID
    FT_EndTag,       // type, ID
    FT_String,       // type, ID, string length, string
    FT_Int,          // type, ID, value
    FT_Bool,         // type, ID, value
    FT_Long,         // type, ID, value
    FT_LongLong,     // type, ID, value
    FT_SizeT,        // type, ID, value
    FT_Float,        // type, ID, value, digits
    FT_Double,       // type, ID, value, digits
    FT_Data,         // type, string length, string
    FT_Raw,          // type, string length, string
    FT_Push,         // type only
    FT_Pop,          // type only
    FT_Name          // type, ID, name length, name
};

// Static so that the dict can be reused each time.
//
// If entries get added later, like when an envelope node (for example)
// is written and then the envelope is later removed, the dict will still
// contain the envelope name, but that's not a problem.

NameMap ProjectSerializer::mNames;
MemoryStream ProjectSerializer::mDict;

TranslatableString ProjectSerializer::FailureMessage(const FilePath& /*filePath*/)
{
    return
        XO("This recovery file was saved by Audacity 2.3.0 or before.\n"
           "You need to run that version of Audacity to recover the project.");
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

// In C++20 this could be
// constexpr bool IsLittleEndian = (std::endian::native == std::endian::little);
// static_assert( IsLittleEndian || (std::endian::native == std::endian::big),
//    "Oh no!  I'm mixed-endian!" );

// Functions that can read and write native integer types to a canonicalized
// little-endian file format.  (We don't bother to do the same for floating
// point numbers.)

// Write native little-endian to little-endian file format
template<typename Number>
void WriteLittleEndian(MemoryStream& out, Number value)
{
    out.AppendData(&value, sizeof(value));
}

// Write native big-endian to little-endian file format
template<typename Number> void WriteBigEndian(MemoryStream& out, Number value)
{
    auto begin = static_cast<unsigned char*>(static_cast<void*>(&value));
    std::reverse(begin, begin + sizeof(value));
    out.AppendData(&value, sizeof(value));
}

// Read little-endian file format to native little-endian
template<typename Number> Number ReadLittleEndian(BufferedStreamReader& in)
{
    Number result;
    in.ReadValue(result);
    return result;
}

// Read little-endian file format to native big-endian
template<typename Number> Number ReadBigEndian(BufferedStreamReader& in)
{
    Number result;
    in.ReadValue(result);
    auto begin = static_cast<unsigned char*>(static_cast<void*>(&result));
    std::reverse(begin, begin + sizeof(result));
    return result;
}

// Choose between implementations!
static const auto WriteUShort
    =IsLittleEndian() ? &WriteLittleEndian<UShort> : &WriteBigEndian<UShort>;
static const auto WriteInt
    =IsLittleEndian() ? &WriteLittleEndian<Int> : &WriteBigEndian<Int>;
static const auto WriteLong
    =IsLittleEndian() ? &WriteLittleEndian<Long> : &WriteBigEndian<Long>;
static const auto WriteULong
    =IsLittleEndian() ? &WriteLittleEndian<ULong> : &WriteBigEndian<ULong>;
static const auto WriteLongLong
    =IsLittleEndian() ? &WriteLittleEndian<LongLong> : &WriteBigEndian<LongLong>;

static const auto ReadUShort
    =IsLittleEndian() ? &ReadLittleEndian<UShort> : &ReadBigEndian<UShort>;
static const auto ReadInt
    =IsLittleEndian() ? &ReadLittleEndian<Int> : &ReadBigEndian<Int>;
static const auto ReadLong
    =IsLittleEndian() ? &ReadLittleEndian<Long> : &ReadBigEndian<Long>;
static const auto ReadULong
    =IsLittleEndian() ? &ReadLittleEndian<ULong> : &ReadBigEndian<ULong>;
static const auto ReadLongLong
    =IsLittleEndian() ? &ReadLittleEndian<LongLong> : &ReadBigEndian<LongLong>;

// Functions to read and write certain lengths -- maybe we will change
// our choices for widths or signedness?

using Length = Int; // Instead, as wide as size_t?
static const auto WriteLength = WriteInt;
static const auto ReadLength = ReadInt;

using Digits = Int; // Instead, just an unsigned char?
static const auto WriteDigits = WriteInt;
static const auto ReadDigits = ReadInt;

class XMLTagHandlerAdapter final
{
public:
    explicit XMLTagHandlerAdapter(XMLTagHandler* handler) noexcept
        : mBaseHandler(handler)
    {
    }

    void EmitStartTag(const std::string_view& name)
    {
        if (mInTag) {
            EmitStartTag();
        }

        mCurrentTagName = name;
        mInTag = true;
    }

    void EndTag(const std::string_view& name)
    {
        if (mInTag) {
            EmitStartTag();
        }

        if (XMLTagHandler* const handler = mHandlers.back()) {
            handler->HandleXMLEndTag(name);
        }

        mHandlers.pop_back();
    }

    void WriteAttr(const std::string_view& name, std::string value)
    {
        assert(mInTag);

        if (!mInTag) {
            return;
        }

        mAttributes.emplace_back(name, CacheString(std::move(value)));
    }

    template<typename T> void WriteAttr(const std::string_view& name, T value)
    {
        assert(mInTag);

        if (!mInTag) {
            return;
        }

        mAttributes.emplace_back(name, XMLAttributeValueView(value));
    }

    void WriteData(std::string value)
    {
        if (mInTag) {
            EmitStartTag();
        }

        if (XMLTagHandler* const handler = mHandlers.back()) {
            handler->HandleXMLContent(CacheString(std::move(value)));
        }
    }

    void WriteRaw(std::string)
    {
        // This method is intentionally left empty.
        // The only data that is serialized by FT_Raw
        // is the boilerplate code like <?xml > and <!DOCTYPE>
        // which are ignored
    }

    bool Finalize()
    {
        if (mInTag) {
            EmitStartTag();
            EndTag(mCurrentTagName);
        }

        return mBaseHandler != nullptr;
    }

private:
    void EmitStartTag()
    {
        if (mHandlers.empty()) {
            mHandlers.push_back(mBaseHandler);
        } else {
            if (XMLTagHandler* const handler = mHandlers.back()) {
                mHandlers.push_back(handler->HandleXMLChild(mCurrentTagName));
            } else {
                mHandlers.push_back(NULL);
            }
        }

        if (XMLTagHandler*& handler = mHandlers.back()) {
            if (!handler->HandleXMLTag(mCurrentTagName, mAttributes)) {
                handler = nullptr;

                if (mHandlers.size() == 1) {
                    mBaseHandler = nullptr;
                }
            }
        }

        mStringsCache.clear();
        mAttributes.clear();
        mInTag = false;
    }

    std::string_view CacheString(std::string string)
    {
        mStringsCache.emplace_back(std::move(string));
        return mStringsCache.back();
    }

    XMLTagHandler* mBaseHandler;

    std::vector<XMLTagHandler*> mHandlers;

    std::string_view mCurrentTagName;

    std::deque<std::string> mStringsCache;
    AttributesList mAttributes;

    bool mInTag { false };
};

// template<typename BaseCharType>
// std::string FastStringConvertFromAscii(const BaseCharType* begin, const BaseCharType* end)
// {
//
// }

template<typename BaseCharType>
std::string FastStringConvert(const void* bytes, int bytesCount)
{
    constexpr int charSize = sizeof(BaseCharType);

    assert(bytesCount % charSize == 0);

    const auto begin = static_cast<const BaseCharType*>(bytes);
    const auto end = begin + bytesCount / charSize;

    const bool isAscii = std::all_of(
        begin, end,
        [](BaseCharType c)
    { return static_cast<std::make_unsigned_t<BaseCharType> >(c) < 0x7f; });

    if (isAscii) {
        return std::string(begin, end);
    }

    return std::wstring_convert<std::codecvt_utf8<BaseCharType>, BaseCharType>()
           .to_bytes(begin, end);
}
} // namespace

ProjectSerializer::ProjectSerializer(size_t allocSize)
{
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

void ProjectSerializer::StartTag(const wxString& name)
{
    mBuffer.AppendByte(FT_StartTag);
    WriteName(name);
}

void ProjectSerializer::EndTag(const wxString& name)
{
    mBuffer.AppendByte(FT_EndTag);
    WriteName(name);
}

void ProjectSerializer::WriteAttr(const wxString& name, const wxChar* value)
{
    WriteAttr(name, wxString(value));
}

void ProjectSerializer::WriteAttr(const wxString& name, const wxString& value)
{
    mBuffer.AppendByte(FT_String);
    WriteName(name);

    const Length len = value.length() * sizeof(wxStringCharType);
    WriteLength(mBuffer, len);
    mBuffer.AppendData(value.wx_str(), len);
}

void ProjectSerializer::WriteAttr(const wxString& name, int value)
{
    mBuffer.AppendByte(FT_Int);
    WriteName(name);

    WriteInt(mBuffer, value);
}

void ProjectSerializer::WriteAttr(const wxString& name, bool value)
{
    mBuffer.AppendByte(FT_Bool);
    WriteName(name);

    mBuffer.AppendByte(value);
}

void ProjectSerializer::WriteAttr(const wxString& name, long value)
{
    mBuffer.AppendByte(FT_Long);
    WriteName(name);

    WriteLong(mBuffer, value);
}

void ProjectSerializer::WriteAttr(const wxString& name, long long value)
{
    mBuffer.AppendByte(FT_LongLong);
    WriteName(name);

    WriteLongLong(mBuffer, value);
}

void ProjectSerializer::WriteAttr(const wxString& name, size_t value)
{
    mBuffer.AppendByte(FT_SizeT);
    WriteName(name);

    WriteULong(mBuffer, value);
}

void ProjectSerializer::WriteAttr(const wxString& name, float value, int digits)
{
    mBuffer.AppendByte(FT_Float);
    WriteName(name);

    mBuffer.AppendData(&value, sizeof(value));
    WriteDigits(mBuffer, digits);
}

void ProjectSerializer::WriteAttr(const wxString& name, double value, int digits)
{
    mBuffer.AppendByte(FT_Double);
    WriteName(name);

    mBuffer.AppendData(&value, sizeof(value));
    WriteDigits(mBuffer, digits);
}

void ProjectSerializer::WriteData(const wxString& value)
{
    mBuffer.AppendByte(FT_Data);

    Length len = value.length() * sizeof(wxStringCharType);
    WriteLength(mBuffer, len);
    mBuffer.AppendData(value.wx_str(), len);
}

void ProjectSerializer::Write(const wxString& value)
{
    mBuffer.AppendByte(FT_Raw);
    Length len = value.length() * sizeof(wxStringCharType);
    WriteLength(mBuffer, len);
    mBuffer.AppendData(value.wx_str(), len);
}

void ProjectSerializer::WriteName(const wxString& name)
{
    wxASSERT(name.length() * sizeof(wxStringCharType) <= SHRT_MAX);
    UShort id;

    auto nameiter = mNames.find(name);
    if (nameiter != mNames.end()) {
        id = nameiter->second;
    } else {
        // mNames is static.  This appends each name to static mDict only once
        // in each run.
        UShort len = name.length() * sizeof(wxStringCharType);

        id = mNames.size();
        mNames[name] = id;

        mDict.AppendByte(FT_Name);
        WriteUShort(mDict, id);
        WriteUShort(mDict, len);
        mDict.AppendData(name.wx_str(), len);

        mDictChanged = true;
    }

    WriteUShort(mBuffer, id);
}

const MemoryStream& ProjectSerializer::GetDict() const
{
    return mDict;
}

const MemoryStream& ProjectSerializer::GetData() const
{
    return mBuffer;
}

bool ProjectSerializer::IsEmpty() const
{
    return mBuffer.GetSize() == 0;
}

bool ProjectSerializer::DictChanged() const
{
    return mDictChanged;
}

// See ProjectFileIO::LoadProject() for explanation of the blockids arg
bool ProjectSerializer::Decode(BufferedStreamReader& in, XMLTagHandler* handler)
{
    if (handler == nullptr) {
        return false;
    }

    XMLTagHandlerAdapter adapter(handler);

    std::vector<char> bytes;
    IdMap mIds;
    std::vector<IdMap> mIdStack;
    char mCharSize = 0;

    mIds.clear();

    struct Error {}; // exception type for short-range try/catch
    auto Lookup = [&mIds]( UShort id ) -> std::string_view
    {
        auto iter = mIds.find(id);
        if (iter == mIds.end()) {
            throw Error{};
        }

        return iter->second;
    };

    int64_t stringsCount = 0;
    int64_t stringsLength = 0;

    auto ReadString = [&mCharSize, &in, &bytes, &stringsCount, &stringsLength](int len) -> std::string
    {
        bytes.reserve(len);
        auto data = bytes.data();
        in.Read(data, len);

        stringsCount++;
        stringsLength += len;

        switch (mCharSize) {
        case 1:
            return std::string(bytes.data(), len);

        case 2:
            return FastStringConvert<char16_t>(bytes.data(), len);

        case 4:
            return FastStringConvert<char32_t>(bytes.data(), len);

        default:
            wxASSERT_MSG(false, wxT("Characters size not 1, 2, or 4"));
            break;
        }

        return {};
    };

    try
    {
        while (!in.Eof())
        {
            UShort id;

            switch (in.GetC()) {
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
                id = ReadUShort(in);
                auto len = ReadUShort(in);
                mIds[id] = ReadString(len);
            }
            break;

            case FT_StartTag:
            {
                id = ReadUShort(in);

                adapter.EmitStartTag(Lookup(id));
            }
            break;

            case FT_EndTag:
            {
                id = ReadUShort(in);

                adapter.EndTag(Lookup(id));
            }
            break;

            case FT_String:
            {
                id = ReadUShort(in);
                int len = ReadLength(in);

                adapter.WriteAttr(Lookup(id), ReadString(len));
            }
            break;

            case FT_Float:
            {
                float val;

                id = ReadUShort(in);
                in.Read(&val, sizeof(val));
                /* int dig = */ ReadDigits(in);

                adapter.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_Double:
            {
                double val;

                id = ReadUShort(in);
                in.Read(&val, sizeof(val));
                /*int dig = */ ReadDigits(in);

                adapter.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_Int:
            {
                id = ReadUShort(in);
                int val = ReadInt(in);

                adapter.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_Bool:
            {
                unsigned char val;

                id = ReadUShort(in);
                in.Read(&val, 1);

                adapter.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_Long:
            {
                id = ReadUShort(in);
                long val = ReadLong(in);

                adapter.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_LongLong:
            {
                id = ReadUShort(in);
                long long val = ReadLongLong(in);
                adapter.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_SizeT:
            {
                id = ReadUShort(in);
                size_t val = ReadULong(in);

                adapter.WriteAttr(Lookup(id), val);
            }
            break;

            case FT_Data:
            {
                int len = ReadLength(in);
                adapter.WriteData(ReadString(len));
            }
            break;

            case FT_Raw:
            {
                int len = ReadLength(in);
                adapter.WriteRaw(ReadString(len));
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
    catch (const Error&)
    {
        // Document was corrupt, or platform differences in size or endianness
        // were not well canonicalized
        return false;
    }

    wxLogInfo(
        "Loaded %lld string %f Kb in size", stringsCount, stringsLength / 1024.0);

    return adapter.Finalize();
}
