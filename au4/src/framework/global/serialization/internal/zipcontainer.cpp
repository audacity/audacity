/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include "zipcontainer.h"

#include <ctime>
#include <cstring>
#include <zlib.h>

#include "global/io/dir.h"

#include "log.h"

// Zip standard version for archives handled by this API
// (actually, the only basic support of this version is implemented but it is enough for now)
#define ZIP_VERSION 20

#if 0
#define ZDEBUG LOGD
#else
#define ZDEBUG if (0) LOGD
#endif

using namespace mu::io;

typedef unsigned long int ulong;
typedef unsigned short int ushort;
typedef unsigned int uint;

namespace mu {
static inline uint readUInt(const uint8_t* data)
{
    return (data[0]) + (data[1] << 8) + (data[2] << 16) + (data[3] << 24);
}

static inline ushort readUShort(const uint8_t* data)
{
    return (data[0]) + (data[1] << 8);
}

static inline void writeUInt(uint8_t* data, uint i)
{
    data[0] = i & 0xff;
    data[1] = (i >> 8) & 0xff;
    data[2] = (i >> 16) & 0xff;
    data[3] = (i >> 24) & 0xff;
}

static inline void writeUShort(uint8_t* data, ushort i)
{
    data[0] = i & 0xff;
    data[1] = (i >> 8) & 0xff;
}

static inline void copyUInt(uint8_t* dest, const uint8_t* src)
{
    dest[0] = src[0];
    dest[1] = src[1];
    dest[2] = src[2];
    dest[3] = src[3];
}

static inline void copyUShort(uint8_t* dest, const uint8_t* src)
{
    dest[0] = src[0];
    dest[1] = src[1];
}

static void writeMSDosDate(uint8_t* dest, const std::tm& dt)
{
    if (dt.tm_year > 0) {
        uint16_t time
            =(dt.tm_hour << 11)     // 5 bit hour
              | (dt.tm_min << 5)    // 6 bit minute
              | (dt.tm_sec >> 1);   // 5 bit double seconds

        dest[0] = time & 0xff;
        dest[1] = time >> 8;

        uint16_t date
            =((dt.tm_year + 1900 - 1980) << 9) // 7 bit year 1980-based
              | ((dt.tm_mon + 1) << 5)         // 4 bit month
              | (dt.tm_mday);                  // 5 bit day

        dest[2] = char(date);
        dest[3] = char(date >> 8);
    } else {
        dest[0] = 0;
        dest[1] = 0;
        dest[2] = 0;
        dest[3] = 0;
    }
}

static int inflate(Bytef* dest, ulong* destLen, const Bytef* source, ulong sourceLen)
{
    z_stream stream;
    int err;

    stream.next_in = const_cast<Bytef*>(source);
    stream.avail_in = (uInt)sourceLen;
    if ((uLong)stream.avail_in != sourceLen) {
        return Z_BUF_ERROR;
    }

    stream.next_out = dest;
    stream.avail_out = (uInt) * destLen;
    if ((uLong)stream.avail_out != *destLen) {
        return Z_BUF_ERROR;
    }

    stream.zalloc = (alloc_func)0;
    stream.zfree = (free_func)0;

    err = inflateInit2(&stream, -MAX_WBITS);
    if (err != Z_OK) {
        return err;
    }

    err = inflate(&stream, Z_FINISH);
    if (err != Z_STREAM_END) {
        inflateEnd(&stream);
        if (err == Z_NEED_DICT || (err == Z_BUF_ERROR && stream.avail_in == 0)) {
            return Z_DATA_ERROR;
        }
        return err;
    }
    *destLen = stream.total_out;

    err = inflateEnd(&stream);
    return err;
}

static int deflate(Bytef* dest, ulong* destLen, const Bytef* source, ulong sourceLen)
{
    z_stream stream;
    int err;

    stream.next_in = const_cast<Bytef*>(source);
    stream.avail_in = (uInt)sourceLen;
    stream.next_out = dest;
    stream.avail_out = (uInt) * destLen;
    if ((uLong)stream.avail_out != *destLen) {
        return Z_BUF_ERROR;
    }

    stream.zalloc = (alloc_func)0;
    stream.zfree = (free_func)0;
    stream.opaque = (voidpf)0;

    err = deflateInit2(&stream, Z_DEFAULT_COMPRESSION, Z_DEFLATED, -MAX_WBITS, 8, Z_DEFAULT_STRATEGY);
    if (err != Z_OK) {
        return err;
    }

    err = deflate(&stream, Z_FINISH);
    if (err != Z_STREAM_END) {
        deflateEnd(&stream);
        return err == Z_OK ? Z_BUF_ERROR : err;
    }
    *destLen = stream.total_out;

    err = deflateEnd(&stream);
    return err;
}

namespace WindowsFileAttributes {
enum {
    Dir        = 0x10, // FILE_ATTRIBUTE_DIRECTORY
    File       = 0x80, // FILE_ATTRIBUTE_NORMAL
    TypeMask   = 0x90,

    ReadOnly   = 0x01, // FILE_ATTRIBUTE_READONLY
    PermMask   = 0x01
};
}

namespace UnixFileAttributes {
enum {
    Dir        = 0040000, // __S_IFDIR
    File       = 0100000, // __S_IFREG
    SymLink    = 0120000, // __S_IFLNK
    TypeMask   = 0170000, // __S_IFMT

    ReadUser   = 0400, // __S_IRUSR
    WriteUser  = 0200, // __S_IWUSR
    ExeUser    = 0100, // __S_IXUSR
    ReadGroup  = 0040, // __S_IRGRP
    WriteGroup = 0020, // __S_IWGRP
    ExeGroup   = 0010, // __S_IXGRP
    ReadOther  = 0004, // __S_IROTH
    WriteOther = 0002, // __S_IWOTH
    ExeOther   = 0001, // __S_IXOTH
    PermMask   = 0777
};
}

static std::tm readMSDosDate(const uint8_t* src)
{
    std::tm tm;
    uint dosDate = readUInt(src);
    uint64_t uDate;
    uDate = (uint64_t)(dosDate >> 16);
    tm.tm_mday = (uDate & 0x1f);
    tm.tm_mon = ((uDate & 0x1E0) >> 5) - 1;
    tm.tm_year = (((uDate & 0x0FE00) >> 9) + 1980 - 1900);
    tm.tm_hour = ((dosDate & 0xF800) >> 11);
    tm.tm_min =  ((dosDate & 0x7E0) >> 5);
    tm.tm_sec =  ((dosDate & 0x1f) << 1);

    return tm;
}

// for details, see http://www.pkware.com/documents/casestudies/APPNOTE.TXT

enum HostOS {
    HostFAT      = 0,
    HostAMIGA    = 1,
    HostVMS      = 2,  // VAX/VMS
    HostUnix     = 3,
    HostVM_CMS   = 4,
    HostAtari    = 5,  // what if it's a minix filesystem? [cjh]
    HostHPFS     = 6,  // filesystem used by OS/2 (and NT 3.x)
    HostMac      = 7,
    HostZ_System = 8,
    HostCPM      = 9,
    HostTOPS20   = 10, // pkzip 2.50 NTFS
    HostNTFS     = 11, // filesystem used by Windows NT
    HostQDOS     = 12, // SMS/QDOS
    HostAcorn    = 13, // Archimedes Acorn RISC OS
    HostVFAT     = 14, // filesystem used by Windows 95, NT
    HostMVS      = 15,
    HostBeOS     = 16, // hybrid POSIX/database filesystem
    HostTandem   = 17,
    HostOS400    = 18,
    HostOSX      = 19
};

enum GeneralPurposeFlag {
    Encrypted = 0x01,
    AlgTune1 = 0x02,
    AlgTune2 = 0x04,
    HasDataDescriptor = 0x08,
    PatchedData = 0x20,
    StrongEncrypted = 0x40,
    Utf8Names = 0x0800,
    CentralDirectoryEncrypted = 0x2000
};

enum CompressionMethod {
    CompressionMethodStored = 0,
    CompressionMethodShrunk = 1,
    CompressionMethodReduced1 = 2,
    CompressionMethodReduced2 = 3,
    CompressionMethodReduced3 = 4,
    CompressionMethodReduced4 = 5,
    CompressionMethodImploded = 6,
    CompressionMethodReservedTokenizing = 7, // reserved for tokenizing
    CompressionMethodDeflated = 8,
    CompressionMethodDeflated64 = 9,
    CompressionMethodPKImploding = 10,

    CompressionMethodBZip2 = 12,

    CompressionMethodLZMA = 14,

    CompressionMethodTerse = 18,
    CompressionMethodLz77 = 19,

    CompressionMethodJpeg = 96,
    CompressionMethodWavPack = 97,
    CompressionMethodPPMd = 98,
    CompressionMethodWzAES = 99
};

struct LocalFileHeader
{
    uint8_t signature[4]; //  0x04034b50
    uint8_t version_needed[2];
    uint8_t general_purpose_bits[2];
    uint8_t compression_method[2];
    uint8_t last_mod_file[4];
    uint8_t crc_32[4];
    uint8_t compressed_size[4];
    uint8_t uncompressed_size[4];
    uint8_t file_name_length[2];
    uint8_t extra_field_length[2];
};

struct DataDescriptor
{
    uint8_t crc_32[4];
    uint8_t compressed_size[4];
    uint8_t uncompressed_size[4];
};

struct CentralFileHeader
{
    uint8_t signature[4]; // 0x02014b50
    uint8_t version_made[2];
    uint8_t version_needed[2];
    uint8_t general_purpose_bits[2];
    uint8_t compression_method[2];
    uint8_t last_mod_file[4];
    uint8_t crc_32[4];
    uint8_t compressed_size[4];
    uint8_t uncompressed_size[4];
    uint8_t file_name_length[2];
    uint8_t extra_field_length[2];
    uint8_t file_comment_length[2];
    uint8_t disk_start[2];
    uint8_t internal_file_attributes[2];
    uint8_t external_file_attributes[4];
    uint8_t offset_local_header[4];
    LocalFileHeader toLocalHeader() const;
};

struct EndOfDirectory
{
    uint8_t signature[4]; // 0x06054b50
    uint8_t this_disk[2];
    uint8_t start_of_directory_disk[2];
    uint8_t num_dir_entries_this_disk[2];
    uint8_t num_dir_entries[2];
    uint8_t directory_size[4];
    uint8_t dir_start_offset[4];
    uint8_t comment_length[2];
};

struct FileHeader
{
    CentralFileHeader h;
    ByteArray file_name;
    ByteArray extra_field;
    ByteArray file_comment;
};

LocalFileHeader CentralFileHeader::toLocalHeader() const
{
    LocalFileHeader h;
    writeUInt(h.signature, 0x04034b50);
    copyUShort(h.version_needed, version_needed);
    copyUShort(h.general_purpose_bits, general_purpose_bits);
    copyUShort(h.compression_method, compression_method);
    copyUInt(h.last_mod_file, last_mod_file);
    copyUInt(h.crc_32, crc_32);
    copyUInt(h.compressed_size, compressed_size);
    copyUInt(h.uncompressed_size, uncompressed_size);
    copyUShort(h.file_name_length, file_name_length);
    copyUShort(h.extra_field_length, extra_field_length);
    return h;
}

struct ZipContainer::Impl {
    IODevice* device = nullptr;

    bool dirtyFileTree = true;
    std::vector<FileHeader> fileHeaders;
    ByteArray comment;
    uint start_of_directory = 0;
    ZipContainer::Status status = ZipContainer::NoError;

    ZipContainer::CompressionPolicy compressionPolicy = ZipContainer::AlwaysCompress;

    enum EntryType {
        Directory, File, Symlink
    };

    void addEntry(EntryType type, const std::string& fileName, const ByteArray& contents);
    bool writeToDevice(const uint8_t* data, size_t len);
    bool writeToDevice(const ByteArray& data);

    Impl(IODevice* d)
        : device(d) {}

    void scanFiles();
    ZipContainer::FileInfo fillFileInfo(size_t index) const;
};

void ZipContainer::Impl::scanFiles()
{
    if (!dirtyFileTree) {
        return;
    }

    if (!(device->isOpen() || device->open(IODevice::ReadOnly))) {
        status = ZipContainer::FileOpenError;
        return;
    }

    if ((device->openMode() & IODevice::ReadOnly) == 0) { // only read the index from readable files.
        status = ZipContainer::FileReadError;
        return;
    }

    dirtyFileTree = false;
    uint8_t tmp[4];
    device->read(tmp, 4);
    if (readUInt(tmp) != 0x04034b50) {
        LOGW("Zip: not a zip file!");
        return;
    }

    // find EndOfDirectory header
    int i = 0;
    int start_of_directory_local = -1;
    int num_dir_entries = 0;
    EndOfDirectory eod;
    while (start_of_directory_local == -1) {
        const int pos = int(device->size()) - int(sizeof(EndOfDirectory)) - i;
        if (pos < 0 || i > 65535) {
            LOGW("Zip: EndOfDirectory not found");
            return;
        }

        device->seek(pos);
        device->read((uint8_t*)&eod, sizeof(EndOfDirectory));
        if (readUInt(eod.signature) == 0x06054b50) {
            break;
        }
        ++i;
    }

    // have the eod
    start_of_directory_local = readUInt(eod.dir_start_offset);
    num_dir_entries = readUShort(eod.num_dir_entries);
    ZDEBUG("start_of_directory at %d, num_dir_entries=%d", start_of_directory_local, num_dir_entries);
    int comment_length = readUShort(eod.comment_length);
    if (comment_length != i) {
        LOGW("Zip: failed to parse zip file.");
    }
    comment = device->read(std::min(comment_length, i));

    device->seek(start_of_directory_local);
    for (i = 0; i < num_dir_entries; ++i) {
        FileHeader header;
        int read = (int)device->read((uint8_t*)&header.h, sizeof(CentralFileHeader));
        if (read < (int)sizeof(CentralFileHeader)) {
            LOGW("Zip: Failed to read complete header, index may be incomplete");
            break;
        }
        if (readUInt(header.h.signature) != 0x02014b50) {
            LOGW("Zip: invalid header signature, index may be incomplete");
            break;
        }

        size_t l = readUShort(header.h.file_name_length);
        header.file_name = device->read(l);
        if (header.file_name.size() != l) {
            LOGW("Zip: Failed to read filename from zip index, index may be incomplete");
            break;
        }
        l = readUShort(header.h.extra_field_length);
        header.extra_field = device->read(l);
        if (header.extra_field.size() != l) {
            LOGW("Zip: Failed to read extra field in zip file, skipping file, index may be incomplete");
            break;
        }
        l = readUShort(header.h.file_comment_length);
        header.file_comment = device->read(l);
        if (header.file_comment.size() != l) {
            LOGW("Zip: Failed to read read file comment, index may be incomplete");
            break;
        }

        ZDEBUG("found file '%s'", header.file_name.data());
        fileHeaders.push_back(header);
    }
}

ZipContainer::FileInfo ZipContainer::Impl::fillFileInfo(size_t index) const
{
    ZipContainer::FileInfo fileInfo;
    FileHeader header = fileHeaders.at(index);
    uint32_t mode = readUInt(header.h.external_file_attributes);
    const HostOS hostOS = HostOS(readUShort(header.h.version_made) >> 8);
    switch (hostOS) {
    case HostUnix:
        mode = (mode >> 16) & 0xffff;
        switch (mode & UnixFileAttributes::TypeMask) {
        case UnixFileAttributes::SymLink:
            fileInfo.isSymLink = true;
            break;
        case UnixFileAttributes::Dir:
            fileInfo.isDir = true;
            break;
        case UnixFileAttributes::File:
        default: // ### just for the case; should we warn?
            fileInfo.isFile = true;
            break;
        }
        break;
    case HostFAT:
    case HostNTFS:
    case HostHPFS:
    case HostVFAT:
        switch (mode & WindowsFileAttributes::TypeMask) {
        case WindowsFileAttributes::Dir:
            fileInfo.isDir = true;
            break;
        case WindowsFileAttributes::File:
        default:
            fileInfo.isFile = true;
            break;
        }

        break;
    default:
        LOGW("Zip: Zip entry format at %zu is not supported.", index);
        return fileInfo; // we don't support anything else
    }

    // ushort general_purpose_bits = readUShort(header.h.general_purpose_bits);
    // if bit 11 is set, the filename and comment fields must be encoded using UTF-8
    // const bool inUtf8 = (general_purpose_bits & Utf8Names) != 0;
    fileInfo.filePath = header.file_name.constChar();
    fileInfo.crc = readUInt(header.h.crc_32);
    fileInfo.size = readUInt(header.h.uncompressed_size);
    fileInfo.lastModified = readMSDosDate(header.h.last_mod_file);

    // fix the file path, if broken (convert separators, eat leading and trailing ones)
    fileInfo.filePath = Dir::fromNativeSeparators(fileInfo.filePath).toStdString();
    {
        bool frontOk = false;
        while (!fileInfo.filePath.empty() && !frontOk) {
            if (fileInfo.filePath.front() == '/') {
                fileInfo.filePath = fileInfo.filePath.substr(1);
            } else if (fileInfo.filePath.rfind("./", 0) == 0) {
                fileInfo.filePath = fileInfo.filePath.substr(2);
            } else if (fileInfo.filePath.rfind("../", 0) == 0) {
                fileInfo.filePath = fileInfo.filePath.substr(3);
            } else {
                frontOk = true;
            }
        }
    }
    while (!fileInfo.filePath.empty() && fileInfo.filePath.back() == '/') {
        fileInfo.filePath = fileInfo.filePath.substr(0, fileInfo.filePath.size() - 1);
    }

    return fileInfo;
}

void ZipContainer::Impl::addEntry(EntryType type, const std::string& fileName, const ByteArray& contents)
{
    if (!(device->isOpen() || device->open(IODevice::WriteOnly))) {
        status = ZipContainer::FileOpenError;
        return;
    }
    device->seek(start_of_directory);

    // don't compress small files
    ZipContainer::CompressionPolicy compression = compressionPolicy;
    if (compressionPolicy == ZipContainer::AutoCompress) {
        if (contents.size() < 64) {
            compression = ZipContainer::NeverCompress;
        } else {
            compression = ZipContainer::AlwaysCompress;
        }
    }

    FileHeader header;
    std::memset(&header.h, 0, sizeof(CentralFileHeader));
    writeUInt(header.h.signature, 0x02014b50);

    writeUShort(header.h.version_needed, ZIP_VERSION);
    writeUInt(header.h.uncompressed_size, (uint)contents.size());

    std::time_t t = std::time(0);   // get time now
    std::tm now;
#ifdef WIN32
    localtime_s(&now, &t);
#else
    localtime_r(&t, &now);
#endif
    writeMSDosDate(header.h.last_mod_file, now);
    ByteArray data = contents;
    if (compression == ZipContainer::AlwaysCompress) {
        writeUShort(header.h.compression_method, CompressionMethodDeflated);

        ulong len = (ulong)contents.size();
        // shamelessly copied form zlib
        len += (len >> 12) + (len >> 14) + 11;
        int res;
        do {
            data.resize(len);
            res = deflate((uint8_t*)data.data(), &len, (const uint8_t*)contents.constData(), (ulong)contents.size());

            switch (res) {
            case Z_OK:
                data.resize(len);
                break;
            case Z_MEM_ERROR:
                LOGW("Zip: Z_MEM_ERROR: Not enough memory to compress file, skipping");
                data.resize(0);
                break;
            case Z_BUF_ERROR:
                len *= 2;
                break;
            }
        } while (res == Z_BUF_ERROR);
    }
// TODO add a check if data.size() > contents.size().  Then try to store the original and revert the compression method to be uncompressed
    writeUInt(header.h.compressed_size, (uint)data.size());
    uint crc_32 = ::crc32(0, 0, 0);
    crc_32 = ::crc32(crc_32, (const uint8_t*)contents.constData(), (uint)contents.size());
    writeUInt(header.h.crc_32, crc_32);

    // if bit 11 is set, the filename and comment fields must be encoded using UTF-8
    ushort general_purpose_bits = Utf8Names; // always use utf-8
    writeUShort(header.h.general_purpose_bits, general_purpose_bits);

    //const bool inUtf8 = (general_purpose_bits & Utf8Names) != 0;
    header.file_name = ByteArray(fileName.c_str(), fileName.size());
    if (header.file_name.size() > 0xffff) {
        LOGW("Zip: Filename is too long, chopping it to 65535 bytes");
        header.file_name = header.file_name.left(0xffff); // ### don't break the utf-8 sequence, if any
    }
    if (header.file_comment.size() + header.file_name.size() > 0xffff) {
        LOGW("Zip: File comment is too long, chopping it to 65535 bytes");
        header.file_comment.truncate(0xffff - header.file_name.size()); // ### don't break the utf-8 sequence, if any
    }
    writeUShort(header.h.file_name_length, (ushort)header.file_name.size());
    //h.extra_field_length[2];

    writeUShort(header.h.version_made, HostUnix << 8);
    //uint8_t internal_file_attributes[2];
    //uint8_t external_file_attributes[4];
    uint32_t mode = UnixFileAttributes::ReadUser
                    | UnixFileAttributes::WriteUser
                    | UnixFileAttributes::ExeUser
                    | UnixFileAttributes::ReadGroup
                    | UnixFileAttributes::ReadOther;
    switch (type) {
    case Symlink:
        mode |= UnixFileAttributes::SymLink;
        break;
    case Directory:
        mode |= UnixFileAttributes::Dir;
        break;
    case File:
        mode |= UnixFileAttributes::File;
        break;
    default:
        UNREACHABLE;
        break;
    }
    writeUInt(header.h.external_file_attributes, mode << 16);
    writeUInt(header.h.offset_local_header, start_of_directory);

    fileHeaders.push_back(header);

    bool ok = true;

    LocalFileHeader h = header.h.toLocalHeader();
    ok &= writeToDevice((const uint8_t*)&h, sizeof(LocalFileHeader));
    ok &= writeToDevice(header.file_name);
    ok &= writeToDevice(data);

    start_of_directory = (uint)device->pos();
    dirtyFileTree = true;

    if (!ok) {
        status = ZipContainer::FileWriteError;
    }
}

bool ZipContainer::Impl::writeToDevice(const uint8_t* data, size_t len)
{
    return device->write(data, len) == len;
}

bool ZipContainer::Impl::writeToDevice(const ByteArray& data)
{
    return device->write(data) == data.size();
}

ZipContainer::ZipContainer(IODevice* device)
    : p(new Impl(device))
{
    assert(device);
}

ZipContainer::~ZipContainer()
{
    close();
    delete p;
}

std::vector<ZipContainer::FileInfo> ZipContainer::fileInfoList() const
{
    p->scanFiles();
    std::vector<FileInfo> files;
    const size_t numFileHeaders = p->fileHeaders.size();
    files.reserve(numFileHeaders);
    for (size_t i = 0; i < numFileHeaders; ++i) {
        files.push_back(p->fillFileInfo(i));
    }
    return files;
}

int ZipContainer::count() const
{
    p->scanFiles();
    return (int)p->fileHeaders.size();
}

bool ZipContainer::fileExists(const std::string& fileName) const
{
    p->scanFiles();
    ByteArray fileNameBa = ByteArray::fromRawData(fileName.c_str(), fileName.size());
    for (size_t i = 0; i < p->fileHeaders.size(); ++i) {
        if (p->fileHeaders.at(i).file_name == fileNameBa) {
            return true;
        }
    }
    return false;
}

ByteArray ZipContainer::fileData(const std::string& fileName) const
{
    p->scanFiles();

    ByteArray fileNameBa = ByteArray::fromRawData(fileName.c_str(), fileName.size());

    size_t i;
    for (i = 0; i < p->fileHeaders.size(); ++i) {
        if (p->fileHeaders.at(i).file_name == fileNameBa) {
            break;
        }
    }

    if (i == p->fileHeaders.size()) {
        return ByteArray();
    }

    FileHeader header = p->fileHeaders.at(i);

    ushort version_needed = readUShort(header.h.version_needed);
    if (version_needed > ZIP_VERSION) {
        LOGW("Zip: .ZIP specification version %d implementation is needed to extract the data.", version_needed);
        return ByteArray();
    }

    ushort general_purpose_bits = readUShort(header.h.general_purpose_bits);
    int compressed_size = readUInt(header.h.compressed_size);
    int uncompressed_size = readUInt(header.h.uncompressed_size);
    int start = readUInt(header.h.offset_local_header);

    p->device->seek(start);
    LocalFileHeader lh;
    p->device->read((uint8_t*)&lh, sizeof(LocalFileHeader));
    uint skip = readUShort(lh.file_name_length) + readUShort(lh.extra_field_length);
    p->device->seek(p->device->pos() + skip);

    int compression_method = readUShort(lh.compression_method);

    if ((general_purpose_bits & Encrypted) != 0) {
        LOGW("Zip: Unsupported encryption method is needed to extract the data.");
        return ByteArray();
    }

    ByteArray compressed = p->device->read(compressed_size);
    if (compression_method == CompressionMethodStored) {
        // no compression
        compressed.truncate(uncompressed_size);
        return compressed;
    } else if (compression_method == CompressionMethodDeflated) {
        // Deflate
        //qDebug("compressed=%d", compressed.size());
        compressed.truncate(compressed_size);
        ByteArray baunzip;
        ulong len = std::max(uncompressed_size,  1);
        int res;
        do {
            baunzip.resize(len);
            res = inflate((uint8_t*)baunzip.data(), &len,
                          (const uint8_t*)compressed.constData(), compressed_size);

            switch (res) {
            case Z_OK:
                if ((size_t)len != baunzip.size()) {
                    baunzip.resize(len);
                }
                break;
            case Z_MEM_ERROR:
                LOGW("Zip: Z_MEM_ERROR: Not enough memory");
                break;
            case Z_BUF_ERROR:
                len *= 2;
                break;
            case Z_DATA_ERROR:
                LOGW("Zip: Z_DATA_ERROR: Input data is corrupted");
                break;
            }
        } while (res == Z_BUF_ERROR);
        return baunzip;
    }

    LOGW("Zip: Unsupported compression method %d is needed to extract the data.", compression_method);
    return ByteArray();
}

ZipContainer::Status ZipContainer::status() const
{
    return p->status;
}

void ZipContainer::setCompressionPolicy(CompressionPolicy policy)
{
    p->compressionPolicy = policy;
}

ZipContainer::CompressionPolicy ZipContainer::compressionPolicy() const
{
    return p->compressionPolicy;
}

void ZipContainer::addFile(const std::string& fileName, const ByteArray& data)
{
    p->addEntry(Impl::File, Dir::fromNativeSeparators(fileName).toStdString(), data);
}

void ZipContainer::addDirectory(const std::string& dirName)
{
    std::string name(Dir::fromNativeSeparators(dirName).toStdString());
    // separator is mandatory
    if (name.back() != '/') {
        name.push_back('/');
    }
    p->addEntry(Impl::Directory, name, ByteArray());
}

void ZipContainer::close()
{
    if (!(p->device->openMode() & IODevice::WriteOnly)) {
        p->device->close();
        return;
    }

    bool ok = true;

    //qDebug("Zip::close writing directory, %d entries", p->fileHeaders.size());
    p->device->seek(p->start_of_directory);
    // write new directory
    for (size_t i = 0; i < p->fileHeaders.size(); ++i) {
        const FileHeader& header = p->fileHeaders.at(i);
        ok &= p->writeToDevice((const uint8_t*)&header.h, sizeof(CentralFileHeader));
        ok &= p->writeToDevice(header.file_name);
        ok &= p->writeToDevice(header.extra_field);
        ok &= p->writeToDevice(header.file_comment);
    }
    int dir_size = (int)p->device->pos() - (int)p->start_of_directory;
    // write end of directory
    EndOfDirectory eod;
    memset(&eod, 0, sizeof(EndOfDirectory));
    writeUInt(eod.signature, 0x06054b50);
    //uint8_t this_disk[2];
    //uint8_t start_of_directory_disk[2];
    writeUShort(eod.num_dir_entries_this_disk, (ushort)p->fileHeaders.size());
    writeUShort(eod.num_dir_entries, (ushort)p->fileHeaders.size());
    writeUInt(eod.directory_size, dir_size);
    writeUInt(eod.dir_start_offset, p->start_of_directory);
    writeUShort(eod.comment_length, (ushort)p->comment.size());

    ok &= p->writeToDevice((const uint8_t*)&eod, sizeof(EndOfDirectory));
    ok &= p->writeToDevice(p->comment);
    p->device->close();

    if (!ok) {
        p->status = ZipContainer::FileWriteError;
    }
}
}
