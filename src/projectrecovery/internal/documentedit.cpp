/*
 * Audacity: A Digital Audio Editor
 */
#include "documentedit.h"

#include <limits>
#include <string>
#include <vector>

#include "projectdocument.h"

using namespace au::projectrecovery;

namespace {
muse::Ret blockError(const WaveBlock& block, const std::string& what)
{
    return muse::make_ret(muse::Ret::Code::BadData, "block " + std::to_string(block.blockId) + ": " + what);
}

muse::Ret silence(std::vector<uint8_t>& doc, size_t dictSize, const WaveBlock& block)
{
    if (block.length < 0) {
        return blockError(block, "the next block starts before it");
    }
    // The value follows the field type and the 2-byte name id
    if (block.blockIdOffset < dictSize + 3 || block.blockIdOffset - dictSize >= doc.size()) {
        return blockError(block, "its blockid is not in the doc");
    }
    const size_t position = block.blockIdOffset - dictSize;
    const auto type = static_cast<FieldType>(doc[position - 3]);
    size_t size = 0;
    if (type == FieldType::LongLong) {
        size = 8;
    } else if (type == FieldType::Int || type == FieldType::Long) {
        size = 4;
    }
    if (size == 0 || position + size > doc.size()) {
        return blockError(block, "its blockid is not a signed integer");
    }
    if (size == 4 && block.length > std::numeric_limits<int32_t>::max()) {
        return blockError(block, "its silence does not fit in its blockid");
    }

    uint64_t stored = 0;
    for (size_t i = 0; i < size; ++i) {
        stored |= static_cast<uint64_t>(doc[position + i]) << (8 * i);
    }
    const int64_t id = size == 8 ? static_cast<int64_t>(stored) : static_cast<int32_t>(static_cast<uint32_t>(stored));
    if (id != block.blockId) {
        return blockError(block, "the bytes of its blockid hold " + std::to_string(id));
    }

    const auto value = static_cast<uint64_t>(-block.length);
    for (size_t i = 0; i < size; ++i) {
        doc[position + i] = static_cast<uint8_t>(value >> (8 * i));
    }
    return muse::make_ok();
}

void collectBlocks(const WaveClip& clip, std::vector<const WaveBlock*>& blocks)
{
    for (const Sequence& sequence : clip.sequences) {
        for (const WaveBlock& block : sequence.blocks) {
            blocks.push_back(&block);
        }
    }
    for (const WaveClip& cutLine : clip.cutLines) {
        collectBlocks(cutLine, blocks);
    }
}
}

muse::RetVal<muse::ByteArray> au::projectrecovery::silenceBlocks(const BinaryXml& binary, const std::set<int64_t>& ids)
{
    const muse::RetVal<XmlDocument> decoded = decodeBinaryXml(binary);
    if (!decoded.ret) {
        return muse::RetVal<muse::ByteArray>::make_ret(decoded.ret);
    }
    const std::optional<ProjectDocument> project = readProjectDocument(decoded.val);
    if (!project) {
        return muse::RetVal<muse::ByteArray>::make_ret(muse::make_ret(muse::Ret::Code::BadData, "the document has no project element"));
    }

    std::vector<const WaveBlock*> blocks;
    for (const WaveTrack& track : project->waveTracks) {
        for (const WaveClip& clip : track.clips) {
            collectBlocks(clip, blocks);
        }
    }

    std::vector<uint8_t> doc(binary.doc.constData(), binary.doc.constData() + binary.doc.size());
    for (const WaveBlock* block : blocks) {
        if (!ids.count(block->blockId)) {
            continue;
        }
        const muse::Ret ret = silence(doc, binary.dict.size(), *block);
        if (!ret) {
            return muse::RetVal<muse::ByteArray>::make_ret(ret);
        }
    }
    return muse::RetVal<muse::ByteArray>::make_ok(muse::ByteArray(doc.data(), doc.size()));
}
