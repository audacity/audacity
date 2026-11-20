/*
 * Audacity: A Digital Audio Editor
 */
#include "documentedit.h"

#include <limits>
#include <string>

#include "projectdocument.h"

using namespace au::projectrecovery;

namespace {
muse::Ret blockError(int64_t blockId, const std::string& what)
{
    return muse::make_ret(muse::Ret::Code::BadData, "block " + std::to_string(blockId) + ": " + what);
}

muse::Ret silence(XmlValue& blockId, const WaveBlock& block)
{
    if (block.length < 0) {
        return blockError(block.blockId, "the next block starts before it");
    }
    const bool wide = blockId.type == FieldType::LongLong;
    if (!wide && blockId.type != FieldType::Int && blockId.type != FieldType::Long) {
        return blockError(block.blockId, "its blockid is not a signed integer");
    }
    if (!wide && block.length > std::numeric_limits<int32_t>::max()) {
        return blockError(block.blockId, "its silence does not fit in its blockid");
    }
    blockId.integer = -block.length;
    return muse::make_ok();
}

muse::Ret silenceSequence(XmlNode& node, const std::set<int64_t>& ids)
{
    const Sequence sequence = readSequence(node);
    size_t index = 0;
    for (XmlNode& child : node.children) {
        if (child.tag != "waveblock") {
            continue;
        }
        const WaveBlock& block = sequence.blocks[index++];
        if (!ids.count(block.blockId)) {
            continue;
        }
        for (XmlAttribute& attribute : child.attributes) {
            if (attribute.name != "blockid") {
                continue;
            }
            const muse::Ret ret = silence(attribute.value, block);
            if (!ret) {
                return ret;
            }
        }
    }
    return muse::make_ok();
}

muse::Ret silenceClip(XmlNode& clip, const std::set<int64_t>& ids)
{
    for (XmlNode& child : clip.children) {
        muse::Ret ret = muse::make_ok();
        if (child.tag == "sequence") {
            ret = silenceSequence(child, ids);
        } else if (child.tag == "waveclip") {
            ret = silenceClip(child, ids);
        }
        if (!ret) {
            return ret;
        }
    }
    return muse::make_ok();
}
}

muse::Ret au::projectrecovery::silenceBlocks(XmlDocument& document, const std::set<int64_t>& ids)
{
    XmlNode* root = nullptr;
    for (XmlNode& element : document.elements) {
        if (element.tag == "project") {
            root = &element;
        }
    }
    if (!root) {
        return muse::make_ret(muse::Ret::Code::BadData, "the document has no project element");
    }
    for (XmlNode& track : root->children) {
        if (track.tag != "wavetrack") {
            continue;
        }
        for (XmlNode& clip : track.children) {
            if (clip.tag != "waveclip") {
                continue;
            }
            const muse::Ret ret = silenceClip(clip, ids);
            if (!ret) {
                return ret;
            }
        }
    }
    return muse::make_ok();
}
