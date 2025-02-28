/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  GetAcidizerTags.cpp

  Matthieu Hodgkinson

**********************************************************************/
#include "GetAcidizerTags.h"
#include "AcidizerTags.h"

#include <algorithm>
#include <array>
#include <cstring> // memset
#include <memory>

namespace LibImportExport {
std::optional<LibFileFormats::AcidizerTags> GetAcidizerTags(
    SNDFILE& file, const std::vector<std::string>& trustedDistributors)
{
    SF_LOOP_INFO loopInfo;
    if (
        sf_command(&file, SFC_GET_LOOP_INFO, &loopInfo, sizeof(loopInfo))
        == SF_FALSE) {
        return {}
    }

    if (
        loopInfo.loop_mode == SF_LOOP_BACKWARD
        || loopInfo.loop_mode == SF_LOOP_ALTERNATING) {
        // Don't know what that is:
        return {}
    }

    if (loopInfo.loop_mode == SF_LOOP_NONE) {
        return LibFileFormats::AcidizerTags::OneShot {}
    }

    if (loopInfo.num_beats != 0) {
        // Forward loop with number of beats set: all files like these I have seen
        // so far were correctly tagged.
        SF_INFO info;
        std::memset(&info, 0, sizeof(info));
        sf_command(&file, SFC_GET_CURRENT_SF_INFO, &info, sizeof(info));
        if (info.samplerate == 0 || info.frames == 0) {
            return {}
        }
        const auto duration = 1. * info.frames / info.samplerate;
        return LibFileFormats::AcidizerTags::Loop { 60. * loopInfo.num_beats
                                                    / duration };
    }

    // There is loop info, but in some unexpected combination. We don't trust it
    // unless it is from a trusted distributor.
    SF_CHUNK_INFO info;
    constexpr std::array<char, 4> listId = { 'L', 'I', 'S', 'T' };
    std::copy(listId.begin(), listId.end(), info.id);
    std::fill(info.id + sizeof(listId), info.id + sizeof(info.id), '\0');
    info.id_size = sizeof(listId);
    auto chunkIt = sf_get_chunk_iterator(&file, &info);
    while (chunkIt)
    {
        if (sf_get_chunk_size(chunkIt, &info) != SF_ERR_NO_ERROR) {
            break;
        }
        constexpr std::array<char, 4> INFO = { 'I', 'N', 'F', 'O' };
        constexpr std::array<char, 4> IDST = { 'I', 'D', 'S', 'T' };
        // Another 4 bytes after INFO and IDST that indicates the size of the data
        constexpr auto dataPos = sizeof(INFO) + sizeof(IDST) + 4;
        if (info.datalen < dataPos) {
            // Not the expected data
            continue;
        }
        const auto chars = std::make_unique<char[]>(info.datalen);
        info.data = chars.get();
        if (sf_get_chunk_data(chunkIt, &info) != SF_ERR_NO_ERROR) {
            break;
        }
        chunkIt = sf_next_chunk_iterator(chunkIt);

        auto pos = 0;
        const auto firstFour
            =std::string { chars.get() + pos, chars.get() + pos + sizeof(INFO) };
        if (firstFour != std::string { INFO.data(), INFO.size() }) {
            continue;
        }

        pos += sizeof(INFO);
        const auto nextFour
            =std::string { chars.get() + pos, chars.get() + pos + sizeof(IDST) };
        if (nextFour != std::string { IDST.data(), IDST.size() }) {
            continue;
        }

        // Ignore trailing nulls, which could be the result of byte-padding for
        // word alignment:
        const auto charsEnd = std::find_if(
            chars.get() + dataPos, chars.get() + info.datalen,
            [](const char c) { return c == '\0'; });
        const auto distributor = std::string { chars.get() + 12, charsEnd };
        const auto isTrusted
            =std::find(
                  trustedDistributors.begin(), trustedDistributors.end(),
                  distributor) != trustedDistributors.end();
        if (isTrusted) {
            // Later we may want to get the key, too, but for now we're only
            // interested in BPM.
            return LibFileFormats::AcidizerTags::Loop { loopInfo.bpm }
        }
    }

    // No luck:
    return {};
}
} // namespace LibImportExport
