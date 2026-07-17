/*
* Audacity: A Digital Audio Editor
*/
#include <gtest/gtest.h>

#include "../internal/effectsutils.h"

using namespace au::effects;

namespace {
EffectId makeId(const std::string& vendor, const std::string& name, const std::string& path)
{
    return EffectId::fromStdString("Effect_VST3_" + vendor + "_" + name + "_" + path);
}

EffectMeta makeVst3Meta(const EffectId& id)
{
    EffectMeta meta;
    meta.id = id;
    meta.family = EffectFamily::VST3;
    meta.state = muse::audioplugins::AudioPluginState::Validated;
    return meta;
}
}

TEST(EffectsBase_EffectsUtilsTests, FindRelocatedVst3EffectId_ReturnsUniqueClassIdMatch)
{
    const EffectId oldId = makeId("Soap Audio", "Soap Voice Cleaner",
                                  "C:/Program Files/Common Files/VST3/Soap Voice Cleaner.vst3;"
                                  "ABCDEF019182FAEB536F6170536F6170");
    const EffectId newId = makeId("Soap Audio", "Soap Voice Cleaner",
                                  "D:/Audio/VST3/Soap Voice Cleaner.vst3;"
                                  "ABCDEF019182FAEB536F6170536F6170");

    EXPECT_EQ(utils::findRelocatedVst3EffectId(oldId, { makeVst3Meta(newId) }), newId);
}

TEST(EffectsBase_EffectsUtilsTests, FindRelocatedVst3EffectId_UsesVendorAndNameWhenClassIdIsDuplicated)
{
    const EffectId oldId = makeId("Soap Audio", "Soap Voice Cleaner",
                                  "C:/Program Files/Common Files/VST3/Soap Voice Cleaner.vst3;"
                                  "ABCDEF019182FAEB536F6170536F6170");
    const EffectId matchingId = makeId("Soap Audio", "Soap Voice Cleaner",
                                       "D:/Audio/VST3/Soap Voice Cleaner.vst3;"
                                       "ABCDEF019182FAEB536F6170536F6170");
    const EffectId otherId = makeId("Other Vendor", "Other Effect",
                                    "D:/Audio/VST3/Other.vst3;"
                                    "ABCDEF019182FAEB536F6170536F6170");

    EXPECT_EQ(utils::findRelocatedVst3EffectId(oldId, { makeVst3Meta(otherId), makeVst3Meta(matchingId) }), matchingId);
}

TEST(EffectsBase_EffectsUtilsTests, FindRelocatedVst3EffectId_ReturnsEmptyWhenAmbiguous)
{
    const EffectId oldId = makeId("Soap Audio", "Soap Voice Cleaner",
                                  "C:/Program Files/Common Files/VST3/Soap Voice Cleaner.vst3;"
                                  "ABCDEF019182FAEB536F6170536F6170");
    const EffectId firstId = makeId("Soap Audio", "Soap Voice Cleaner",
                                    "D:/Audio/VST3/Soap Voice Cleaner.vst3;"
                                    "ABCDEF019182FAEB536F6170536F6170");
    const EffectId secondId = makeId("Soap Audio", "Soap Voice Cleaner",
                                     "E:/Audio/VST3/Soap Voice Cleaner.vst3;"
                                     "ABCDEF019182FAEB536F6170536F6170");

    EXPECT_TRUE(utils::findRelocatedVst3EffectId(oldId, { makeVst3Meta(firstId), makeVst3Meta(secondId) }).empty());
}

TEST(EffectsBase_EffectsUtilsTests, FindRelocatedVst3EffectId_KeepsExactLoadableMatch)
{
    const EffectId currentId = makeId("Soap Audio", "Soap Voice Cleaner",
                                      "C:/Program Files/Common Files/VST3/Soap Voice Cleaner.vst3;"
                                      "ABCDEF019182FAEB536F6170536F6170");
    const EffectId duplicateId = makeId("Soap Audio", "Soap Voice Cleaner",
                                        "D:/Audio/VST3/Soap Voice Cleaner.vst3;"
                                        "ABCDEF019182FAEB536F6170536F6170");

    EXPECT_TRUE(utils::findRelocatedVst3EffectId(currentId, { makeVst3Meta(currentId), makeVst3Meta(duplicateId) }).empty());
}

TEST(EffectsBase_EffectsUtilsTests, FindRelocatedVst3EffectId_IgnoresNonLoadableCandidates)
{
    const EffectId oldId = makeId("Soap Audio", "Soap Voice Cleaner",
                                  "C:/Program Files/Common Files/VST3/Soap Voice Cleaner.vst3;"
                                  "ABCDEF019182FAEB536F6170536F6170");
    EffectMeta candidate = makeVst3Meta(makeId("Soap Audio", "Soap Voice Cleaner",
                                               "D:/Audio/VST3/Soap Voice Cleaner.vst3;"
                                               "ABCDEF019182FAEB536F6170536F6170"));
    candidate.state = muse::audioplugins::AudioPluginState::Missing;

    EXPECT_TRUE(utils::findRelocatedVst3EffectId(oldId, { candidate }).empty());
}
