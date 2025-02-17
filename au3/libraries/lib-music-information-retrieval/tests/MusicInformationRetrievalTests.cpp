#include "MirFakes.h"
#include "MirProjectInterface.h"
#include "MusicInformationRetrieval.h"
#include "WavMirAudioReader.h"

#include <catch2/catch.hpp>

namespace MIR {
TEST_CASE("GetBpmFromFilename")
{
    const std::vector<std::pair<std::string, std::optional<double> > > testCases {
        { "120 BPM", 120 },

        // there may be an extension
        { "120 BPM.opus", 120 },
        { "120 BPM", 120 },

        // it may be preceeded by a path
        { "C:/my\\path/to\\120 BPM", 120 },

        // value must be between 30 and 300 inclusive
        { "1 BPM", std::nullopt },
        { "29 BPM", std::nullopt },
        { "30 BPM", 30 },
        { "300 BPM", 300 },
        { "301 BPM", std::nullopt },
        { "1000 BPM", std::nullopt },

        // it may be preceeded by zeros
        { "000120 BPM", 120 },

        // there may be something before the value
        { "anything 120 BPM", 120 },
        // but then there must be a separator
        { "anything120 BPM", std::nullopt },
        // there may be something after the value
        { "120 BPM anything", 120 },
        // but then there must also be a separator
        { "120 BPManything", std::nullopt },

        // what separator is used doesn't matter
        { "anything-120-BPM", 120 },
        { "anything_120_BPM", 120 },
        { "anything.120.BPM", 120 },

        // but of course that can't be an illegal filename character
        { "120/BPM", std::nullopt },
        { "120\\BPM", std::nullopt },
        { "120:BPM", std::nullopt },
        { "120;BPM", std::nullopt },
        { "120'BPM", std::nullopt },
        // ... and so on.

        // separators before and after don't have to match
        { "anything_120-BPM", 120 },

        // no separator between value and "bpm" is ok
        { "anything.120BPM", 120 },

        // a few real file names found out there
        { "Cymatics - Cyclone Top Drum Loop 3 - 174 BPM", 174 },
        { "Fantasie Impromptu Op. 66.mp3", std::nullopt },
    };
    std::vector<bool> success(testCases.size());
    std::transform(
        testCases.begin(), testCases.end(), success.begin(),
        [](const auto& testCase) {
        return GetBpmFromFilename(testCase.first) == testCase.second;
    });
    REQUIRE(
        std::all_of(success.begin(), success.end(), [](bool b) { return b; }));
}

namespace {
using namespace LibFileFormats;
constexpr auto filename100bpm = "my/path\\foo_-_100BPM_Sticks_-_foo.wav";
const EmptyMirAudioReader emptyReader;
const ProjectSyncInfoInput arbitaryInput { emptyReader };
} // namespace

TEST_CASE("GetProjectSyncInfo")
{
    SECTION("operator bool")
    {
        SECTION("returns false if ACID tag says one-shot")
        {
            auto input = arbitaryInput;
            input.tags.emplace(AcidizerTags::OneShot {});
            REQUIRE(!GetProjectSyncInfo(input).has_value());
        }

        SECTION("returns true if ACID tag says non-one-shot")
        {
            auto input = arbitaryInput;
            input.tags.emplace(AcidizerTags::Loop { 120.0 });
            REQUIRE(GetProjectSyncInfo(input).has_value());
        }

        SECTION("BPM is invalid")
        {
            SECTION("returns true if filename has BPM")
            {
                auto input = arbitaryInput;
                input.filename = filename100bpm;
                REQUIRE(GetProjectSyncInfo(input).has_value());
            }

            SECTION("returns false if filename has no BPM")
            {
                auto input = arbitaryInput;
                input.filename = "filenameWithoutBpm";
                REQUIRE(!GetProjectSyncInfo(input).has_value());
            }
        }
    }

    SECTION("GetProjectSyncInfo")
    {
        SECTION("prioritizes ACID tags over filename")
        {
            auto input = arbitaryInput;
            input.filename = filename100bpm;
            input.tags.emplace(AcidizerTags::Loop { 120. });
            const auto info = GetProjectSyncInfo(input);
            REQUIRE(info);
            REQUIRE(info->rawAudioTempo == 120);
        }

        SECTION("falls back on filename if tag bpm is invalid")
        {
            auto input = arbitaryInput;
            input.filename = filename100bpm;
            input.tags.emplace(AcidizerTags::Loop { -1. });
            const auto info = GetProjectSyncInfo(input);
            REQUIRE(info);
            REQUIRE(info->rawAudioTempo == 100);
        }

        SECTION("stretchMinimizingPowOfTwo is as expected")
        {
            auto input = arbitaryInput;
            input.filename = filename100bpm;

            input.projectTempo = 100.;
            REQUIRE(GetProjectSyncInfo(input)->stretchMinimizingPowOfTwo == 1.);

            // Project tempo twice as fast. Without compensation, the audio would
            // be stretched to 0.5 its length. Not stretching it at all may still
            // yield musically interesting results.
            input.projectTempo = 200;
            REQUIRE(GetProjectSyncInfo(input)->stretchMinimizingPowOfTwo == 2.);

            // Same principle applies in the following:
            input.projectTempo = 400;
            REQUIRE(GetProjectSyncInfo(input)->stretchMinimizingPowOfTwo == 4.);
            input.projectTempo = 50;
            REQUIRE(GetProjectSyncInfo(input)->stretchMinimizingPowOfTwo == .5);
            input.projectTempo = 25;
            REQUIRE(GetProjectSyncInfo(input)->stretchMinimizingPowOfTwo == .25);

            // Now testing edge cases:
            input.projectTempo = 100 * std::pow(2, .51);
            REQUIRE(GetProjectSyncInfo(input)->stretchMinimizingPowOfTwo == 2.);
            input.projectTempo = 100 * std::pow(2, .49);
            REQUIRE(GetProjectSyncInfo(input)->stretchMinimizingPowOfTwo == 1.);
            input.projectTempo = 100 * std::pow(2, -.49);
            REQUIRE(GetProjectSyncInfo(input)->stretchMinimizingPowOfTwo == 1.);
            input.projectTempo = 100 * std::pow(2, -.51);
            REQUIRE(GetProjectSyncInfo(input)->stretchMinimizingPowOfTwo == .5);
        }
    }
}

TEST_CASE("SynchronizeProject")
{
    constexpr auto initialProjectTempo = 100.;
    FakeProjectInterface project { initialProjectTempo };

    SECTION("single-file import")
    {
        constexpr FakeAnalyzedAudioClip::Params clipParams {
            123., TempoObtainedFrom::Title
        };

        // Generate all possible situations, and in the sections filter for the
        // conditions we want to check.
        project.isBeatsAndMeasures = GENERATE(false, true);
        project.shouldBeReconfigured = GENERATE(false, true);
        const auto projectWasEmpty = GENERATE(false, true);
        const auto clipsHaveTempo = GENERATE(false, true);

        const std::vector<std::shared_ptr<AnalyzedAudioClip> > clips {
            std::make_shared<FakeAnalyzedAudioClip>(
                clipsHaveTempo ? std::make_optional(clipParams) : std::nullopt)
        };

        const auto projectWasReconfigured = [&](bool yes) {
            const auto reconfigurationCheck = yes == project.wasReconfigured;
            const auto projectTempoCheck
                =project.projectTempo
                  == (yes ? clipParams.tempo : initialProjectTempo);
            REQUIRE(reconfigurationCheck);
            REQUIRE(projectTempoCheck);
        };

        const auto clipsWereSynchronized = [&](bool yes) {
            const auto check = yes == project.clipsWereSynchronized;
            REQUIRE(check);
        };

        SECTION("nothing happens if")
        {
            SECTION("no clip has tempo")
            if (!clipsHaveTempo) {
                SynchronizeProject(clips, project, projectWasEmpty);
                projectWasReconfigured(false);
                clipsWereSynchronized(false);
            }
            SECTION(
                "user doesn't want reconfiguration and view is minutes and seconds")
            if (!project.shouldBeReconfigured && !project.isBeatsAndMeasures) {
                SynchronizeProject(clips, project, projectWasEmpty);
                projectWasReconfigured(false);
                clipsWereSynchronized(false);
            }
            SECTION(
                "user wants reconfiguration but view is minutes and seconds and project is not empty")
            if (
                project.shouldBeReconfigured && !project.isBeatsAndMeasures
                && !projectWasEmpty) {
                SynchronizeProject(clips, project, projectWasEmpty);
                projectWasReconfigured(false);
                clipsWereSynchronized(false);
            }
        }

        SECTION(
            "project gets reconfigured only if clips have tempo, user wants to and project is empty")
        {
            SynchronizeProject(clips, project, projectWasEmpty);
            projectWasReconfigured(
                clipsHaveTempo && project.shouldBeReconfigured && projectWasEmpty);
        }

        SECTION("project does not get reconfigured if")
        {
            SECTION("user doesn't want to")
            if (!project.shouldBeReconfigured) {
                SynchronizeProject(clips, project, projectWasEmpty);
                projectWasReconfigured(false);
            }

            SECTION("project was not empty")
            if (!projectWasEmpty) {
                SynchronizeProject(clips, project, projectWasEmpty);
                projectWasReconfigured(false);
            }
        }

        SECTION("clips don't get synchronized if view is minutes and seconds and")
        if (!project.isBeatsAndMeasures) {
            SECTION("user says no to reconfiguration")
            if (!project.shouldBeReconfigured) {
                SynchronizeProject(clips, project, projectWasEmpty);
                clipsWereSynchronized(false);
            }
            SECTION("project was not empty")
            if (!projectWasEmpty) {
                SynchronizeProject(clips, project, projectWasEmpty);
                clipsWereSynchronized(false);
            }
        }

        SECTION("clips get synchronized if some clip has tempo and")
        if (clipsHaveTempo) {
            SECTION(
                "user doesn't want reconfiguration but view is beats and measures")
            if (!project.shouldBeReconfigured && project.isBeatsAndMeasures) {
                SynchronizeProject(clips, project, projectWasEmpty);
                clipsWereSynchronized(true);
            }
            SECTION(
                "user wants reconfiguration, view is beats and measures and project is not empty")
            if (
                project.shouldBeReconfigured && project.isBeatsAndMeasures
                && !projectWasEmpty) {
                SynchronizeProject(clips, project, projectWasEmpty);
                clipsWereSynchronized(true);
            }
        }
    }

    SECTION("multiple-file import")
    {
        project.shouldBeReconfigured = true;
        constexpr auto projectWasEmpty = true;

        SECTION(
            "for clips of different tempi, precedence is header-based, then title-based, then signal-based")
        {
            SynchronizeProject(
                {
                    std::make_shared<FakeAnalyzedAudioClip>(
                        FakeAnalyzedAudioClip::Params { 123.,
                                                        TempoObtainedFrom::Title }),
                    std::make_shared<FakeAnalyzedAudioClip>(
                        FakeAnalyzedAudioClip::Params { 456.,
                                                        TempoObtainedFrom::Header }),
                    std::make_shared<FakeAnalyzedAudioClip>(
                        FakeAnalyzedAudioClip::Params { 789.,
                                                        TempoObtainedFrom::Signal }),
                },
                project, projectWasEmpty);
            REQUIRE(project.projectTempo == 456.);

            SynchronizeProject(
                {
                    std::make_shared<FakeAnalyzedAudioClip>(
                        FakeAnalyzedAudioClip::Params { 789.,
                                                        TempoObtainedFrom::Signal }),
                    std::make_shared<FakeAnalyzedAudioClip>(
                        FakeAnalyzedAudioClip::Params { 123.,
                                                        TempoObtainedFrom::Title }),
                },
                project, projectWasEmpty);
            REQUIRE(project.projectTempo == 123.);

            SynchronizeProject(
                {
                    std::make_shared<FakeAnalyzedAudioClip>(
                        FakeAnalyzedAudioClip::Params { 789.,
                                                        TempoObtainedFrom::Signal }),
                },
                project, projectWasEmpty);
            REQUIRE(project.projectTempo == 789.);
        }

        SECTION("raw audio tempo of one-shot clips is set to project tempo")
        {
            const auto oneShotClip
                =std::make_shared<FakeAnalyzedAudioClip>(std::nullopt);
            constexpr auto whicheverMethod = TempoObtainedFrom::Signal;
            SynchronizeProject(
                {
                    std::make_shared<FakeAnalyzedAudioClip>(
                        FakeAnalyzedAudioClip::Params { 123., whicheverMethod }),
                    oneShotClip,
                },
                project, projectWasEmpty);
            REQUIRE(project.projectTempo == 123);
            REQUIRE(oneShotClip->rawAudioTempo == 123);
        }
    }
}
} // namespace MIR
