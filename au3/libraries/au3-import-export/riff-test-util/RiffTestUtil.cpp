/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AcidizerTagUtilMain.cpp

  Matthieu Hodgkinson

  An command-line application to add the Acidizer tags to a WAV file, for the
  purpose of testing.

**********************************************************************/

#include "AcidizerTags.h"
#include "LibsndfileTagger.h"

#include "sndfile.h"
#include <fstream>
#include <iostream>
#include <optional>

namespace {
void PrintHelp(const char* const* argv)
{
    std::cout
        << std::endl
        << "Test utility to tag files either the SoundForge or the MuseHub way."
        << std::endl;
    std::cout
        << "(SoundForge prioritizes number-of-beats over tempo, and the other way round for MuseHub.)"
        << std::endl
        << std::endl;
    std::cout
        << "Usage: " << argv[0]
        << " <outputFile> <duration (s)> [ MuseHub [ one-shot | <tempo (bpm)> ] | SoundForge [ one-shot | loop <num. beats> ] ]"
        << std::endl
        << std::endl;
    std::cout << "Examples:" << std::endl << std::endl;

    std::cout
        << "Create a 10-second file and add 60 BPM information (MuseHub style):"
        << std::endl;
    std::cout << "riff-test-util output.wav 10.0 MuseHub 60.0" << std::endl
              << std::endl;

    std::cout
        << "Create a 10-second file and specify 16 beats (SoundForge style):"
        << std::endl;
    std::cout << "riff-test-util output.wav 10.0 SoundForge 16.0" << std::endl
              << std::endl;

    std::cout << "Create a 3-second one-shot file:" << std::endl;
    std::cout << "riff-test-util output.wav 3.0 SoundForge one-shot" << std::endl
              << std::endl;
}
} // namespace

int main(int argc, char* const* argv)
{
    using namespace LibImportExport;

    if (argc < 5) {
        PrintHelp(argv);
        return 1;
    }

    const std::string inputFile = argv[1];
    auto duration = 0.;
    try
    {
        duration = std::stod(argv[2]);
    }
    catch (std::invalid_argument& e)
    {
        std::cout << "Invalid duration: " << argv[2] << std::endl;
        PrintHelp(argv);
        return 1;
    }

    Test::LibsndfileTagger tagger { duration, inputFile };
    if (!tagger) {
        std::cout << "Failed to open file: " << inputFile << std::endl;
        return 1;
    }

    enum class Mode
    {
        Unknown,
        MuseHub,
        SoundForge,
    };
    const Mode mode = std::string { "MuseHub" } == argv[3] ? Mode::MuseHub
    : std::string { "SoundForge" } == argv[3]
    ? Mode::SoundForge
    : Mode::Unknown;
    if (mode == Mode::Unknown) {
        std::cout << "Unknown mode: " << argv[3] << std::endl;
        PrintHelp(argv);
        return 1;
    }

    const auto isOneShot = argc >= 5 && std::string { "one-shot" } == argv[4];
    if (isOneShot) {
        tagger.AddAcidizerTags(Test::AcidizerTags::OneShot {});
    } else if (mode == Mode::MuseHub) {
        try
        {
            const auto bpm = std::stod(argv[4]);
            tagger.AddAcidizerTags(Test::AcidizerTags::Loop { bpm });
            tagger.AddDistributorInfo("Muse Hub");
        }
        catch (std::invalid_argument& e)
        {
            std::cout << "Invalid BPM: " << argv[4] << std::endl;
            PrintHelp(argv);
            return 1;
        }
    } else {
        try
        {
            const auto numBeats = std::stoi(argv[4]);
            tagger.AddAcidizerTags(Test::AcidizerTags::Beats { numBeats });
        }
        catch (std::invalid_argument& e)
        {
            std::cout << "Invalid number of beats: " << argv[5] << std::endl;
            PrintHelp(argv);
            return 1;
        }
    }

    return 0;
}
