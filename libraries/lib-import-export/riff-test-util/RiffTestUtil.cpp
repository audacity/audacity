/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AcidizerTagUtilMain.cpp

  Matthieu Hodgkinson

  An command-line application to add the Acidizer tags to a WAV file, for the
  purpose of testing.

**********************************************************************/

#include "AcidizerTags.h"
#include "EditRiffTags.h"

#include "sndfile.h"
#include <fstream>
#include <iostream>
#include <optional>

int main(int argc, char** argv)
{
   using namespace LibFileFormats;

   if (argc < 3)
   {
      std::cout
         << "Usage: " << argv[0]
         << " <inputFile> <outputFile> [--bpm <bpm>] [--one-shot <zero for no>] "
            "[--DistributedBy <distributor>]"
         << std::endl;
      return 1;
   }

   const std::string inputFile = argv[1];
   const std::string outputFile = argv[2];

   std::optional<double> bpm;
   std::optional<bool> isOneShot;
   std::optional<std::string> distributor;

   for (int i = 3; i < argc; ++i)
   {
      const std::string arg = argv[i];
      if (arg == "--bpm")
      {
         if (i + 1 >= argc)
         {
            std::cout << "Missing argument for --bpm" << std::endl;
            return 1;
         }
         bpm = std::stod(argv[i + 1]);
         ++i;
      }
      else if (arg == "--one-shot")
      {
         if (i + 1 >= argc)
         {
            std::cout << "Missing argument for --one-shot" << std::endl;
            return 1;
         }
         isOneShot = std::stoi(argv[i + 1]) != 0;
         ++i;
      }
      else if (arg == "--DistributedBy")
      {
         if (i + 1 >= argc)
         {
            std::cout << "Missing argument for --DistributedBy" << std::endl;
            return 1;
         }
         distributor = argv[i + 1];
         ++i;
      }
      else
      {
         std::cout << "Unknown argument: " << arg << std::endl;
         return 1;
      }
   }

   // To begin with, just copy the input file and rename it to the output file
   // (without using filesystem or libsndfile):
   {
      std::ifstream in(inputFile, std::ios::binary);
      std::ofstream out(outputFile, std::ios::binary);
      out << in.rdbuf();
   }

   if (!bpm.has_value() && !isOneShot.has_value() && !distributor.has_value())
      return 0;

   // Open the output file for writing:
   SF_INFO sfInfo;
   SNDFILE* file = sf_open(outputFile.c_str(), SFM_RDWR, &sfInfo);
   if (file == nullptr)
   {
      std::cout << "Error opening " << outputFile << ": " << sf_strerror(file)
                << std::endl;
      return 1;
   }

   if (isOneShot.has_value() && *isOneShot)
      LibImportExport::AddAcidizerTags(AcidizerTags::OneShot {}, file);
   else if (bpm.has_value() && *bpm > 0)
      LibImportExport::AddAcidizerTags(AcidizerTags::Loop { *bpm }, file);

   if (distributor.has_value())
      LibImportExport::AddDistributorInfo(*distributor, file);

   sf_close(file);

   return 0;
}
