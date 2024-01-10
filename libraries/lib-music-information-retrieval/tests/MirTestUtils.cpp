#include "MirTestUtils.h"

#define USE_FILESYSTEM (__has_include(<filesystem>) && _WIN32)

#if USE_FILESYSTEM
#   include <filesystem>
#endif

#include <array>
#include <cmath>
#include <iomanip>
#include <iostream>

namespace MIR
{
std::vector<std::string> GetWavFilesUnderDir(const char* dir)
{
   // Only compile this if std::filesystem is available.
   std::vector<std::string> files;
#if USE_FILESYSTEM
   namespace fs = std::filesystem;
   for (const auto& entry : fs::directory_iterator(dir))
      // Ignore directories starting with underscore.
      if (entry.is_directory() && entry.path().filename().string()[0] != '_')
      {
         // Look for a file `audacity-maxNumFiles.txt` in this directory. If it
         // exists and contains a number, only process that many files.
         const auto maxNumFilesFilename =
            entry.path().string() + "/audacity-maxNumFiles.txt";
         std::ifstream maxNumFilesFile { maxNumFilesFilename };
         size_t maxNumFiles = std::numeric_limits<size_t>::max();
         if (maxNumFilesFile.good())
         {
            std::string line;
            std::getline(maxNumFilesFile, line);
            maxNumFiles = std::stoi(line);
         }
         // Collect up to `maxNumFiles` wav files in this directory.
         size_t numFiles = 0;
         for (const auto& subEntry : fs::recursive_directory_iterator(entry))
            if (
               subEntry.is_regular_file() &&
               subEntry.path().extension() == ".wav")
            {
               files.push_back(subEntry.path().string());
               if (++numFiles >= maxNumFiles)
                  break;
            }
      }
#endif
   return files;
}

void ProgressBar(int width, int percent)
{
   int progress = (width * percent) / 100;
   std::cout << "[";
   for (int i = 0; i < width; ++i)
      if (i < progress)
         std::cout << "=";
      else
         std::cout << " ";
   std::cout << "] " << std::setw(3) << percent << "%\r";
   std::cout.flush();
}

OctaveError GetOctaveError(double expected, double actual)
{
   constexpr std::array<double, 5> factors { 1., 2., .5, 3., 1. / 3 };
   std::vector<OctaveError> octaveErrors;
   std::transform(
      factors.begin(), factors.end(), std::back_inserter(octaveErrors),
      [&](double factor) {
         const auto remainder = std::log2(factor * actual / expected);
         return OctaveError { factor, remainder };
      });
   return *std::min_element(
      octaveErrors.begin(), octaveErrors.end(),
      [](const auto& a, const auto& b) {
         return std::abs(a.remainder) < std::abs(b.remainder);
      });
}
} // namespace MIR
