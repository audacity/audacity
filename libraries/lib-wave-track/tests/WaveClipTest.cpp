#include "WaveClip.h"
#include "MockedAudio.h"
#include "MockedPrefs.h"
#include "Project.h"
#include "ProjectFileIO.h"
#include "SampleBlock.h"
#include "WaveTrack.h"

#include <catch2/catch_all.hpp>

namespace
{
std::optional<MockedAudio> instantiatedToInitializePortAudio;
std::optional<MockedPrefs>
   mockedPrefs; // todo(mhodgkinson) How about this one ?
std::shared_ptr<AudacityProject> project;
} // namespace

class testRunListener : public Catch::EventListenerBase
{
public:
   using Catch::EventListenerBase::EventListenerBase;

   void testRunStarting(Catch::TestRunInfo const&) override
   {
      instantiatedToInitializePortAudio.emplace();
      mockedPrefs.emplace();
      project = AudacityProject::Create();
      ProjectFileIO::InitializeSQL();
      ProjectFileIO::Get(*project).OpenProject();
   }

   void testRunEnded(Catch::TestRunStats const& testRunStats) override
   {
      ProjectFileIO::Get(*project).CloseProject();
   }
};

CATCH_REGISTER_LISTENER(testRunListener)

TEST_CASE("WaveClip")
{
   constexpr auto stride = 1; // Still don't know what that is.
   constexpr auto effectiveFormat = sampleFormat::int16Sample;
   constexpr auto rawClipPlayDuration = 2.0;
   constexpr auto sampleRate = 8000;
   constexpr auto colorIndex = 0;

   const auto clip = std::make_shared<WaveClip>(
      SampleBlockFactory::New(*project), sampleFormat::floatSample, sampleRate,
      colorIndex);
   std::vector<float> silence(sampleRate * rawClipPlayDuration);
   clip->Append(
      reinterpret_cast<char*>(silence.data()), sampleFormat::floatSample,
      silence.size(), 1, effectiveFormat);

   SECTION("GetSequenceStartTime")
   {
      SECTION("Trimming then stretching")
      {
         clip->Offset(10.0);
         clip->SetTrimLeft(
            rawClipPlayDuration /
            2); // start time now 10 + rawClipPlayDuration / 2
         REQUIRE(clip->GetPlayStartTime() == 10.0 + rawClipPlayDuration / 2);
         clip->SetClipStretchRatioFromRight(
            0.5); // squeeze the remainder from the right ; start time stays the
                  // same.
         REQUIRE(clip->GetPlayStartTime() == 10.0 + rawClipPlayDuration / 2);
      }
   }

   SECTION("GetPlayDuration")
   {
      SECTION("without stretching")
      {
         REQUIRE(clip->GetPlayDuration() == rawClipPlayDuration);
      }
      SECTION("with stretching")
      {
         clip->SetClipStretchRatioFromRight(0.1);
         REQUIRE(clip->GetPlayDuration() == rawClipPlayDuration * 0.1);
         clip->SetClipStretchRatioFromRight(10.);
         REQUIRE(clip->GetPlayDuration() == rawClipPlayDuration * 10.);
      }
      SECTION("with offset")
      {
         clip->Offset(123.0);
         REQUIRE(clip->GetPlayDuration() == rawClipPlayDuration);
      }
      SECTION("with offset and stretching")
      {
         clip->Offset(123.0);
         clip->SetClipStretchRatioFromRight(10.0);
         REQUIRE(clip->GetPlayDuration() == rawClipPlayDuration * 10.0);
      }
      SECTION("with trimming")
      {
         clip->SetTrimLeft(rawClipPlayDuration / 2);
         REQUIRE(clip->GetPlayDuration() == rawClipPlayDuration / 2);
      }
      SECTION("with stretching then trimming")
      {
         clip->SetClipStretchRatioFromRight(0.5); // halves play duration ...
         clip->SetTrimLeft(
            rawClipPlayDuration / 4); // halves remaining play duration ...
         REQUIRE(clip->GetPlayDuration() == rawClipPlayDuration / 4);
      }
      SECTION("with trimming then stretching")
      {
         clip->SetTrimLeft(rawClipPlayDuration / 2); // halves play duration ...
         clip->SetClipStretchRatioFromRight(
            0.5); // halves remaining play duration ...
         REQUIRE(clip->GetPlayDuration() == rawClipPlayDuration / 4);
      }
      // todo(mhodgkinson) continue when focus is on trimming.
   }
}
