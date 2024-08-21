#include "EditUtils.h"
#include "AudioIO.h"
#include "ProjectAudioManager.h"
#include <thread>

void EditUtils::StopAndResumePlayback(AudacityProject& project)
{
   auto& pam = ProjectAudioManager::Get(project);
   if (!pam.Playing())
      return;
   constexpr auto stopStream = true;
   pam.Stop(stopStream);
   const auto io = AudioIO::Get();
   while (io->IsBusy())
      std::this_thread::sleep_for(std::chrono::milliseconds { 10 });
   pam.ResumePlayback();
}

[[nodiscard]] Finally<std::function<void()>>
EditUtils::StopPlaybackWhileEditing(AudacityProject& project)
{
   auto pam = &ProjectAudioManager::Get(project);
   const auto wasPlaying = pam->Playing();
   if (wasPlaying)
   {
      constexpr auto stopStream = true;
      pam->Stop(stopStream);
      const auto io = AudioIO::Get();
      while (io->IsBusy())
         std::this_thread::sleep_for(std::chrono::milliseconds { 10 });
   }
   return Finally<std::function<void()>> { [=] {
      if (wasPlaying)
         pam->ResumePlayback();
   } };
}
