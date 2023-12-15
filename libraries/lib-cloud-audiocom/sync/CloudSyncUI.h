/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  CloudSyncUI.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

#include <string>

class AudacityProject;

namespace BasicUI
{
class WindowPlacement;
} // namespace BasicUI

namespace cloud::audiocom::sync
{
enum class CloudProjectVisibility
{
   Private,
   Unlisted,
   Public,
};

struct SaveResult
{
   std::string Title;
   int PreviewSaveFrequency { 0 };
   CloudProjectVisibility Visiblity {
      CloudProjectVisibility::Private
   };
   bool SaveToCloud { false };
   bool Cancelled { false };
};

class CLOUD_AUDIOCOM_API CloudSyncUI /* not final */
{
public:
   virtual ~CloudSyncUI();

   virtual SaveResult OnHandleFirstSave(
      const AudacityProject& project,
      const BasicUI::WindowPlacement& placement) = 0;

   virtual SaveResult OnHandleSave(
      const AudacityProject& project,
      const BasicUI::WindowPlacement& placement) = 0;

   virtual bool OnUnauthorizedSave(const BasicUI::WindowPlacement& placement) = 0;
};
} // namespace cloud::audiocom::sync
