/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  AsynchronousOperation.h

  Dmitry Vedenko

**********************************************************************/

#pragma once

#include <cstdint>
#include <memory>
#include <vector>

class TrackList;

namespace cloud::audiocom::sync
{
struct ProjectUploadData final
{
   std::vector<uint8_t> ProjectSnapshot;
   std::shared_ptr<TrackList> Tracks;
};

class ProjectUploadOperation /* not final */
{
public:
   virtual ~ProjectUploadOperation() = default;

   virtual void Start(const ProjectUploadData& data) = 0;
   virtual void Cancel() = 0;
   virtual bool IsCompleted() const = 0;
}; // class AsynchronousOperation
} // namespace cloud::audiocom::sync
