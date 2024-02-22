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

#include "concurrency/ICancellable.h"

class TrackList;

namespace cloud::audiocom::sync
{
enum class UploadMode
{
   Normal,
   CreateNew,
   ForceOverwrite,
};

struct ProjectUploadData final
{
   std::vector<uint8_t> ProjectSnapshot;
   std::shared_ptr<TrackList> Tracks;
};

class ProjectUploadOperation /* not final */ :
    public audacity::concurrency::ICancellable
{
public:
   virtual ~ProjectUploadOperation() = default;

   virtual void Start(UploadMode mode)                       = 0;
   virtual void SetUploadData(const ProjectUploadData& data) = 0;
   virtual bool IsCompleted() const                          = 0;
}; // class AsynchronousOperation
} // namespace cloud::audiocom::sync
