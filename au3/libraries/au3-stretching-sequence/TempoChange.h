/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TempoChange.h

  PaulLicameli

**********************************************************************/
#include "AttachedVirtualFunction.h"
#include <optional>

class ChannelGroup;

//! Method to set project tempo on a channel group, defaulting to no-op
using OnProjectTempoChange
    =AttachedVirtualFunction<
          struct OnProjectTempoChangeTag,
          void, ChannelGroup,
          const std::optional<double>& /* oldTempo */, double /* newTempo */
          >;
DECLARE_EXPORTED_ATTACHED_VIRTUAL(
    STRETCHING_SEQUENCE_API, OnProjectTempoChange);

STRETCHING_SEQUENCE_API
void DoProjectTempoChange(ChannelGroup& group, double newTempo);

STRETCHING_SEQUENCE_API
const std::optional<double>& GetProjectTempo(const ChannelGroup& group);
