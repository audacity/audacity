/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  TempoChange.h

  PaulLicameli

**********************************************************************/
#include "au3-registries/AttachedVirtualFunction.h"
#include <optional>

class ChannelGroup;

//! Method to set project tempo on a channel group, defaulting to no-op
using OnProjectTempoChange
    =AttachedVirtualFunction<
          struct OnProjectTempoChangeTag,
          void, ChannelGroup,
          const std::optional<double>& /* oldTempo */, double /* newTempo */
          >;
DECLARE_EXPORTED_ATTACHED_VIRTUAL(, OnProjectTempoChange);

void DoProjectTempoChange(ChannelGroup& group, double newTempo);

const std::optional<double>& GetProjectTempo(const ChannelGroup& group);
