#[[
Abstract ChannelGroup, ChannelGroupInterval, Channel, and ChannelInterval

A ChannelGroup supports attachment of other information as a ClientData::Site.

Besides that, it exposes a grid decomposition along two discrete "axes."  It
allows iteration over its constituent Channels, and the other way, iteration
over ChannelGroupIntervals, which each occupy a (possibly zero-length) finite
interval on a timeline, denominated in seconds.

ChannelInterval represents the intersection of information associated with one
Channel and one ChannelGroupInterval, each of which exposes a way to iterate
its ChannelIntervals.

Thus one can implement nested loops taking either "axis" for the outer loop.

The intention is to organize storage so that it maps more directly onto the
interval-major iteration, but sometimes the other iteration is more convenient
to the task.
]]

set( SOURCES
   Channel.cpp
   Channel.h
)
set( LIBRARIES
   lib-registries-interface
)
audacity_library( lib-channel "${SOURCES}" "${LIBRARIES}"
   "" ""
)
