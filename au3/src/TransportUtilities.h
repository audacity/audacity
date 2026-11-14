/**********************************************************************

  Audacity: A Digital Audio Editor

  @file TransportUtilities.h
  @brief Some UI related to starting and stopping play and record

  Paul Licameli split from TransportMenus.h

**********************************************************************/

#ifndef __AUDACITY_TRANSPORT_UTILITIES__
#define __AUDACITY_TRANSPORT_UTILITIES__

struct AudioIOStartStreamOptions;
class CommandContext;
class SelectedRegion;
class TrackList;
class TransportSequences;
enum class PlayMode : int;

struct AUDACITY_DLL_API TransportUtilities
{
    static void PlayCurrentRegionAndWait(
        const CommandContext& context, bool newDefault = false, bool cutpreview = false);
    static void PlayPlayRegionAndWait(
        const CommandContext& context, const SelectedRegion& selectedRegion, const AudioIOStartStreamOptions& options, PlayMode mode);
    static void RecordAndWait(
        const CommandContext& context, bool altAppearance);

    static void DoStartPlaying(
        const CommandContext& context, bool newDefault = false);
    static bool DoStopPlaying(const CommandContext& context);
};

/*!
 @param nonWaveToo if true, collect all PlayableTracks
 */
TransportSequences MakeTransportTracks(
    TrackList& trackList, bool selectedOnly, bool nonWaveToo = false);

#endif
