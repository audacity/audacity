/**********************************************************************

  Audacity: A Digital Audio Editor

  LoadLV2.h

  Audacity(R) is copyright (c) 1999-2008 Audacity Team.
  License: GPL v2.  See License.txt.

*********************************************************************/

#include <lilv/lilv.h>

extern LilvWorld *gWorld;

// This is the LV2 Feature array. It is passed to every LV2 plugin on
// instantiation. So far it only contains the URI Map Feature, which is
// needed to load synths.
extern LV2_Feature * const gLV2Features[];

// These are needed for comparisons
extern LilvNode *gAudioPortClass;
extern LilvNode *gControlPortClass;
extern LilvNode *gMidiPortClass;
extern LilvNode *gInputPortClass;
extern LilvNode *gOutputPortClass;
extern LilvNode *gPortToggled;
extern LilvNode *gPortIsInteger;
extern LilvNode *gPortIsSampleRate;
extern LilvNode *gPortIsEnumeration;
extern LilvNode *gPortIsLatency;
extern LilvNode *gPortIsOptional;
extern LilvNode *gName;
extern LilvNode *gPortGroup;
extern LilvNode *gSubGroupOf;


void LoadLV2Plugins();
void UnloadLV2Plugins();
