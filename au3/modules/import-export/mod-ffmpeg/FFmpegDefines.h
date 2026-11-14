/**********************************************************************

   Audacity: A Digital Audio Editor

   FFmpegTypes.h

   Audacity(R) is copyright (c) 1999-2010 Audacity Team.
   License: GPL v2 or later.  See License.txt.

   LRN

   Vitaly Sverchinsky split from ExportFFmpegDialogs.cpp

**********************************************************************/

#pragma once

#include <wx/string.h>

#define AV_CANMETA (AV_VERSION_INT(255, 255, 255))

/// This construction defines a enumeration of UI element IDs, and a static
/// array of their string representations (this way they're always synchronized).
/// Do not store the enumerated values in external files, as they may change;
/// the strings may be stored.
#define FFMPEG_EXPORT_CTRL_ID_ENTRIES \
    FFMPEG_EXPORT_CTRL_ID_FIRST_ENTRY(FEFirstID, 20000), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEFormatID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FECodecID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEBitrateID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEQualityID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FESampleRateID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FELanguageID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FETagID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FECutoffID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEFrameSizeID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEBufSizeID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEProfileID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FECompLevelID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEUseLPCID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FELPCCoeffsID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEMinPredID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEMaxPredID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEPredOrderID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEMinPartOrderID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEMaxPartOrderID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEMuxRateID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEPacketSizeID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEBitReservoirID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEVariableBlockLenID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FELastID), \
 \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEFormatLabelID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FECodecLabelID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEFormatNameID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FECodecNameID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEPresetID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FESavePresetID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FELoadPresetID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEDeletePresetID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEAllFormatsID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEAllCodecsID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEImportPresetsID), \
    FFMPEG_EXPORT_CTRL_ID_ENTRY(FEExportPresetsID) \

// First the enumeration
#define FFMPEG_EXPORT_CTRL_ID_FIRST_ENTRY(name, num)  name = num
#define FFMPEG_EXPORT_CTRL_ID_ENTRY(name)             name

enum FFmpegExportCtrlID {
    FFMPEG_EXPORT_CTRL_ID_ENTRIES
};

// Now the string representations
#undef FFMPEG_EXPORT_CTRL_ID_FIRST_ENTRY
#define FFMPEG_EXPORT_CTRL_ID_FIRST_ENTRY(name, num)  wxT(#name)
#undef FFMPEG_EXPORT_CTRL_ID_ENTRY
#define FFMPEG_EXPORT_CTRL_ID_ENTRY(name)             wxT(#name)
static const wxChar* FFmpegExportCtrlIDNames[] = {
    FFMPEG_EXPORT_CTRL_ID_ENTRIES
};

#undef FFMPEG_EXPORT_CTRL_ID_ENTRIES
#undef FFMPEG_EXPORT_CTRL_ID_ENTRY
#undef FFMPEG_EXPORT_CTRL_ID_FIRST_ENTRY
