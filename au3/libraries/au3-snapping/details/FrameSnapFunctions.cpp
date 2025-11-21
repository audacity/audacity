/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FrameSnapFunctions.cpp

  Dmitry Vedenko

**********************************************************************/

#include "SnapUtils.h"

namespace {
SnapRegistryItemRegistrator videoFrames {
    SnapFunctionItems("frames",
                      SnapFunctionGroup(
                          "video", { XO("Video frames"), false },
                          TimeInvariantSnapFunction(
                              "film_24_fps", XO("Film frames (24 fps)"), 24.0),
                          TimeInvariantSnapFunction(
                              "ntsc_29.97_fps", XO("NTSC frames (29.97 fps)"), 30.0 / 1.001),
                          TimeInvariantSnapFunction(
                              "ntsc_30_fps", XO("NTSC frames (30 fps)"), 30.0 / 1.001),
                          TimeInvariantSnapFunction("film_25_fps", XO("PAL frames (25 fps)"), 25.0)),
                      SnapFunctionGroup(
                          "cd", { XO("CD frames"), false },
                          TimeInvariantSnapFunction("cd_75_fps", XO("CDDA frames (75 fps)"), 75.0))
                      ),
    Registry::Placement { {}, { Registry::OrderingHint::After, "time" } }
};
}
