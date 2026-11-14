/*  SPDX-License-Identifier: GPL-2.0-or-later */
/*!********************************************************************

  Audacity: A Digital Audio Editor

  FrameStatisticsDialog.h

  Dmitry Vedenko

**********************************************************************/
#pragma once

//! A dialog that displays information about time needed to paint the TrackPanel
class FrameStatisticsDialog final
{
public:
    //! Shows the dialog
    static void Show(bool show);
    //! Destroys the dialog to prevent Audacity from hanging on exit
    static void Destroy();
};
