/**********************************************************************

  Audacity: A Digital Audio Editor

  ExportUtils.h
 
  Dominic Mazzoni

  Vitaly Sverchinsky split from ExportPlugin.h

**********************************************************************/

#pragma once

#include <memory>

#include "SampleFormat.h"

class TrackList;
class Mixer;
class TranslatableString;
class wxFileNameWrapper;

namespace BasicUI
{
class ProgressDialog;
}

namespace MixerOptions
{
class Downmix;
}

class ExportUtils final
{
public:

   static std::unique_ptr<Mixer> CreateMixer(const TrackList &tracks,
         bool selectionOnly,
         double startTime, double stopTime,
         unsigned numOutChannels, size_t outBufferSize, bool outInterleaved,
         double outRate, sampleFormat outFormat,
         MixerOptions::Downmix *mixerSpec);

   // Create or recycle a dialog.
   static void InitProgress(std::unique_ptr<BasicUI::ProgressDialog> &pDialog,
         const TranslatableString &title, const TranslatableString &message);
   static void InitProgress(std::unique_ptr<BasicUI::ProgressDialog> &pDialog,
         const wxFileNameWrapper &title, const TranslatableString &message);

};

