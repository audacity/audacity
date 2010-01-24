/******************************************/
/*
  info.cpp
  by Gary P. Scavone, 2001

  Prints audio system/device info.
*/
/******************************************/

#include "RtAudio.h"
#include <iostream>

int main(int argc, char *argv[])
{
  RtAudio *audio;
  RtAudioDeviceInfo info;
  try {
    audio = new RtAudio();
  }
  catch (RtError &error) {
    error.printMessage();
    exit(EXIT_FAILURE);
  }

  int devices = audio->getDeviceCount();
  std::cout << "\nFound " << devices << " device(s) ...\n";

  for (int i=1; i<=devices; i++) {
    try {
      info = audio->getDeviceInfo(i);
    }
    catch (RtError &error) {
      error.printMessage();
      break;
    }

    std::cout << "\nDevice Name = " << info.name << '\n';
    if (info.probed == false)
      std::cout << "Probe Status = UNsuccessful\n";
    else {
      std::cout << "Probe Status = Successful\n";
      std::cout << "Output Channels = " << info.outputChannels << '\n';
      std::cout << "Input Channels = " << info.inputChannels << '\n';
      std::cout << "Duplex Channels = " << info.duplexChannels << '\n';
      if (info.isDefault) std::cout << "This is the default device.\n";
      else std::cout << "This is NOT the default device.\n";
      if ( info.nativeFormats == 0 )
        std::cout << "No natively supported data formats(?)!";
      else {
        std::cout << "Natively supported data formats:\n";
        if ( info.nativeFormats & RTAUDIO_SINT8 )
          std::cout << "  8-bit int\n";
        if ( info.nativeFormats & RTAUDIO_SINT16 )
          std::cout << "  16-bit int\n";
        if ( info.nativeFormats & RTAUDIO_SINT24 )
          std::cout << "  24-bit int\n";
        if ( info.nativeFormats & RTAUDIO_SINT32 )
          std::cout << "  32-bit int\n";
        if ( info.nativeFormats & RTAUDIO_FLOAT32 )
          std::cout << "  32-bit float\n";
        if ( info.nativeFormats & RTAUDIO_FLOAT64 )
          std::cout << "  64-bit float\n";
      }
      if ( info.sampleRates.size() < 1 )
        std::cout << "No supported sample rates found!";
      else {
        std::cout << "Supported sample rates = ";
        for (unsigned int j=0; j<info.sampleRates.size(); j++)
          std::cout << info.sampleRates[j] << " ";
      }
      std::cout << std::endl;
    }
  }
  std::cout << std::endl;

  delete audio;
  return 0;
}
