if(USE_AUDIO_UNITS AND wxIS_MAC)

do_directory(
   INCLUDE_PRIVATE
      src/etc

   PRIVATE
      src/widgets
      wxWidgets

   OBJCPP_SOURCES
      AUControl.mm

   PRECOMPILE
      AColor.h
      AudioIO.h
      Diags.h
      FFT.h
      ImageManipulation.h
      Mix.h
      Prefs.h
      Project.h
      ASlider.h
      ProgressDialog.h
      Ruler.h

   SOURCES
      AUControl.h
      AudioUnitEffect.cpp
      AudioUnitEffect.h
)

endif()
