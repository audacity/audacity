if(USE_LADSPA)

do_directory(
   INCLUDE_PRIVATE
      src/etc

   PRIVATE
      src/widgets
      wxWidgets

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
      LadspaEffect.cpp
      LadspaEffect.h
      ladspa.h
)

endif()
