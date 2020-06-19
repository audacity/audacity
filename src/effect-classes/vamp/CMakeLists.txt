if(USE_VAMP)

do_directory(
   INCLUDE_PRIVATE
      src/effects
      src/etc
      src/track
      src/xml

   PRIVATE
      src/widgets
      wxWidgets
      $<$<BOOL:${USE_VAMP}>:libvamp>

   PRECOMPILE
      AColor.h
      AudioIO.h
      Diags.h
      DirManager.h
      Envelope.h
      FFT.h
      ImageManipulation.h
      LabelTrack.h
      Mix.h
      NoteTrack.h
      Prefs.h
      Project.h
      TimeTrack.h
      WaveTrack.h
      ASlider.h
      ProgressDialog.h
      Ruler.h

   SOURCES
      LoadVamp.cpp
      LoadVamp.h
      VampEffect.cpp
      VampEffect.h
)

endif()
