if(USE_NYQUIST)

do_directory(
   INCLUDE_PRIVATE
      src/commands
      src/etc
      src/effects
      src/ondemand
      src/prefs
      src/track
      src/wavetrack
      src/xml

   PRIVATE
      src/widgets
      wxWidgets
      $<$<BOOL:${USE_NYQUIST}>:libnyquist>

   PRECOMPILE
      AColor.h
      AudioIO.h
      BlockFile.h
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
      Sequence.h
      TimeTrack.h
      UndoManager.h
      WaveTrack.h
      ASlider.h
      ProgressDialog.h
      Ruler.h

   SOURCES
      LoadNyquist.cpp
      LoadNyquist.h
      Nyquist.cpp
      Nyquist.h
)

endif()
