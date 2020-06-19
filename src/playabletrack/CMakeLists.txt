do_directory(
   INCLUDE_PRIVATE
      src/etc
      src/track
      src/wavetrack
      src/xml

   PRIVATE
      src/widgets
      wxWidgets

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
      PlayableTrackButtonHandles.cpp
      PlayableTrackButtonHandles.h
      PlayableTrackControls.cpp
      PlayableTrackControls.h
)

