do_directory(
   INCLUDE_PRIVATE
      src/etc
      src/track
      src/xml

   PRIVATE
      src/widgets
      wxWidgets

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
      TimeTrackControls.cpp
      TimeTrackControls.h
      TimeTrackVRulerControls.cpp
      TimeTrackVRulerControls.h
      TimeTrackView.cpp
      TimeTrackView.h
)
