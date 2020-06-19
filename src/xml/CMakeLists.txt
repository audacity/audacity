do_directory( TYPE STATIC
   INCLUDE_PRIVATE
      src/etc

   PRIVATE
      wxWidgets
      expat

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

   SOURCES
      XMLFileReader.cpp
      XMLFileReader.h
      XMLTagHandler.cpp
      XMLTagHandler.h
      XMLWriter.cpp
      XMLWriter.h
      audacityproject.dtd
)
