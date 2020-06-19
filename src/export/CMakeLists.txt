set(
 MY_SOURCES
      Export.cpp
      Export.h
      ExportCL.cpp
      ExportMP3.cpp
      ExportMP3.h
      ExportMultiple.cpp
      ExportMultiple.h
      ExportPCM.cpp
)

# Optional exporters
if( USE_FFMPEG )
   list( APPEND MY_SOURCES
      ExportFFmpeg.cpp
      ExportFFmpegDialogs.cpp
      ExportFFmpegDialogs.h )
endif()

if( USE_LIBFLAC )
   list( APPEND MY_SOURCES ExportFLAC.cpp )
endif()

if( USE_LIBTWOLAME )
   list( APPEND MY_SOURCES ExportMP2.cpp )
endif()

if( USE_LIBOGG AND USE_LIBVORBIS )
   list( APPEND MY_SOURCES ExportOGG.cpp )
endif()

do_directory(
   INCLUDE_PRIVATE
      src/etc
      src/ondemand
      src/prefs
      src/track
      src/xml

   PRIVATE
      src/widgets
      wxWidgets
      lame
      libsndfile
      $<$<BOOL:${USE_FFMPEG}>:ffmpeg>
      $<$<BOOL:${USE_LIBFLAC}>:libflac>
      $<$<BOOL:${USE_LIBID3TAG}>:libid3tag>
      $<$<BOOL:${USE_LIBOGG}>:libogg>
      $<$<BOOL:${USE_LIBVORBIS}>:libvorbis>
      $<$<BOOL:${USE_LIBTWOLAME}>:twolame>

   PRECOMPILE
      AColor.h
      AudioIO.h
      BlockFile.h
      Diags.h
      DirManager.h
      Envelope.h
      FFT.h
      FileFormats.h
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
      ${MY_SOURCES}
)
