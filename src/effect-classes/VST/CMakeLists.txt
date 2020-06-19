if(USE_VST)

set(
   MY_SOURCES
      VSTEffect.cpp
      VSTEffect.h
      aeffectx.h
)

if(wxIS_GTK)
   list( APPEND MY_SOURCES VSTControlGTK.cpp VSTControlGTK.h )
endif()

if(wxIS_WIN)
   list( APPEND MY_SOURCES VSTControlMSW.cpp VSTControlMSW.h )
endif()

if(wxIS_MAC)
   list( APPEND MY_SOURCES VSTControlOSX.h )
endif()

do_directory(
   INCLUDE_PRIVATE
      src/effects
      src/etc
      src/track
      src/xml

   PRIVATE
      src/widgets
      wxWidgets

   OBJCPP_SOURCES
      VSTControlOSX.mm

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
      ${MY_SOURCES}
)

endif()
