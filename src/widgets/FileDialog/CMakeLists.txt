set( MY_SOURCES
   FileDialog.cpp
   FileDialog.h
)

# select the platform specific implementation files
if( wxIS_WIN )
   list( APPEND MY_SOURCES win/FileDialogPrivate.cpp win/FileDialogPrivate.h )
elseif( wxIS_MAC )
   list( APPEND MY_SOURCES mac/FileDialogPrivate.h )
elseif( wxIS_GTK )
   list( APPEND MY_SOURCES gtk/FileDialogPrivate.cpp gtk/FileDialogPrivate.h )
endif()

do_directory(
   INCLUDE_PRIVATE
      src/etc
      src/widgets

   PRIVATE
      wxWidgets

   OBJCPP_SOURCES
      mac/FileDialogPrivate.mm

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
      ${MY_SOURCES}
)
