set(MODULE_NAME numeric-formats)

set(MODULE_SOURCES
   NumericConverterFormatterAdapter.cpp
   NumericConverterFormatterAdapter.h
   NumericConverterFormatterAdapterFactory.cpp
   NumericConverterFormatterAdapterFactory.h
)

set(MODULE_QML_SOURCES
   qml/NumericTextControl.qml
)

audacity_qml_module(${MODULE_NAME}
   URI "Audacity.NumericFormats"
   VERSION 1.0
   STATIC
   DEPENDENCIES
      QtQuick
      Audacity.UiComponents
   QML_FILES
      ${MODULE_QML_SOURCES}
   SOURCES
      ${MODULE_SOURCES}
   LIBRARIES
      lib-qt-init-interface
      lib-numeric-formats-interface
)
