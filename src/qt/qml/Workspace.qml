import QtQuick

QtObject {
   id: root
   objectName: "Workspace"

   enum Mode {
      Classic,
      SimpleRecording,
      AudioEditing,
      SpectralEditing
   }
}
