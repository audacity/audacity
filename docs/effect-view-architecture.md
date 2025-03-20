```mermaid
---
  config:
    class:
      hideEmptyMembersBox: true
---
classDiagram
  VstViewer *-- VstView
  EffectsViewerDialog *-- EffectsViewer

  namespace legend {
    class CPP
    class QML:::qml
  }

  class IEffectViewLauncher {
    showEffect(instanceId)
    showRealtimeEffect(effectState)
  }


  IEffectViewLauncher <|-- Vst3ViewLauncher
  IEffectViewLauncher <|-- BuiltinViewLauncher

  RealtimeEffectViewerDialog *-- Loader
  Loader o.. EffectsViewer: sourceComponent
  Loader o.. VstViewer: sourceComponent
  VstViewerDialog *-- VstViewer
  EffectsViewer *-- EffectViewLoader
  EffectViewLoader o.. ReverbView: loads
  EffectViewLoader o.. CompressorView: loads
  Vst3ViewLauncher ..> VstViewerDialog
  BuiltinViewLauncher ..> EffectsViewerDialog

  class Loader:::qml
  class RealtimeEffectViewerDialog:::qml
  class ReverbView:::qml
  class CompressorView:::qml

  class EffectViewLoader {
    load(instanceId)
  }

  class EffectsViewerDialog:::qml {
    instanceId: string
    ------------
    manageBtn
    previewBtn
    cancelBtn
    applyBtn
  }

  class VstViewerDialog:::qml {
    instanceId: string
    ------------
    manageBtn
    previewBtn
    cancelBtn
    applyBtn
  }

  class RealtimeEffectViewerDialog:::qml {
    instanceId: string
    ------------
    bypassBtn
    manageBtn
  }

  class EffectsViewer:::qml {
    load(instanceId)
  }

  class VstViewer:::qml {
    load(instanceId)
  }

  class VstView {
    instanceId: string
  }

  classDef qml fill:darkgreen
```
