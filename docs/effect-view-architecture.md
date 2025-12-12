```mermaid
---
  config:
    class:
      hideEmptyMembersBox: true
---
classDiagram
  VstViewer *-- VstView

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

  RealtimeEffectViewerDialog o.. BuiltinEffectViewer
  RealtimeEffectViewerDialog o.. VstViewer
  DestructiveEffectsViewerDialog o.. VstViewer
  DestructiveEffectsViewerDialog o.. BuiltinEffectViewer
  BuiltinEffectViewer *-- BuiltinEffectViewLoader
  BuiltinEffectViewLoader o.. ReverbView: loads
  BuiltinEffectViewLoader o.. CompressorView: loads
  Vst3ViewLauncher ..> DestructiveEffectsViewerDialog
  BuiltinViewLauncher ..> DestructiveEffectsViewerDialog

  class ReverbView:::qml
  class CompressorView:::qml

  class BuiltinEffectViewLoader {
    load(instanceId)
  }

  class DestructiveEffectsViewerDialog:::qml {
    instanceId: string
    ------------
    manageBtn
    previewBtn
    cancelBtn
    applyBtn
  }

  class DestructiveEffectsViewerDialog:::qml {
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

  class BuiltinEffectViewer:::qml {
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
