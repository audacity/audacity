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
  EffectsViewerDialog o.. VstViewer
  EffectsViewerDialog o.. BuiltinEffectViewer
  BuiltinEffectViewer *-- BuiltinEffectViewLoader
  BuiltinEffectViewLoader o.. ReverbView: loads
  BuiltinEffectViewLoader o.. CompressorView: loads
  Vst3ViewLauncher ..> EffectsViewerDialog
  BuiltinViewLauncher ..> EffectsViewerDialog

  class ReverbView:::qml
  class CompressorView:::qml

  class BuiltinEffectViewLoader {
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

  class EffectsViewerDialog:::qml {
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
