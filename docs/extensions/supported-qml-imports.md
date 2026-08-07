# Supported QML imports for extensions

Audacity extensions that provide QML user interfaces may rely on the following
Qt modules being present in release packages:

- `QtQuick`
- `QtQuick.Controls`, using the style selected for the current platform
- `QtQuick.Layouts`
- `QtQuick.Window`
- `QtQuick.Shapes`
- `QtQuick.Effects`
- `Qt5Compat.GraphicalEffects`
- `QtQml`
- `QtQml.Models`
- `Qt.labs.platform`

Transitive implementation modules required by these imports, including
`QtQml.WorkerScript`, `Qt.labs.qmlmodels`, `QtQuick.Templates`, and the selected
Controls style implementation, are deployed automatically.

Other Qt QML modules are not part of the extension compatibility surface and
may be absent from release packages. In particular, extensions must not depend
on Dialogs, LocalStorage, Particles, VectorImage, XmlListModel, Quick3D, Graphs,
or WebSockets. Extensions should not force a particular Controls style because
the available style is platform-dependent.
