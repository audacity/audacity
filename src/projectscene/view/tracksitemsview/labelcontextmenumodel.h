/*
* Audacity: A Digital Audio Editor
*/
#pragma once

#include "context/iglobalcontext.h"
#include "uicomponents/qml/Muse/UiComponents/abstractmenumodel.h"

#include "iprojectsceneconfiguration.h"
#include "types/projectscenetypes.h"

namespace au::projectscene {
class LabelContextMenuModel : public muse::uicomponents::AbstractMenuModel
{
    Q_OBJECT

    muse::GlobalInject<projectscene::IProjectSceneConfiguration> projectSceneConfiguration;

    muse::Inject<context::IGlobalContext> globalContext;

    Q_PROPERTY(LabelKey labelKey READ labelKey WRITE setLabelKey NOTIFY labelKeyChanged FINAL)

public:
    LabelContextMenuModel() = default;

    Q_INVOKABLE void load() override;
    Q_INVOKABLE void handleMenuItem(const QString& itemId) override;

    LabelKey labelKey() const;
    void setLabelKey(const LabelKey& key);

signals:
    void labelKeyChanged();
    void labelEditRequested();

private:
    LabelKey m_labelKey;
};
}
