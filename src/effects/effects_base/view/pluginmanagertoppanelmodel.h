/*
 * Audacity: A Digital Audio Editor
 */
#pragma once

#include "ieffectsprovider.h"

#include "framework/global/modularity/ioc.h"

#include <QObject>
#include <QQmlParserStatus>

namespace au::effects {
class DropdownOptionsModel;

class PluginManagerTopPanelModel : public QObject, public QQmlParserStatus
{
    Q_OBJECT

    Q_PROPERTY(DropdownOptionsModel * showModel READ showModel CONSTANT)
    Q_PROPERTY(DropdownOptionsModel * typeModel READ typeModel CONSTANT)
    Q_PROPERTY(DropdownOptionsModel * categoryModel READ categoryModel CONSTANT)

    muse::GlobalInject<IEffectsProvider> effectsProvider;

public:
    explicit PluginManagerTopPanelModel(QObject* parent = nullptr);

    DropdownOptionsModel* showModel() const;
    DropdownOptionsModel* typeModel() const;
    DropdownOptionsModel* categoryModel() const;

private:
    void classBegin() override {}
    void componentComplete() override;

    DropdownOptionsModel* const m_showModel;
    DropdownOptionsModel* const m_typeModel;
    DropdownOptionsModel* const m_categoryModel;
};
}
