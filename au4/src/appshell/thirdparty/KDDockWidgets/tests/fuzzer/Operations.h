/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

// We don't care about performance related checks in the tests
// clazy:excludeall=ctor-missing-parent-argument,missing-qobject-macro,range-loop,missing-typeinfo,detaching-member,function-args-by-ref,non-pod-global-static,reserve-candidates,qstring-allocations

#ifndef KDDOCKWIDGETS_TESTING_OPERATIONS_H
#define KDDOCKWIDGETS_TESTING_OPERATIONS_H

#include "KDDockWidgets.h"
#include "../Testing.h"

#include <QObject>
#include <QVector>
#include <QMetaEnum>

#include <memory>

namespace KDDockWidgets {

class DockWidgetBase;
class MainWindowBase;

namespace Testing {

class Fuzzer;

namespace Operations {
Q_NAMESPACE
enum OperationType {
    OperationType_None = 0,
    OperationType_CloseViaDockWidgetAPI, ///< Closing programmatically via DockWidget::close()
    OperationType_HideViaDockWidgetAPI,  ///< Hiding programmatically via DockWidget::hide()
    OperationType_ShowViaDockWidgetAPI,  ///< Hiding programmatically via DockWidget::show()
    OperationType_AddDockWidget,         ///< MainWindow::addDockWidget()
    OperationType_AddDockWidgetAsTab,    ///< DockWidget::addDockWidgetAsTab()
    OperationType_SaveLayout, ///< LayoutSaver::saveLayout()
    OperationType_RestoreLayout, ///< LayoutSaver::restoreLayout()
    OperationType_Count /// Keep at end
};
Q_ENUM_NS(OperationType)


///@brief Describes a testable action.
class OperationBase
{
    Q_DISABLE_COPY(OperationBase)
public:
    typedef std::shared_ptr<OperationBase> Ptr;
    typedef QVector<OperationBase::Ptr> List;
    explicit OperationBase(OperationType, Fuzzer *);
    virtual ~OperationBase();

    void execute();

    QVariantMap toVariantMap() const;
    static OperationBase::Ptr fromVariantMap(Fuzzer *fuzzer, const QVariantMap &);
    static OperationBase::Ptr newOperation(Fuzzer *fuzzer, OperationType);

    OperationType type() const { return m_operationType; }
    QString description() const { return m_description; }

    virtual void updateDescription() = 0;
    virtual bool hasParams() const = 0;
    QString toString();

protected:
    virtual void execute_impl() = 0;
    virtual void generateRandomParams() = 0;
    virtual QVariantMap paramsToVariantMap() const = 0;
    virtual void fillParamsFromVariantMap(const QVariantMap &) = 0;
    DockWidgetBase* dockByName(const QString &) const;
    MainWindowBase* mainWindowByName(const QString &) const;
    QString dockStr(const QString &name) const;

    const OperationType m_operationType;
    Fuzzer *const m_fuzzer;
    int m_sleepMS = 0;
    QString m_description;
};

class CloseViaDockWidgetAPI : public OperationBase
{
    Q_DISABLE_COPY(CloseViaDockWidgetAPI)
public:
    explicit CloseViaDockWidgetAPI(Fuzzer *);

protected:
    void updateDescription() override;
    void generateRandomParams() override;
    bool hasParams() const override;
    void execute_impl() override;
    QVariantMap paramsToVariantMap() const override;
    void fillParamsFromVariantMap(const QVariantMap &) override;
    QString m_dockWidgetName;
};

class HideViaDockWidgetAPI : public OperationBase
{
    Q_DISABLE_COPY(HideViaDockWidgetAPI)
public:
    explicit HideViaDockWidgetAPI(Fuzzer *);

protected:
    void generateRandomParams() override;
    bool hasParams() const override;
    void updateDescription() override;
    void execute_impl() override;
    QVariantMap paramsToVariantMap() const override;
    void fillParamsFromVariantMap(const QVariantMap &) override;
    QString m_dockWidgetName;
};

class ShowViaDockWidgetAPI : public OperationBase
{
    Q_DISABLE_COPY(ShowViaDockWidgetAPI)
public:
    explicit ShowViaDockWidgetAPI(Fuzzer *);

protected:
    void generateRandomParams() override;
    bool hasParams() const override;
    void updateDescription() override;
    void execute_impl() override;
    QVariantMap paramsToVariantMap() const override;
    void fillParamsFromVariantMap(const QVariantMap &) override;
    QString m_dockWidgetName;
};

class AddDockWidget : public OperationBase
{
    Q_DISABLE_COPY(AddDockWidget)
public:
    explicit AddDockWidget(Fuzzer *);

protected:
    void generateRandomParams() override;
    bool hasParams() const override;
    void updateDescription() override;
    void execute_impl() override;
    QVariantMap paramsToVariantMap() const override;
    void fillParamsFromVariantMap(const QVariantMap &) override;
private:
    Testing::AddDockWidgetParams m_params;
};

class AddDockWidgetAsTab : public OperationBase
{
    Q_DISABLE_COPY(AddDockWidgetAsTab)
public:
    explicit AddDockWidgetAsTab(Fuzzer *);

protected:
    void generateRandomParams() override;
    bool hasParams() const override;
    void updateDescription() override;
    void execute_impl() override;
    QVariantMap paramsToVariantMap() const override;
    void fillParamsFromVariantMap(const QVariantMap &) override;
private:
    QString m_dockWidgetName;
    QString m_dockWidgetToAddName;
};

class SaveLayout : public OperationBase
{
    Q_DISABLE_COPY(SaveLayout)
public:
    explicit SaveLayout(Fuzzer *);

protected:
    bool hasParams() const override;
    void updateDescription() override;
    void execute_impl() override;

    void generateRandomParams() override;
    QVariantMap paramsToVariantMap() const override;
    void fillParamsFromVariantMap(const QVariantMap &) override;
};

class RestoreLayout : public OperationBase
{
    Q_DISABLE_COPY(RestoreLayout)
public:
    explicit RestoreLayout(Fuzzer *);

protected:
    bool hasParams() const override;
    void updateDescription() override;
    void execute_impl() override;

    void generateRandomParams() override;
    QVariantMap paramsToVariantMap() const override;
    void fillParamsFromVariantMap(const QVariantMap &) override;
};

}
}
}

#endif
