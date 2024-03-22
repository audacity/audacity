/*
 * SPDX-License-Identifier: GPL-3.0-only
 * MuseScore-CLA-applies
 *
 * MuseScore
 * Music Composition & Notation
 *
 * Copyright (C) 2021 MuseScore BVBA and others
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License version 3 as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
#include "uicomponentsmodule.h"

#include <QtQml>
#include "view/sampleobject.h"
#include "view/validators/doubleinputvalidator.h"
#include "view/validators/intinputvalidator.h"
#include "view/iconview.h"
#include "view/sortfilterproxymodel.h"
#include "view/popupview.h"
#include "view/dialogview.h"
#include "view/dropdownview.h"
#include "view/menuview.h"
#include "view/filepickermodel.h"
#include "view/colorpickermodel.h"
#include "view/itemmultiselectionmodel.h"
#include "view/textinputfieldmodel.h"
#include "view/selectmultipledirectoriesmodel.h"
#include "view/buttonboxmodel.h"

#include "view/treeview/qquicktreemodeladaptor_p.h"
#include "view/treeview/qquickrangemodel_p.h"
#include "view/treeview/qquickwheelarea_p.h"
#include "view/treeview/qquickabstractstyle_p.h"
#include "view/treeview/qquickselectionmode_p.h"

#include "modularity/ioc.h"
#include "ui/iuiengine.h"

#include "ui/uitypes.h"
#include "ui/iinteractiveuriregister.h"

using namespace mu::uicomponents;
using namespace mu::ui;
using namespace mu::modularity;

static void uicomponents_init_qrc()
{
    Q_INIT_RESOURCE(uicomponents);

#ifdef MU_QT5_COMPAT
    Q_INIT_RESOURCE(graphicaleffects_qt5);
#else
    Q_INIT_RESOURCE(graphicaleffects);
#endif
}

std::string UiComponentsModule::moduleName() const
{
    return "uicomponents";
}

void UiComponentsModule::registerExports()
{
}

void UiComponentsModule::resolveImports()
{
    auto ir = ioc()->resolve<IInteractiveUriRegister>(moduleName());
    if (ir) {
        ir->registerUri(Uri("musescore://interactive/selectMultipleDirectories"),
                        ContainerMeta(ContainerType::QmlDialog, "MuseScore/UiComponents/SelectMultipleDirectoriesDialog.qml"));
    }
}

void UiComponentsModule::registerResources()
{
    uicomponents_init_qrc();
}

void UiComponentsModule::registerUiTypes()
{
    qmlRegisterType<SampleObject>("MuseScore.UiComponents", 1, 0, "SampleObject");

    qmlRegisterUncreatableType<QAbstractItemModel>("MuseScore.UiComponents", 1, 0, "AbstractItemModel", "Cannot …");

    qmlRegisterType<DoubleInputValidator>("MuseScore.UiComponents", 1, 0, "DoubleInputValidator");
    qmlRegisterType<IntInputValidator>("MuseScore.UiComponents", 1, 0, "IntInputValidator");
    qmlRegisterType<IntInputValidator>("MuseScore.UiComponents", 1, 0, "IntInputValidator");

    qmlRegisterType<IconView>("MuseScore.UiComponents", 1, 0, "IconView");

    qmlRegisterType<SortFilterProxyModel>("MuseScore.UiComponents", 1, 0, "SortFilterProxyModel");
    qmlRegisterType<FilterValue>("MuseScore.UiComponents", 1, 0, "FilterValue");
    qmlRegisterType<SorterValue>("MuseScore.UiComponents", 1, 0, "SorterValue");
    qmlRegisterUncreatableType<CompareType>("MuseScore.UiComponents", 1, 0, "CompareType", "Cannot create a CompareType");

    qmlRegisterType<PopupView>("MuseScore.UiComponents", 1, 0, "PopupView");
    qmlRegisterType<DialogView>("MuseScore.UiComponents", 1, 0, "DialogView");
    qmlRegisterType<DropdownView>("MuseScore.UiComponents", 1, 0, "DropdownView");
    qmlRegisterType<MenuView>("MuseScore.UiComponents", 1, 0, "MenuView");

    qmlRegisterType<FilePickerModel>("MuseScore.UiComponents", 1, 0, "FilePickerModel");
    qmlRegisterType<ColorPickerModel>("MuseScore.UiComponents", 1, 0, "ColorPickerModel");
    qmlRegisterType<ItemMultiSelectionModel>("MuseScore.UiComponents", 1, 0, "ItemMultiSelectionModel");

    qmlRegisterType<TextInputFieldModel>("MuseScore.UiComponents", 1, 0, "TextInputFieldModel");
    qmlRegisterType<SelectMultipleDirectoriesModel>("MuseScore.UiComponents", 1, 0, "SelectMultipleDirectoriesModel");
    qmlRegisterType<ButtonBoxModel>("MuseScore.UiComponents", 1, 0, "ButtonBoxModel");

    qmlRegisterType<QQuickTreeModelAdaptor1>("MuseScore.UiComponents.Private", 1, 0, "TreeModelAdaptor");
    qmlRegisterType<QQuickRangeModel1>("MuseScore.UiComponents.Private", 1, 0, "RangeModel");
    qmlRegisterType<QQuickWheelArea1>("MuseScore.UiComponents.Private", 1, 0, "WheelArea");
    qmlRegisterType<QQuickAbstractStyle1>("MuseScore.UiComponents.Private", 1, 0, "AbstractStyle");
    qmlRegisterType<QQuickPadding1>("MuseScore.UiComponents.Private", 1, 0, "Padding");
    qmlRegisterUncreatableType<QQuickSelectionMode1>("MuseScore.UiComponents.Private", 1, 0, "SelectionMode",
                                                     QLatin1String("Do not create objects of type SelectionMode"));

    auto ui = modularity::ioc()->resolve<ui::IUiEngine>(moduleName());
    if (ui) {
        ui->addSourceImportPath(uicomponents_QML_IMPORT);
    }
}
