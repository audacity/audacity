/*
  This file is part of KDDockWidgets.

  SPDX-FileCopyrightText: 2019-2021 Klarälvdalens Datakonsult AB, a KDAB Group company <info@kdab.com>
  Author: Sérgio Martins <sergio.martins@kdab.com>

  SPDX-License-Identifier: GPL-2.0-only OR GPL-3.0-only

  Contact KDAB at <info@kdab.com> for commercial licensing options.
*/

#ifndef KD_DROPINDICATOROVERLAYINTERFACE_P_H
#define KD_DROPINDICATOROVERLAYINTERFACE_P_H

#include "kddockwidgets/docks_export.h"
#include "kddockwidgets/QWidgetAdapter.h"
#include "Frame_p.h"
#include "kddockwidgets/KDDockWidgets.h"

namespace KDDockWidgets {

class DropArea;

class DOCKS_EXPORT DropIndicatorOverlayInterface : public QWidgetAdapter
{
    Q_OBJECT
    Q_PROPERTY(QRect hoveredFrameRect READ hoveredFrameRect NOTIFY hoveredFrameRectChanged)
    Q_PROPERTY(KDDockWidgets::DropIndicatorOverlayInterface::DropLocation currentDropLocation READ currentDropLocation NOTIFY currentDropLocationChanged)
public:
    enum DropLocation
    {
        DropLocation_None = 0,
        DropLocation_Left,
        DropLocation_Top,
        DropLocation_Right,
        DropLocation_Bottom,
        DropLocation_Center,
        DropLocation_OutterLeft,
        DropLocation_OutterTop,
        DropLocation_OutterRight,
        DropLocation_OutterBottom,

        DropLocation_First = DropLocation_Left,
        DropLocation_Last = DropLocation_OutterBottom,
    };
    Q_ENUM(DropLocation)

    explicit DropIndicatorOverlayInterface(DropArea *dropArea);
    void setHoveredFrame(Frame *);
    void setWindowBeingDragged(bool);
    QRect hoveredFrameRect() const;
    bool isHovered() const;
    DropLocation currentDropLocation() const;
    Frame *hoveredFrame() const
    {
        return m_hoveredFrame;
    }
    void setCurrentDropLocation(DropIndicatorOverlayInterface::DropLocation location);

    KDDockWidgets::DropIndicatorOverlayInterface::DropLocation hover(QPoint globalPos);

    /// Clears and hides drop indicators
    void removeHover();

    /// @brief returns the position of the specified drop location
    /// The return is in global coordinates
    virtual QPoint posForIndicator(DropLocation) const = 0;

    static KDDockWidgets::Location multisplitterLocationFor(DropLocation);

Q_SIGNALS:
    void hoveredFrameChanged(KDDockWidgets::Frame *);
    void hoveredFrameRectChanged();
    void currentDropLocationChanged();

private:
    void onFrameDestroyed();
    void setHoveredFrameRect(QRect);
    QRect m_hoveredFrameRect;
    DropLocation m_currentDropLocation = DropLocation_None;

protected:
    virtual DropIndicatorOverlayInterface::DropLocation hover_impl(QPoint globalPos) = 0;
    virtual void onHoveredFrameChanged(Frame *);
    virtual void updateVisibility() {};

    Frame *m_hoveredFrame = nullptr;
    DropArea *const m_dropArea;
    bool m_draggedWindowIsHovering = false;
};

}

#endif
