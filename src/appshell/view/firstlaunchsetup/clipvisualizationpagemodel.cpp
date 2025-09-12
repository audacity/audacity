#include "clipvisualizationpagemodel.h"

#include "global/translation.h"

using namespace au::appshell;
using namespace au::projectscene;
using namespace muse;

ClipVisualizationPageModel::ClipVisualizationPageModel(QObject* parent)
    : QObject(parent)
{
    // Listen to clip style changes from the configuration
    m_projectSceneConfiguration()->clipStyleChanged().onReceive(this, [this](const ClipStyles::Style& style) {
        Q_UNUSED(style);
        updateClipStyles();
    });
}

void ClipVisualizationPageModel::load()
{
    // Initialize with clip style options
    m_clipStyles.clear();

    ClipStyleInfo colorful;
    colorful.m_style = static_cast<int>(ClipStyles::Style::COLORFUL);
    colorful.m_title = qtrc("appshell/gettingstarted", "Colorful");
    colorful.m_description = qtrc("appshell/gettingstarted", "Each track gets a new color");
    colorful.m_imagePath = "resources/ClipVisuals_ColorfulClips.png";
    colorful.m_selected = false;
    m_clipStyles.append(colorful);

    ClipStyleInfo classic;
    classic.m_style = static_cast<int>(ClipStyles::Style::CLASSIC);
    classic.m_title = qtrc("appshell/gettingstarted", "Classic");
    classic.m_description = qtrc("appshell/gettingstarted", "The clips you know and love");
    classic.m_imagePath = "resources/ClipVisuals_ClassicClips.png";
    classic.m_selected = false;
    m_clipStyles.append(classic);

    updateClipStyles();
}

void ClipVisualizationPageModel::selectClipStyle(int style)
{
    ClipStyles::Style currentStyle = m_projectSceneConfiguration()->clipStyle();
    ClipStyles::Style newStyle = static_cast<ClipStyles::Style>(style);

    if (currentStyle == newStyle) {
        return;
    }

    // Save the clip style preference to configuration
    m_projectSceneConfiguration()->setClipStyle(newStyle);

    // updateClipStyles() will be called automatically via the clipStyleChanged signal
}

QVariantList ClipVisualizationPageModel::clipStyles() const
{
    QVariantList result;
    for (const ClipStyleInfo& style : m_clipStyles) {
        result << style.toMap();
    }
    return result;
}

int ClipVisualizationPageModel::currentClipStyle() const
{
    return static_cast<int>(m_projectSceneConfiguration()->clipStyle());
}

QString ClipVisualizationPageModel::currentImagePath() const
{
    const int currentStyleValue = currentClipStyle();
    for (const ClipStyleInfo& style : m_clipStyles) {
        if (style.m_style == currentStyleValue) {
            return style.m_imagePath;
        }
    }
    return ""; // Fallback
}

QString ClipVisualizationPageModel::pageTitle()
{
    return qtrc("appshell/gettingstarted", "Clip visualization");
}

void ClipVisualizationPageModel::updateClipStyles()
{
    const int currentStyleValue = currentClipStyle();
    for (ClipStyleInfo& style : m_clipStyles) {
        style.m_selected = (style.m_style == currentStyleValue);
    }
    emit clipStylesChanged();
    emit currentClipStyleChanged();
}
