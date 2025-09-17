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

QString ClipVisualizationPageModel::navigationAccessibleName()
{
    return qtrc("appshell/gettingstarted", "Clip visualization options");
}

QString ClipVisualizationPageModel::navigationAccessibleDescription()
{
    return qtrc("appshell/gettingstarted", "Choose how audio clips are displayed in the timeline");
}

QString ClipVisualizationPageModel::pageAccessibleDescription()
{
    return qtrc("appshell/gettingstarted", "Select your preferred clip visualization style. Preview is shown on the right.");
}

QString ClipVisualizationPageModel::previewAccessibleName()
{
    return qtrc("appshell/gettingstarted", "Clip visualization preview");
}

QString ClipVisualizationPageModel::previewAccessibleDescription()
{
    return qtrc("appshell/gettingstarted", "Preview of how audio clips will appear with the selected visualization style");
}

QString ClipVisualizationPageModel::currentlySelectedText()
{
    return qtrc("appshell/gettingstarted", "Currently selected");
}

QString ClipVisualizationPageModel::clickToSelectText()
{
    return qtrc("appshell/gettingstarted", "Click to select this style");
}

QString ClipVisualizationPageModel::availableOptionText()
{
    return qtrc("appshell/gettingstarted", "Available option");
}

QString ClipVisualizationPageModel::formatNavigationDescription(const QString& description, bool selected) const
{
    const QString statusText = selected ? currentlySelectedText() : clickToSelectText();
    //: %1 is the clip style description (e.g. "Each track gets a new color"), %2 is the selection status (e.g. "Currently selected" or "Click to select this style")
    return qtrc("appshell/gettingstarted", "%1. %2").arg(description).arg(statusText);
}

QString ClipVisualizationPageModel::formatAccessibleDescription(const QString& description, bool selected) const
{
    const QString statusText = selected ? currentlySelectedText() : availableOptionText();
    //: %1 is the clip style description (e.g. "Each track gets a new color"), %2 is the availability status (e.g. "Currently selected" or "Available option")
    return qtrc("appshell/gettingstarted", "%1. %2").arg(description).arg(statusText);
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
