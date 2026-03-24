/*
* Audacity: A Digital Audio Editor
*/
#include "geteffectsmodel.h"

#include <QPointer>

#include "framework/global/runtime.h"
#include "framework/global/async/async.h"

#include "au3-musehub/MuseHubService.h"

using namespace au::projectscene;

GetEffectsModel::GetEffectsModel(QObject* parent)
    : QObject(parent), muse::Injectable(muse::iocCtxForQmlObject(this))
{
}

void GetEffectsModel::load()
{
    setIsLoading(true);
    setHasError(false);

    audacity::musehub::GetEffects([this, guard = QPointer(this)](std::vector<audacity::musehub::EffectsGroup> groups)
    {
        if (!guard) {
            return;
        }
        muse::async::Async::call(this, [this, guard, groups = std::move(groups)]()
        {
            if (!guard) {
                return;
            }
            m_allGroups.clear();
            m_categories.clear();

            for (const auto& group : groups) {
                EffectsGroupData groupData;
                groupData.title = QString::fromStdString(group.title);
                for (const auto& effect : group.effects) {
                    EffectData effectData;
                    effectData.iconUrl = QString::fromStdString(effect.iconUrl);
                    effectData.code = QString::fromStdString(effect.code);
                    effectData.title = QString::fromStdString(effect.title);
                    effectData.subtitle = QString::fromStdString(effect.subtitle);
                    effectData.category = QString::fromStdString(effect.category);
                    if (effectData.code == "openvino-ai-tools") {
                        // TODO: add support for OpenVino AI Tools in Audacity 4
                        // disable Open Vino AI Tools from listing until support is added
                        continue;
                    }
                    groupData.effects.append(effectData);
                }
                if (!groupData.effects.isEmpty()) {
                    m_allGroups.append(groupData);
                    m_categories.append(groupData.title);
                }
            }

            setIsLoading(false);
            if (m_allGroups.isEmpty()) {
                setHasError(true);
            }
            emit categoriesChanged();
            emit effectsGroupsChanged();
        }, muse::runtime::mainThreadId());
    });
}

void GetEffectsModel::openUrl(const std::string& url) const
{
    const muse::Ret ret = platformInteractive()->openUrl(url);
    if (!ret) {
        LOGE() << ret.toString();
    }
}

void GetEffectsModel::openEffectUrl(const QString& effectCode) const
{
    const std::string url = audacity::musehub::GetEffectUrl(effectCode.toStdString());
    openUrl(url);
}

void GetEffectsModel::openBecomeAPartnerUrl() const
{
    const std::string url = audacity::musehub::GetBecomeAPartnerUrl();
    openUrl(url);
}

QVariantList GetEffectsModel::effectsGroups() const
{
    QVariantList result;
    for (int i = 0; i < m_allGroups.size(); ++i) {
        const auto& group = m_allGroups[i];
        QVariantList effectsList;
        for (const auto& effect : group.effects) {
            QVariantMap effectMap;
            effectMap["iconUrl"] = effect.iconUrl;
            effectMap["code"] = effect.code;
            effectMap["title"] = effect.title;
            effectMap["subtitle"] = effect.subtitle;
            effectMap["category"] = effect.category;
            effectsList.append(effectMap);
        }
        QVariantMap groupMap;
        groupMap["title"] = group.title;
        groupMap["effects"] = effectsList;
        result.append(groupMap);
    }
    return result;
}

QVariantList GetEffectsModel::categories() const
{
    QVariantList result;
    for (const auto& cat : m_categories) {
        result.append(cat);
    }
    return result;
}

int GetEffectsModel::selectedCategoryIndex() const
{
    return m_selectedCategoryIndex;
}

void GetEffectsModel::setSelectedCategoryIndex(int index)
{
    if (m_selectedCategoryIndex == index) {
        return;
    }
    m_selectedCategoryIndex = index;
    emit selectedCategoryIndexChanged();
}

bool GetEffectsModel::isLoading() const { return m_isLoading; }
bool GetEffectsModel::hasError() const { return m_hasError; }

void GetEffectsModel::setIsLoading(bool loading)
{
    if (m_isLoading == loading) {
        return;
    }
    m_isLoading = loading;
    emit isLoadingChanged();
}

void GetEffectsModel::setHasError(bool error)
{
    if (m_hasError == error) {
        return;
    }
    m_hasError = error;
    emit hasErrorChanged();
}
