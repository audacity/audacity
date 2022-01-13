/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   DeepModelManager.h
   Hugo Flores Garcia

******************************************************************/
/**

\file DeepModelManager.h
\brief tools for downloading and managing Audacity models hosted in HuggingFace

*/
/*******************************************************************/

#pragma once

#include "DeepModel.h"
#include "ModelCard.h"

#include "Request.h"
#include "NetworkManager.h"
#include "widgets/ProgressDialog.h"
#include "AudacityException.h"

// this exception should be caught internally, but we 
// derive from MessageBoxException just in case it needs to 
// get handled by Audacity
class ModelManagerException final : public MessageBoxException
{
public:
   ModelManagerException(const TranslatableString msg) : 
                        m_msg(std::move(msg)),
                        MessageBoxException{
                           ExceptionType::Internal,
                           XO("Model Manager Error")
                        } 
   {}

   //! internal message
   virtual const char* what() const noexcept
      { return m_msg.Translation().c_str(); }

   //! user facing message
   virtual TranslatableString ErrorMessage() const
      { return XO("Model Manager Error: \n %s").Format(m_msg);}

   const TranslatableString m_msg;
};

using RepoIDList = std::vector<std::string>;

// callbacks
using CompletionHandler = std::function<void (int httpCode, std::string responseBody)>;
using ProgressCallback = std::function<void(int64_t current, int64_t expected)>;

using ModelSizeCallback = std::function<void(size_t size)>;
using CardFetchProgressCallback = std::function<void(int64_t current, int64_t expected)>;
using RepoListFetchedCallback = std::function<void(bool success, RepoIDList repos)>;
using CardFetchedCallback = std::function<void(bool succcess, ModelCardHolder card)>;

class DeepModelManager final
{
   // private! Use Get()
   DeepModelManager(); 
   ~DeepModelManager() = default;

   DeepModelManager(const DeepModelManager&) = delete;
   DeepModelManager &operator=(const DeepModelManager&) = delete;

   FilePath GetRepoDir(ModelCardHolder card) const;

   std::string GetRootURL(const std::string &repoID) const;
   std::string GetFileURL(const std::string &repoID, const std::string &filePath) const;
   audacity::network_manager::ResponsePtr doGet(std::string url, CompletionHandler completionHandler, 
                                                ProgressCallback onProgress=NULL);

   void FetchRepos(RepoListFetchedCallback onReposFetched);

   //! download a model from HuggingFace.
   audacity::network_manager::ResponsePtr DownloadModel(ModelCardHolder card, 
                                                        ProgressCallback onProgress, 
                                                        CompletionHandler onCompleted);

public:

   static DeepModelManager& Get();
   static void Shutdown();

   // loads the deep model and passes ownership to the caller
   DeepModelHolder GetModel(ModelCardHolder card) const;

   // returns a URL to the HF's repo's readme
   std::string GetMoreInfoURL(ModelCardHolder card) const;

   // download and install a deep learning model
   bool IsInstalled(ModelCardHolder card) const;
   bool IsInstalling(ModelCardHolder card) const;

   // may fail silently, check with IsInstalled()
   void Install(ModelCardHolder card, ProgressCallback onProgress, 
                                 CompletionHandler onCompleted);
   void Uninstall(ModelCardHolder card);
   void CancelInstall(ModelCardHolder card);

   void FetchLocalCards(CardFetchedCallback onCardFetched);
   void FetchModelCards(CardFetchedCallback onCardFetched, CardFetchProgressCallback onProgress);
   void FetchCard(const std::string &repoID, CardFetchedCallback onCardFetched);

   // if the card is local, checks the model file
   // else, it sends a HEAD request for the HF repo's model file
   // if this fails, the callback is not called. 
   void FetchModelSize(ModelCardHolder card, ModelSizeCallback onModelSizeRetrieved) const;

   ModelCardHolder GetEmptyCard() const;
   ModelCardCollection GetCards() const { return mCards; }
   ModelCardCollection GetCards(const std::string &effect_type) const;

private:
   // factory functions for model cards
   bool NewCardFromHuggingFace(ModelCardHolder card, const std::string &jsonBody, const std::string &repoID);
   bool NewCardFromLocal(ModelCardHolder card, const std::string &filePath);

   std::mutex mCardMutex;
   ModelCardCollection mCards;
   
   std::map<std::string, audacity::network_manager::ResponsePtr> mResponseMap;

   const std::string mAPIEndpoint;
   Doc mModelCardSchema;
};

