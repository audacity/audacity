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
#include "ThreadPool/ThreadPool.h"

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
using InstallCompletionHandler = std::function<void()>;
using ProgressCallback = std::function<void(int64_t current, int64_t expected)>;

using CardFetchedCallback = std::function<void(ModelCardHolder card)>;
using CardExceptionCallback = std::function<void(const ModelManagerException &)>;

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

   //! performs a blocking GET request
   audacity::network_manager::ResponsePtr doGet(std::string url) const;

   //! download a model from HuggingFace.
   audacity::network_manager::ResponsePtr DownloadModel(ModelCardHolder card, 
                                                        ProgressCallback onProgress, 
                                                        InstallCompletionHandler onCompleted);

public:

   static DeepModelManager& Get();
   static void Shutdown();

   // loads the deep model and passes ownership to the caller
   DeepModelHolder GetModel(ModelCardHolder card) const;

   // returns a URL to the HF's repo's readme
   std::string GetMoreInfoURL(ModelCardHolder card) const;

   // download and install a deep learning model
   bool IsInstalled(ModelCardHolder card) const;
   bool IsInstalling(ModelCardHolder card);

   // may fail silently, check with IsInstalled()
   void Install(ModelCardHolder card, ProgressCallback onProgress, 
                                 InstallCompletionHandler onCompleted);
   void Uninstall(ModelCardHolder card);
   void CancelInstall(ModelCardHolder card, bool error = false);

   //! retrieves a list of repoIDs from the a HuggingFace URL. 
   //! performs a blocking network request. should NOT run from UI thread. 
   RepoIDList FetchRepos() const;

   //! fetch all modelcards available locally, and insert them into mCards
   //! this function CAN be called from UI thread, as it does not block, but returns immediately
   //! callbacks are scheduled to run from the main UI thread. 
   //! all validation errors are caught and logged to the error log. 
   void FetchLocalCards(CardFetchedCallback onCardFetched);

   //! fetch all modelcards available in HuggingFace, and insert them into mCards.
   //! this function CAN be called from a UI thread, as it does not block, but returns immediately.
   //! callbacks are scheduled to run from the main UI thread. 
   //! all validation errors are caught and logged to the error log. 
   void FetchHuggingFaceCards(CardFetchedCallback onCardFetched);

   //! fetch a single card from HuggingFace, given a RepoID, and insert it into mCards. 
   //! if block == true, this function CAN be called from a UI thread, as it does not block, but returns immediately.
   //! callbacks are scheduled to run from the main UI thread. 
   //! all validation errors are caught and logged to the error log. 
   void AddHuggingFaceCard(const std::string &repoID, 
                           CardFetchedCallback onCardFetched, 
                           CardExceptionCallback onCardException, 
                           bool block);

   //! performs a blocking network request. should NOT run from UI thread. 
   //! may throw ModelCardException if the json fails to parse
   //! may throw a ModelManagerException if there's an error downloading the card
   ModelCardHolder FetchCard(const std::string &repoID) const;

   //! performs a blocking network request. should NOT run from UI thread. 
   //! may throw a ModelManagerException if there's an error in the HTTP query
   //! may throw a ModelManagerException if the card is local and the model file doesn't exist. 
   //! if the card is local, checks the model file
   //! else, it sends a HEAD request for the HF repo's model file
   void FetchModelSize(ModelCardHolder card) const;

   ModelCardHolder GetEmptyCard() const;
   ModelCardCollection GetCards() const { return mCards; }
   ModelCardCollection GetCards(const std::string &effect_type) const;

private:
   //! factory functions for model cards
   //! may throw InvalidModelCardDocument if the json fails to parse
   ModelCardHolder NewCardFromHuggingFace(const std::string &jsonBody, const std::string &repoID) const;
   ModelCardHolder NewCardFromLocal(const std::string &filePath);

   std::mutex mCardMutex;
   ModelCardCollection mCards;
   
   std::mutex mResponseMutex;
   std::map<std::string, audacity::network_manager::ResponsePtr> mResponseMap;

   std::unique_ptr<ThreadPool> mThreadPool {nullptr};
   const std::string mAPIEndpoint;
   Doc mModelCardSchema;
};

