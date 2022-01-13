/**********************************************************************

   Audacity: A Digital Audio Editor
   Audacity(R) is copyright (c) 1999-2021 Audacity Team.

   DeepModelManager.cpp
   Hugo Flores Garcia

******************************************************************/

#include "DeepModelManager.h"

#include "CodeConversions.h"
#include "NetworkManager.h"
#include "IResponse.h"
#include "Request.h"

#include <wx/tokenzr.h>
#include <wx/log.h>

using namespace audacity;

#define MODEL_FILENAME "model.pt"
#define METADATA_FILENAME "metadata.json"

namespace {
   size_t GetFileSize(wxFileName path)
   {
      wxFile file = wxFile(path.GetFullPath(), wxFile::read);
      if (!file.IsOpened())
         throw ModelManagerException(XO("Failed to open file %s").Format(path.GetFullPath()));

      return static_cast<size_t>(file.Length());
   }
}
 
DeepModelManager::DeepModelManager() :
   mAPIEndpoint("https://huggingface.co/api/")
{
   const std::string schemaPath = audacity::ToUTF8(
      wxFileName(DeepModel::BuiltInModulesDir(), wxT("modelcard-schema.json"))
                  .GetFullPath()
   );
   mModelCardSchema = parsers::ParseFile(schemaPath);
}

DeepModelManager& DeepModelManager::Get()
{
   static DeepModelManager manager;
   return manager;
}

FilePath DeepModelManager::GetRepoDir(ModelCardHolder card) const
{
   static const std::string sep = "_";

   FilePath repoDir = FileNames::MkDir( 
      wxFileName(DeepModel::DLModelsDir(),
                 card->author() + sep + card->name() 
      ).GetFullPath()
   );

   return repoDir;
}

DeepModelHolder DeepModelManager::GetModel(ModelCardHolder card) const
{
   if (!IsInstalled(card))
      throw ModelManagerException(XO("Model is not loaded."));

   DeepModelHolder model = std::make_shared<DeepModel>();
   model->SetCard(card);

   // GetRepoDir won't work if the card is empty
   wxFileName path = wxFileName(GetRepoDir(card), MODEL_FILENAME);

   // finally, load
   model->Load(audacity::ToUTF8(path.GetFullPath()));

   return model;
}

std::string DeepModelManager::GetMoreInfoURL(ModelCardHolder card) const
{
   return "https://huggingface.co/" + card->GetRepoID();
}

bool DeepModelManager::IsInstalled(ModelCardHolder card) const
{
   FilePath repoDir = wxFileName(card->local_path()).GetFullPath();

   wxFileName modelPath = wxFileName(repoDir, MODEL_FILENAME);
   wxFileName metadataPath = wxFileName(repoDir, METADATA_FILENAME);

   bool success = modelPath.FileExists() && metadataPath.FileExists();
   if (modelPath.FileExists())
   {
      size_t model_size = GetFileSize(modelPath);
      success = success &&  (model_size == card->model_size());
   };

   return success;
}

void DeepModelManager::Install(ModelCardHolder card, ProgressCallback onProgress, CompletionHandler onCompleted)
{ 
   if (IsInstalled(card))
      return ;

   ProgressCallback progressHandler(
   [this, card, handler {std::move(onProgress)}](int64_t current, int64_t expected)
   {
      // if the install has been cancelled, bail
      // if we don't bail early, then calling the handler will 
      // segfault.
      if (!this->IsInstalling(card))
         return;

      return handler(current, expected);
   }
   );

   // set up the install handler
   CompletionHandler installHandler(
   [this, card, handler {std::move(onCompleted)} ](int httpCode, std::string body)
   {
      if (!this->IsInstalling(card))
      {
         Uninstall(card);
         return;
      }

      if (!(httpCode == 200) && 
          !(httpCode == 302))
         Uninstall(card);
          
      
      // let the caller handle this
      handler(httpCode, body);

      // get rid of the cached response
      this->mResponseMap.erase(card->GetRepoID());
   });

   // download the model
   try 
   {
      // save the metadata
      wxLogDebug("saving modelcard for %s \n", card->GetRepoID());
      card->SerializeToFile(
         audacity::ToUTF8(wxFileName(GetRepoDir(card), METADATA_FILENAME).GetFullPath())
      );

      wxLogDebug("downloading model for %s \n",card->GetRepoID());

      network_manager::ResponsePtr response = DownloadModel(card, progressHandler, installHandler);

      // add response to a temporary map (in case of cancellation)
      mResponseMap[card->GetRepoID()] = response;
   }
   catch (const char *msg)
   {
      wxLogError(msg);
      return;
   }
}

void DeepModelManager::Uninstall(ModelCardHolder card)
{
   bool success = true;
   success &= wxRemoveFile(wxFileName(GetRepoDir(card), MODEL_FILENAME).GetFullPath());
   success &= wxRemoveFile(wxFileName(GetRepoDir(card), METADATA_FILENAME).GetFullPath());

   if (!success)
      throw ModelManagerException(XO("An error occurred while uninstalling the model."));
   
   wxRmDir(wxFileName(GetRepoDir(card)).GetFullPath());
   
}

bool DeepModelManager::IsInstalling(ModelCardHolder card) const
{
   return mResponseMap.find(card->GetRepoID()) != mResponseMap.end();
}

void DeepModelManager::CancelInstall(ModelCardHolder card)
{
   if (!IsInstalling(card))
    {
      // should never really reach here (can't cancel an install that's not ongoing)
      wxASSERT(false);
      return;
    }   
   else
   {
      const std::string repoid = card->GetRepoID();
      network_manager::ResponsePtr response = mResponseMap[repoid];
      response->abort();
      mResponseMap.erase(repoid);
   }
}

ModelCardCollection DeepModelManager::GetCards(const std::string &effect_type) const
{
   ModelCardFilter filterId([&effect_type](const ModelCard &card)
   {
      return card.effect_type() == effect_type;
   });
   return mCards.Filter(filterId);
}

void DeepModelManager::FetchModelCards(CardFetchedCallback onCardFetched, CardFetchProgressCallback onProgress)
{
   // add the card to our collection before passing to the callback
   CardFetchedCallback onCardFetchedWrapper = [this, onCardFetched = std::move(onCardFetched)]
   (bool success, ModelCardHolder card)
   {
      if (success)
      {
         // validate it and insert it into collection
         try
         {
            std::lock_guard<std::mutex> guard(mCardMutex);
               this->mCards.Insert(card);
         }
         catch (const InvalidModelCardDocument &e)
         {
            // TODO: GetRepoID should be a no-throw if we're going to use it here
            wxLogError("Failed to validate metadata.json for repo %s ;\n %s", 
                     card->GetRepoID(), e.what());
         }
      }
      // pass it on
      onCardFetched(success, card);
   };

   // get the repos 
   RepoListFetchedCallback onRepoListFetched = [this, onCardFetchedWrapper = std::move(onCardFetchedWrapper),
                                                onProgress = std::move(onProgress)]
   (bool success, RepoIDList ids)
   {
      if (success)
      {
         int64_t total = ids.size();
         for (int64_t idx = 0;  idx < total ; idx++)
         {
            onProgress(idx+1, total);
            std::string repoId = ids[idx];
            this->FetchCard(repoId, onCardFetchedWrapper);
         }
      }
   };

   FetchRepos(onRepoListFetched);
}

std::string DeepModelManager::GetRootURL(const std::string &repoID) const
{
   return "https://huggingface.co/"+repoID+"/resolve/main/";
}

std::string DeepModelManager::GetFileURL(const std::string &repoID, const std::string &filePath) const
{
   return GetRootURL(repoID)+filePath;
}

void DeepModelManager::FetchLocalCards(CardFetchedCallback onCardFetched)
{
   FilePaths pathList;
   FilePaths modelFiles; 

   // NOTE: maybe we should support multiple search paths? 
   pathList.push_back(DeepModel::DLModelsDir());

   FileNames::FindFilesInPathList(wxT(MODEL_FILENAME), pathList, 
                                 modelFiles, wxDIR_FILES | wxDIR_DIRS);

   for (const auto &modelFile : modelFiles)
   {
      wxFileName modelPath(modelFile);
      wxFileName cardPath(modelPath.GetFullPath());

      cardPath.SetFullName(METADATA_FILENAME);

      if (cardPath.FileExists() && modelPath.FileExists())
      {
         ModelCardHolder card = std::make_shared<ModelCard>();
         bool success = NewCardFromLocal(card, audacity::ToUTF8(cardPath.GetFullPath()));
         onCardFetched(success, card);
      }
   }
}

void DeepModelManager::FetchRepos(RepoListFetchedCallback onReposFetched)
{
   // NOTE: the url below asks for all repos in huggingface
   // that contain the tag "audacity". 
   // however, it might be better for us to keep a curated list of 
   // models which we show to the user, and allow the user to explore huggingface
   // on their own for more repos
   // std::string query = mAPIEndpoint + "models?filter=audacity";
   std::string query = GetFileURL("hugggof/audacity-models", "models.json");

   // TODO: handle exception in main thread. try using a broken url?
   CompletionHandler handler = 
   [query, onReposFetched = std::move(onReposFetched)]
   (int httpCode, std::string body)
   {
      RepoIDList repos;
      if (httpCode != 200)
      {
         wxLogError("GET request failed for url %s. Error code: %d", 
                     query, httpCode);
         onReposFetched(false, repos); 
      }
      else
      {
         bool success = true;

         Doc reposDoc;
         try
         {
            reposDoc = parsers::ParseString(body);
         }
         catch (const InvalidModelCardDocument &e)
         {
            wxLogError("error parsing JSON reponse for fetching repos");
            wxLogError(e.what());
            success = false;
         }

         if (success && reposDoc.IsArray())
         {
            for (auto itr = reposDoc.Begin(); itr != reposDoc.End(); ++itr)
            {
               wxLogDebug("Found repo with name %s", itr->GetString());
               repos.emplace_back(itr->GetString());
            }
         }
         else 
            success = false;

         onReposFetched(success, repos);
      }
   };

   doGet(query, handler);
}

void DeepModelManager::FetchCard(const std::string &repoID, CardFetchedCallback onCardFetched)
{ 
   const std::string modelCardUrl = GetFileURL(repoID, METADATA_FILENAME);
   wxLogMessage("Fetching model card from %s", modelCardUrl);

   // TODO: how do you handle an exception inside a thread, like this one? 
   CompletionHandler completionHandler = 
   [this, modelCardUrl, repoID, onCardFetched = std::move(onCardFetched)]
   (int httpCode, std::string body)
   { 
      ModelCardHolder card = std::make_shared<ModelCard>();
      if (httpCode != 200)
      {
         wxLogError(
            wxString("GET request failed for url %s. Error code: %d")
                  .Format(wxString(modelCardUrl), httpCode)
         );
         onCardFetched(false, card);
      }
      else
      {
         wxLogMessage(
            wxString("GET request succeeded for url %s. Error code: %d")
                  .Format(wxString(modelCardUrl), httpCode)
         );
         bool success = NewCardFromHuggingFace(card, body, repoID);
         onCardFetched(success, card);
      }

   };

   doGet(modelCardUrl, completionHandler);
}

void DeepModelManager::FetchModelSize(ModelCardHolder card, ModelSizeCallback onModelSizeRetrieved) const
{
   if (card->local())
   {
      FilePath repoDir = GetRepoDir(card);
      wxFileName modelPath = wxFileName(repoDir, MODEL_FILENAME);

      if (modelPath.FileExists())
      {
         size_t model_size = GetFileSize(modelPath);
         card->model_size(model_size);
         onModelSizeRetrieved(model_size);
      }
   }
   else
   {
      using namespace network_manager;

      const std::string modelUrl = GetFileURL(card->GetRepoID(), MODEL_FILENAME);

      Request request(modelUrl);

      NetworkManager &manager = NetworkManager::GetInstance();
      ResponsePtr response = manager.doHead(request);

      response->setRequestFinishedCallback(
         [card, response, handler = std::move(onModelSizeRetrieved)](IResponse*)
         {
            if ((response->getHTTPCode() != 200) 
                  && (response->getHTTPCode() != 302))
               return;

            if (response->hasHeader("x-linked-size"))
            {
               std::string length = response->getHeader("x-linked-size");

               size_t modelSize = std::stoi(length, nullptr, 10);
               handler(modelSize);

               card->model_size(modelSize);
            }

         }
      );
   }
}

ModelCardHolder DeepModelManager::GetEmptyCard() const
{
   ModelCardHolder card = std::make_shared<ModelCard>();
   return card;
}

bool DeepModelManager::NewCardFromHuggingFace(ModelCardHolder card, const std::string &jsonBody, const std::string &repoID)
{
   wxStringTokenizer st(wxString(repoID), wxT("/"));
   const std::string cardAuthor = audacity::ToUTF8(st.GetNextToken());
   const std::string cardName = audacity::ToUTF8(st.GetNextToken());
   
   try
   {
      Doc doc = parsers::ParseString(jsonBody);
      card->Deserialize(doc, mModelCardSchema);
      (*card).name(cardName)
             .author(cardAuthor)
             .local(false)
             .local_path(audacity::ToUTF8(GetRepoDir(card)));

      return true;
   }
   catch (const InvalidModelCardDocument &e)
   {
      wxLogError(wxString(e.what()));
      return false;
   }
   catch (const char *msg)
   { 
      wxLogError(wxString(msg));
      wxASSERT(false);
      return false;
   }
}

bool DeepModelManager::NewCardFromLocal(ModelCardHolder card, const std::string &filePath)
{
   try
   {
      std::string localPath = audacity::ToUTF8(wxFileName(wxString(filePath)).GetPath());
      card->DeserializeFromFile(filePath, mModelCardSchema);
      (*card).local(true)
             .local_path(localPath);
      return true;
   }
   catch (const InvalidModelCardDocument &e)
   {
      return false;
   }
}

network_manager::ResponsePtr DeepModelManager::DownloadModel
(ModelCardHolder card, ProgressCallback onProgress, CompletionHandler onCompleted)
{
   using namespace network_manager;

   // TODO: this is not raising an error for an invalid URL 
   // try adding a double slash anywhere to break it. 
   // the reason is because huggingface returns 200s saying "Not Found"
   const std::string url = GetFileURL(card->GetRepoID(), MODEL_FILENAME);
   
   wxLogDebug("downloading from %s", url);

   Request request(url);

   // send request
   NetworkManager &manager = NetworkManager::GetInstance();
   ResponsePtr response = manager.doGet(request);

   // open a file to write the model to 
   const std::string repoId = card->GetRepoID(); 
   wxString path = wxFileName(this->GetRepoDir(card), MODEL_FILENAME).GetFullPath();
   std::shared_ptr<wxFile> file = std::make_shared<wxFile>(path, wxFile::write);

   // set callback for download progress
   if (onProgress)
      response->setDownloadProgressCallback(onProgress);

   // completion handler
   response->setRequestFinishedCallback(
      [response, handler = std::move(onCompleted)](IResponse*) 
      {
         const std::string responseData = response->readAll<std::string>();

         if (handler)
            handler(response->getHTTPCode(), responseData);
      }
   );

   // write to file here
   response->setOnDataReceivedCallback(
      [this, response, card, file](IResponse*) 
      {
         // abort
         if (!this->IsInstalling(card))
         {
            Uninstall(card);
            return;
         }

         // only attempt save if request succeeded
         int httpCode = response->getHTTPCode();
         if ((httpCode == 200) || (httpCode == 302))
         {
            const std::string responseData = response->readAll<std::string>();
            size_t bytesWritten = file->Write(responseData.c_str(), responseData.size());

            if (bytesWritten != responseData.size())
            {
               Uninstall(card);
            }
         }
      }
   );

   return response;

}

network_manager::ResponsePtr DeepModelManager::doGet
(std::string url, CompletionHandler completionHandler, ProgressCallback onProgress)
{
   using namespace network_manager;

   Request request(url);

   NetworkManager &manager = NetworkManager::GetInstance();
   ResponsePtr response = manager.doGet(request);

   // set callback for download progress
   if (onProgress)
      response->setDownloadProgressCallback(onProgress);

   response->setRequestFinishedCallback(
      [response, handler = std::move(completionHandler)](IResponse*) {
         const std::string responseData = response->readAll<std::string>();

         if (handler)
            handler(response->getHTTPCode(), responseData);
      });

   return response;
}
