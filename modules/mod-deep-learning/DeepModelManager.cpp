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
#include <wx/app.h>

using namespace audacity;

#define MODEL_FILENAME "model.pt"
#define METADATA_FILENAME "metadata.json"

namespace {
   size_t GetFileSize(wxFileName path)
   {
      wxFile file = wxFile(path.GetFullPath(), wxFile::read);
      if (!file.IsOpened())
         throw ModelManagerException{ XO("Failed to open file %s").Format(path.GetFullPath()) };

      return static_cast<size_t>(file.Length());
   }
}
 
DeepModelManager::DeepModelManager() :
   mAPIEndpoint("https://huggingface.co/api/"), 
   mThreadPool(std::make_unique<ThreadPool>(std::thread::hardware_concurrency()))
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
      throw ModelManagerException{ XO("Model is not loaded.") };

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

void DeepModelManager::Install(ModelCardHolder card, ProgressCallback onProgress, InstallCompletionHandler onCompleted)
{ 
   if (IsInstalled(card))
      return ;

   // save the metadata
   wxLogDebug("saving modelcard for %s \n", card->GetRepoID());
   card->SerializeToFile(
      audacity::ToUTF8(wxFileName(GetRepoDir(card), METADATA_FILENAME).GetFullPath())
   );

   // download the model
   wxLogDebug("downloading model for %s \n",card->GetRepoID());
   network_manager::ResponsePtr response = DownloadModel(card, onProgress, onCompleted);

   // add response to a temporary map (in case of cancellation)
   std::lock_guard<std::mutex> guard(mResponseMutex);
      mResponseMap[card->GetRepoID()] = response;
}

void DeepModelManager::Uninstall(ModelCardHolder card)
{
   bool success = true;
   auto modelFile = wxFileName(GetRepoDir(card), MODEL_FILENAME);
   auto cardFile = wxFileName(GetRepoDir(card), METADATA_FILENAME);

   if (modelFile.FileExists())
      success &= wxRemoveFile(modelFile.GetFullPath());
   if (cardFile.FileExists())
      success &= wxRemoveFile(cardFile.GetFullPath());

   if (!success)
   {
      wxTheApp->CallAfter([](){
         throw ModelManagerException{ XO("An error occurred while uninstalling the model.") };
      });
   }
   
   wxRmDir(wxFileName(GetRepoDir(card)).GetFullPath());
   
}

bool DeepModelManager::IsInstalling(ModelCardHolder card)
{
   std::lock_guard<std::mutex> guard(mResponseMutex);
      return mResponseMap.find(card->GetRepoID()) != mResponseMap.end();
}

void DeepModelManager::CancelInstall(ModelCardHolder card, bool error)
{
   const std::string repoid = card->GetRepoID();
   
   std::lock_guard<std::mutex> guard(mResponseMutex);
      if (mResponseMap.find(repoid) != mResponseMap.end())
      {
         network_manager::ResponsePtr response =  mResponseMap[repoid];
         response->abort();
         mResponseMap.erase(repoid);
      }

   Uninstall(card);

   if (error)
   {
      auto msg = XO("An error occurred while installing the model with repoID %s.").Format(repoid);
      wxLogError(msg.Translation());
      throw ModelManagerException{ msg };
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

std::string DeepModelManager::GetRootURL(const std::string &repoID) const
{
   return "https://huggingface.co/"+repoID+"/resolve/main/";
}

std::string DeepModelManager::GetFileURL(const std::string &repoID, const std::string &filePath) const
{
   return GetRootURL(repoID)+filePath;
}

RepoIDList DeepModelManager::FetchRepos() const
{
   // NOTE: the url below asks for all repos in huggingface
   // that contain the tag "audacity". 
   // however, it might be better for us to keep a curated list of 
   // models which we show to the user, and allow the user to explore huggingface
   // on their own for more repos
   // std::string query = mAPIEndpoint + "models?filter=audacity";
   std::string query = GetFileURL("hugggof/audacity-models", "models.json");
   wxLogDebug("Fetching available repositories from URL: %s", query);

   auto response = doGet(query);
   const std::string responseData = response->readAll<std::string>();
   int httpCode = response->getHTTPCode();
   wxLogDebug("Got reponse with HTTP code: %d", httpCode);

   RepoIDList repos;
   if (httpCode != 200)
   {
      auto msg = XO("Failed to fetch available repositories from Huggingface.\n"
                   "HTTP code: %d").Format(httpCode);
      wxLogError(msg.Translation());
      throw ModelManagerException{ msg };
   }

   Doc reposDoc;
   try
   {
      // could throw InvalidModelCardDocument if the string isn't valid json
      reposDoc = parsers::ParseString(responseData);

      // if we didn't get an array, throw
      if (!reposDoc.IsArray())
         throw InvalidModelCardDocument(XO("Provided JSON document is not an Array."));

   }
   catch (const InvalidModelCardDocument &e)
   {
      auto msg = XO("Error parsing JSON reponse for fetching a list of deep model repositories.");
      wxLogError(msg.Translation());
      wxLogError("ModelCard error: %s", wxString(e.what()));
      throw ModelManagerException{ msg };
   }

   // iterate through list of repos and add them to our list
   for (auto itr = reposDoc.Begin(); itr != reposDoc.End(); ++itr)
   {
      wxLogDebug("Found repo with name %s", itr->GetString());
      repos.emplace_back(itr->GetString());
   }

   return repos;
}

void DeepModelManager::FetchLocalCards(CardFetchedCallback onCardFetched)
{
   wxLogDebug("Fetching local deep model cards");
   mThreadPool->enqueue([onCardFetched, this]()
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
            // may throw, so just catch and log
            try
            {
               ModelCardHolder card = NewCardFromLocal(audacity::ToUTF8(cardPath.GetFullPath()));

               // call the callback from the main UI thread
               wxTheApp->CallAfter([onCardFetched, card]()
               {
                  onCardFetched(card);
               });
            }
            catch (const InvalidModelCardDocument &e)
            { 
               wxLogError(wxString(e.what())); 
            }
         }
      }
   });
}

void DeepModelManager::FetchHuggingFaceCards(CardFetchedCallback onCardFetched)
{
   // fetch all cards from huggingface
   mThreadPool->enqueue([onCardFetched, this]()
   {
      // fetch the list of repos
      RepoIDList repos = FetchRepos();

      // fetch each model card in a separate thread
      // when this pool goes out of scope, all threads will join. 
      auto pool = ThreadPool(std::thread::hardware_concurrency());

      int64_t total = repos.size();
      for (int64_t idx = 0;  idx < total ; idx++)
      {
         pool.enqueue(
         [idx, total, this, onCardFetched, &repos]()
         {
            std::string repoId = repos[idx];
            AddHuggingFaceCard(repoId, onCardFetched, true);
         });
      }
   });

   // all threads will join when the pool gets destroyed here
}

void DeepModelManager::AddHuggingFaceCard(const std::string &repoID, CardFetchedCallback onCardFetched, bool block)
{
   auto worker = [this, repoID, onCardFetched = std::move(onCardFetched)]()
   {
      // need to catch any errors here and just log them
      try { 
         ModelCardHolder card = this->FetchCard(repoID); 

         // validate and insert into the collection.
         std::lock_guard<std::mutex> guard(mCardMutex);
            this->mCards.Insert(card);

         // call the callback from the main UI thread
         wxTheApp->CallAfter([onCardFetched = std::move(onCardFetched), card]()
         {
            onCardFetched(card);
         });
      }
      catch (const ModelManagerException &e)
      { 
         wxLogError(wxString(e.what())); 
         }
      catch (const InvalidModelCardDocument &e)
      { 
         wxLogError(wxString(e.what())); 
      }
   };

   if (!block)
      mThreadPool->enqueue(worker);
   else
      worker();
}

ModelCardHolder DeepModelManager::FetchCard(const std::string &repoID) const
{ 
   const std::string query = GetFileURL(repoID, METADATA_FILENAME);
   wxLogDebug("Fetching model card from %s", query);

   auto response = doGet(query);
   const std::string responseData = response->readAll<std::string>();
   int httpCode = response->getHTTPCode();
   wxLogDebug("Got reponse with HTTP code: %d", httpCode);

   if (httpCode != 200)
   {
      auto msg = XO("Failed to fetch model card with URL %s from Huggingface.\n"
                   "HTTP code: %d").Format(query, httpCode);
      wxLogError(msg.Translation());
      throw ModelManagerException{ msg };;
   }

   // may throw InvalidModelCardDocument if parsing fails
   ModelCardHolder card = NewCardFromHuggingFace(responseData, repoID);

   // now, fill out the size
   FetchModelSize(card);

   return card;
}

void DeepModelManager::FetchModelSize(ModelCardHolder card) const
{
   if (card->local())
   {
      FilePath repoDir = GetRepoDir(card);
      wxFileName modelPath = wxFileName(repoDir, MODEL_FILENAME);

      if (modelPath.FileExists())
      {
         size_t model_size = GetFileSize(modelPath);
         card->model_size(model_size);
      }
      else
      {
         // throw if the model card is local but the modelPath doesn't exist
         auto msg = XO("Model card is local but the model file %s doesn't exist.").Format(modelPath.GetFullPath());
         wxLogError(msg.Translation());
         throw ModelManagerException{ msg };
      }
   }
   else
   {
      using namespace network_manager;
      NetworkManager &manager = NetworkManager::GetInstance();

      const std::string modelUrl = GetFileURL(card->GetRepoID(), MODEL_FILENAME);
      Request request(modelUrl);
      request.setBlocking(true);
      ResponsePtr response = manager.doHead(request);

      bool success = true;
      if ((response->getHTTPCode() != 200) && (response->getHTTPCode() != 302))
         success = false;

      if (response->hasHeader("x-linked-size"))
      {
         std::string length = response->getHeader("x-linked-size");
         card->model_size(std::stoi(length, nullptr, 10));
      }
      else // failed fetching model size
         success = false;

      // throw if something went wrong during the HEAD request
      if (!success)
      {
         auto msg = XO("Failed to fetch model size for model card with repo ID %s.\n"
                      "HTTP code: %d").Format(card->GetRepoID(), response->getHTTPCode());
         wxLogError(msg.Translation());
         throw ModelManagerException{ msg };
      }
   }
}

ModelCardHolder DeepModelManager::GetEmptyCard() const
{
   ModelCardHolder card = std::make_shared<ModelCard>();
   return card;
}

ModelCardHolder DeepModelManager::NewCardFromHuggingFace(const std::string &jsonBody, const std::string &repoID) const
{
   ModelCardHolder card = std::make_shared<ModelCard>();

   wxStringTokenizer st(wxString(repoID), wxT("/"));
   const std::string cardAuthor = audacity::ToUTF8(st.GetNextToken());
   const std::string cardName = audacity::ToUTF8(st.GetNextToken());
   
   Doc doc = parsers::ParseString(jsonBody);
   card->Deserialize(doc, mModelCardSchema);
   (*card).name(cardName)
            .author(cardAuthor)
            .local(false)
            .local_path(audacity::ToUTF8(GetRepoDir(card)));

   return card;
}

ModelCardHolder DeepModelManager::NewCardFromLocal(const std::string &filePath)
{
   ModelCardHolder card = std::make_shared<ModelCard>();

   std::string localPath = audacity::ToUTF8(wxFileName(wxString(filePath)).GetPath());
   card->DeserializeFromFile(filePath, mModelCardSchema);
   (*card).local(true)
            .local_path(localPath);

   return card;
}

network_manager::ResponsePtr DeepModelManager::DownloadModel
(ModelCardHolder card, ProgressCallback onProgress, InstallCompletionHandler onCompleted)
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
      [response, onCompleted = std::move(onCompleted), 
       this, card](IResponse*) 
   {
      // bail if the response was aborted
      if (!this->IsInstalling(card))
      {
         CancelInstall(card, true);
         return;
      }

      int httpCode = response->getHTTPCode();
      // bail if the response is invalid
      if (!(httpCode == 200) && 
         !(httpCode == 302))
         CancelInstall(card, true);
      
      {
      std::lock_guard<std::mutex> guard(mResponseMutex);
         this->mResponseMap.erase(card->GetRepoID());
      }

      // call OnCompleted on the UI thread
      if (onCompleted)
         wxTheApp->CallAfter(onCompleted);

   });

   // write to file here
   response->setOnDataReceivedCallback(
      [this, response, card, file](IResponse*) 
      {
         // abort if we've been cancelled
         if (!this->IsInstalling(card))
         {
            wxTheApp->CallAfter([this, card](){
               CancelInstall(card, true);
            });
         }

         // only attempt save if request succeeded
         int httpCode = response->getHTTPCode();
         if ((httpCode == 200) || (httpCode == 302))
         {
            const std::string responseData = response->readAll<std::string>();
            size_t bytesWritten = file->Write(responseData.c_str(), responseData.size());

            if (bytesWritten != responseData.size())
               wxTheApp->CallAfter([this, card](){
                  CancelInstall(card, true);
               });
         }
      }
   );

   return response;

}

network_manager::ResponsePtr DeepModelManager::doGet
(std::string url) const
{
   using namespace network_manager;

   Request request(url);

   request.setBlocking(true);

   NetworkManager &manager = NetworkManager::GetInstance();
   ResponsePtr response = manager.doGet(request);

   return response;
}
