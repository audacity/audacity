#!/usr/bin/env python3

import hashlib
import json
import os
import subprocess
import time
import zipfile
from urllib.parse import urlparse, urlunparse

# needs to be equal or smaller than the cron
period = 300
outputDir = "share/locale/"


def getS3Url():
    override = os.environ.get("S3_UPLOAD_URL")
    if override:
        return override if override.endswith('/') else override + '/'

    configPath = "src/app/configs/languages.cfg"
    configFile = open(configPath, "r+")
    configJson = json.load(configFile)
    configFile.close()

    rawHttpsUrl = configJson["server_url"]
    parsedHttpsUrl = urlparse(rawHttpsUrl)

    return urlunparse(('s3', *parsedHttpsUrl[1:]))


s3Url = getS3Url()


def processTsFile(prefix, langCode, data):
    print("Processing " + langCode)
    filename = prefix + '_' + langCode
    tsFilePath = outputDir + filename + ".ts"
    qmFilePath = outputDir + filename + ".qm"

    if not os.path.isfile(tsFilePath):
        print(prefix + ' ' + langCode + " skipped (no .ts file — not sufficiently translated on Transifex yet)")
        removed = data.pop(langCode, None) is not None
        return False, removed

    lang_time = int(os.path.getmtime(tsFilePath))
    cur_time = int(time.time())

    # if the file has been updated, update or add entry in details.json
    if (cur_time - lang_time < period) or not os.path.isfile(qmFilePath):
        # generate qm file
        lrelease = subprocess.Popen(
            ['lrelease', tsFilePath, '-qm', qmFilePath])
        lrelease.communicate()

        # get qm file size
        file_size = os.path.getsize(qmFilePath)
        file_size = "%.2f" % (file_size / 1024)

        # compute qm file hash
        file = open(qmFilePath, 'rb')
        hash_file = hashlib.sha1()
        hash_file.update(file.read())
        file.close()

        if langCode not in data:
            data[langCode] = {}
        if prefix not in data[langCode]:
            data[langCode][prefix] = {}

        data[langCode][prefix]["file_name"] = filename + ".qm"
        data[langCode][prefix]["hash"] = str(hash_file.hexdigest())
        data[langCode][prefix]["file_size"] = file_size

        return True, True
    else:
        print(prefix + ' ' + langCode + " not changed")
        return False, False


newDetailsFile = False
translationChanged = False

# read languages.json and store language code and name
langCode_file = open("share/locale/languages.json", "r+")
langCodeNameDict = json.load(langCode_file)  # language code --> props
langCode_file.close()

detailsJson = outputDir + "details.json"
# read details.json or create it
if os.path.isfile(detailsJson):
    json_file = open(outputDir + "details.json", "r+")
    data = json.load(json_file)
    json_file.close()
else:
    newDetailsFile = True
    data = {}
    data["type"] = "Languages"
    data["version"] = "2.0"


translationChanged = newDetailsFile
for lang_code, languageProps in langCodeNameDict.items():
    updateAudacity, detailsChanged = processTsFile("audacity", lang_code, data)
    translationChanged = detailsChanged or translationChanged

    if updateAudacity:
        # create a zip file, compute size, hash, add it to json and save to s3
        zipName = 'locale_' + lang_code + '.zip'
        zipPath = outputDir + zipName
        myzip = zipfile.ZipFile(zipPath, mode='w')
        qmFilePath = outputDir + 'audacity_' + lang_code + ".qm"
        myzip.write(qmFilePath, 'audacity_' + lang_code + ".qm")
        myzip.close()

        # get zip file size
        file_size = os.path.getsize(zipPath)
        file_size = "%.2f" % (file_size / 1024)

        # compute zip file hash
        file = open(zipPath, 'rb')
        hash_file = hashlib.sha1()
        hash_file.update(file.read())
        file.close()

        data[lang_code]["file_name"] = zipName
        data[lang_code]["name"] = languageProps["name"]
        data[lang_code]["hash"] = str(hash_file.hexdigest())
        data[lang_code]["file_size"] = file_size

        push_zip = subprocess.Popen(
            ['s3cmd', 'put', '--acl-public', '--guess-mime-type', zipPath, s3Url + zipName])
        push_zip.communicate()


json_file = open(outputDir + "details.json", "w")
json_file.write(json.dumps(data, sort_keys=True, indent=4))
json_file.close()

if translationChanged:
    push_json = subprocess.Popen(
        ['s3cmd', 'put', '--acl-public', '--guess-mime-type', outputDir + 'details.json', s3Url + 'details.json'])
    push_json.communicate()
