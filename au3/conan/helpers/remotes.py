import subprocess
import json

from helpers.conan_environment import get_conan

def list_remotes():
    remotes = subprocess.check_output([get_conan(), 'remote', 'list', '--format', 'json'], stdin=subprocess.DEVNULL).decode('utf-8')
    return json.loads(remotes)


def remove_remote(remote_name:str):
    remotes = list_remotes()
    for remote in remotes:
        if remote['name'] != remote_name:
            continue
        return subprocess.check_call([get_conan(), 'remote', 'remove', remote_name], stdin=subprocess.DEVNULL)


def add_remote(name:str, url:str) -> None:
    remotes = list_remotes()
    for remote in remotes:
        if remote['name'] != name:
            continue
        if remote['url'] != url:
            subprocess.check_call([get_conan(), 'remote', 'update', '--url', name], stdin=subprocess.DEVNULL)
        return

    subprocess.check_call([get_conan(), 'remote', 'add', name, url], stdin=subprocess.DEVNULL)


def validate_remotes():
    old_remotes = 'audacity', 'conan-center-cache', 'audacity-recipes', 'audacity-binaries'

    for remote in old_remotes:
        try:
            remove_remote(remote)
        finally:
            pass

    add_remote('audacity-recipes-conan2', 'https://artifactory.audacityteam.org/artifactory/api/conan/audacity-recipes-conan2')
    add_remote('audacity-binaries-conan2', 'https://artifactory.audacityteam.org/artifactory/api/conan/audacity-binaries-conan2')
