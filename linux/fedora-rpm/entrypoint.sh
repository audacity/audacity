#!/usr/bin/env bash

if [ ! -d "audacity" ]
then
    git clone https://github.com/audacity/audacity
fi

tar --exclude '.git' -czf /root/rpmbuild/SOURCES/audacity.tar.gz audacity

ls /root/rpmbuild/SOURCES/

rpmbuild -bs /root/rpmbuild/SPEC/audacity.spec

mock -r audacity /root/rpmbuild/SRPMS/*
