#! /bin/bash
git clone https://github.com/WillCup/metamap.git
cd  metamap/metamap_django/
git checkout $BRANCH
bash /restart_metamap_docker.sh
tailf /tmp/metamap_gunicorn_error.log
