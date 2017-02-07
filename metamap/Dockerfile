FROM python:2.7.12
MAINTAINER  will
RUN apt-get -qq  update && apt-get install -y git mysql-client libsasl2-dev python-dev libldap2-dev libssl-dev 
COPY /new_requiremetns.txt /requires.txt
RUN pip --no-cache-dir install -r /requires.txt \
   && apt-get clean 
COPY /restart_metamap_docker.sh /restart_metamap_docker.sh
COPY /entrypoint.sh /entrypoint.sh
ENTRYPOINT ["bash", "/entrypoint.sh"]
EXPOSE 8088
