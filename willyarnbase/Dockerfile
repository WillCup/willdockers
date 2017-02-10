FROM centos:7
MAINTAINER  will
COPY /jdk1.8.0_60 /usr/local/java
ADD /hadoop-2.7.1.tar.gz /etc/hadoop/
ENV JAVA_HOME /usr/local/java
ENV PATH $JAVA_HOME/bin:$PATH
ENV USER root
RUN yum install -y which
COPY /entrypoint.sh /entrypoint.sh
ENTRYPOINT ["bash", "/entrypoint.sh"," resourcemanager"] 
