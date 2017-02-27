FROM centos:7 
MAINTAINER  will
COPY /jdk1.8.0_60 /usr/local/java
ADD /hadoop-2.7.1.tar.gz /etc/hadoop/
VOLUME ["/etc/hadoop/conf"]
ENV JAVA_HOME /usr/local/java
ENV PATH=$JAVA_HOME/bin:$PATH
ENV USER root
COPY /entrypoint.sh /entrypoint.sh
COPY /installr.r /installr.r
RUN chmod a+x /entrypoint.sh \
	&& yum install -y -q which --nogpgcheck 
RUN rpm -Uvh https://mirrors.tuna.tsinghua.edu.cn/epel//7/x86_64/e/epel-release-7-9.noarch.rpm \
        && yum update -y -q && yum install -y -q R --nogpgcheck && yum install -y -q R-littler --nogpgcheck \
	&& chmod a+x /installr.r && ./installr.r
#	&& wget http://mirrors.hust.edu.cn/apache/hadoop/common/hadoop-2.7.1/hadoop-2.7.1.tar.gz \
#	&& tar -zxvf hadoop-2.7.1.tar.gz \
#	&& echo "done"
ENTRYPOINT ["/entrypoint.sh", "resourcemanager"] 
