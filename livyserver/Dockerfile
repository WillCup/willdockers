FROM ubuntu:14.04
MAINTAINER willcup <willcup@163.com>

# Add R list
RUN echo 'deb http://cran.rstudio.com/bin/linux/ubuntu trusty/' | sudo tee -a /etc/apt/sources.list.d/r.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys E084DAB9

# packages
RUN apt-get update && apt-get install -yq --no-install-recommends --force-yes \
    wget \
    git \
    openjdk-7-jdk \
    maven \
    libjansi-java \
    libsvn1 \
    libcurl3 \
    libsasl2-modules 

# Overall ENV vars
ENV SPARK_VERSION 1.6.1
ENV LIVY_BUILD_VERSION livy-server-0.3.0-SNAPSHOT

# Set install path for Livy
ENV LIVY_APP_PATH /apps/$LIVY_BUILD_VERSION

# Set build path for Livy
ENV LIVY_BUILD_PATH /apps/build/livy

# Set Hadoop config directory
ENV HADOOP_CONF_DIR /etc/hadoop/conf

# Set Spark home directory
ENV SPARK_HOME /usr/local/spark

# Spark ENV vars
ENV SPARK_VERSION_STRING spark-$SPARK_VERSION-bin-hadoop2.6
ENV SPARK_DOWNLOAD_URL http://d3kbcqa49mib13.cloudfront.net/$SPARK_VERSION_STRING.tgz

# Download and unzip Spark
RUN wget $SPARK_DOWNLOAD_URL && \
    mkdir -p $SPARK_HOME && \
    tar xvf $SPARK_VERSION_STRING.tgz -C /tmp && \
    cp -rf /tmp/$SPARK_VERSION_STRING/* $SPARK_HOME && \
    rm -rf -- /tmp/$SPARK_VERSION_STRING && \
    rm spark-$SPARK_VERSION-bin-hadoop2.6.tgz

# Clone Livy repository
RUN mkdir -p /apps/build && \
    cd /apps/build && \
	git clone https://github.com/cloudera/livy.git && \
	cd $LIVY_BUILD_PATH && \
    mvn -DskipTests -Dspark.version=$SPARK_VERSION clean package && \
    unzip $LIVY_BUILD_PATH/assembly/target/$LIVY_BUILD_VERSION.zip -d /apps && \
    rm -rf $LIVY_BUILD_PATH && \
	mkdir -p $LIVY_APP_PATH/upload 

RUN rm -rf /var/lib/apt/lists/* && rm -rf /usr/lib/python2.7/site-packages /usr/local/lib/python2.7/site-packages && apt-get install -yq --no-install-recommends --force-yes   python2.7 && cp /usr/bin/python2.7 /usr/bin/python

# Add custom files, set permissions
ADD entrypoint.sh .

RUN chmod +x entrypoint.sh

# Expose port
EXPOSE 8998

ENTRYPOINT ["/entrypoint.sh"]
