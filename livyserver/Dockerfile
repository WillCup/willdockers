FROM tobilg/livy:0.3.0
MAINTAINER willcup <willcup@163.com>

# packages
RUN rm -rf /usr/lib/python2.7/site-packages /usr/local/lib/python2.7/site-packages && \
     apt-get update && apt-get install -yq --no-install-recommends --force-yes python2.7 && \
    cp /usr/bin/python2.7 /usr/bin/python && \ 
    rm -rf /var/lib/apt/lsits/*
    

