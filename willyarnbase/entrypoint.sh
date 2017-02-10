#!/bin/sh
serv=$1
sh /etc/hadoop/hadoop-2.7.1/sbin/yarn-daemon.sh start $serv
# tailf /etc/hadoop/hadoop-2.7.1/logs/yarn-root-resourcemanager-*.log
exec "$2"
