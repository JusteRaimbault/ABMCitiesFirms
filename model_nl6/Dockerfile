# must use openmole docker and not comses/docker-netlogo to have java > 8
# ! do not need netlogo
FROM openmole/openmole:latest

RUN mkdir -p /data/outputs

# using /data dir is not a good idea as mounted by dataslots on dafni
# -> chge in dafni behavior compared to saltelli? it does not use /data
#RUN mkdir -p /data/model

COPY . /root/model

#WORKDIR /root/model

# why do simple command fail? ("ls dir" e.g.): no interpreter?; sh -c ls works - command shouldn't be in quote, ls dir works
# in run.sh, the shebang provides it
# docker inspect openmole/openmole:latest: cmd is sh -c openmole-docker (with #(noop): ?)
CMD ["/root/model/run.sh"]
