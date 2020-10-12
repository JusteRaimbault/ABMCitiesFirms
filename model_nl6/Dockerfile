# must use openmole docker and not comses/docker-netlogo to have java > 8
# ! do not need netlogo
FROM openmole/openmole:latest

RUN mkdir -p /data/outputs

RUN mkdir -p /data/model

COPY . /data/model

WORKDIR /data/model

CMD ["/data/model/run.sh"]
