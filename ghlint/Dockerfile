FROM debian:stable-slim

RUN apt-get -y update \
 && apt-get install -y busybox \
 && rm -rf /var/lib/apt/lists/* \
 && ln -s /bin/busybox /bin/wget

RUN wget -qO - https://api.github.com/repos/ndmitchell/hlint/releases/latest \
  | grep "browser_download_url.*linux.tar.gz" | cut -d : -f 2,3 \
  | tr -d "\"" \
  | wget -q $(cat /dev/stdin) \
  && mkdir hlint \
  && tar xvf *.tar.gz -C hlint --strip-components 1 \
  && rm -rvf *.tar.gz

COPY ghlint /usr/local/bin
RUN chmod +x /usr/local/bin/ghlint

WORKDIR /data
VOLUME ["/data"]

ENTRYPOINT ["ghlint", "/hlint/hlint"]

