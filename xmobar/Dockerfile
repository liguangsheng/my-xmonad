FROM haskell:8 AS build-stage

# copy configuration
RUN mkdir -p /root/.config/xmobar
WORKDIR /root/.config/xmobar

RUN apt-get update && apt-get install -y libx11-dev libxft-dev libxinerama-dev libxrandr-dev libxss-dev \
libasound2-dev libiw-dev libxpm-dev
RUN git clone --depth 1 --single-branch --branch 0.44.2 https://codeberg.org/xmobar/xmobar /root/.config/xmobar/xmobar-git

RUN stack setup
RUN stack init
RUN echo '\n\
extra-deps: \n\
  - netlink-1.1.1.0 \n\
  - iwlib-0.1.0 \n'\
>> /root/.config/xmobar/stack.yaml
RUN cat /root/.config/xmobar/stack.yaml
RUN stack install --flag xmobar:all_extensions

COPY ./build-for-docker /root/.config/xmobar/build
COPY ./xmobar.hs /root/.config/xmobar/xmobar.hs

# build xmobar cache
# xmobar recompile will continue run itself, this will error
RUN timeout 10s xmobar --recompile -v; exit 0

FROM scratch AS export-stage
COPY --from=build-stage /root/.config/xmobar/xmobar-x86_64-linux .
COPY --from=build-stage /root/.local/bin/xmobar .
