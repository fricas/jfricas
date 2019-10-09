# **************************************************************************
# Purpose ...: jfricas (https://github.com/fricas/jfricas.pip)
# Build .... : docker build -t jfricas:latest .
# Test ..... : docker run -ti --rm --network=host 
#                --env DISPLAY=:0 nilqed/jfricas:latest jupyter notebook
# Version .. : 0.2.17 -- 17-SEP-2019
# OS ....... : ubuntu:latest
# FriCAS ... : fricas:latest 
# **************************************************************************

FROM nilqed/fricas

LABEL fricas-devel <fricas-devel@googlegroups.com>

# ENV DEBIAN_FRONTEND noninteractive
# discouraged: see https://docs.docker.com/engine/faq/

ENV JFRICAS_GITHUB https://github.com/nilqed/jfricas.pip
ENV JFRICAS_VERSION 0.2.17

# ======================================
# Get packages and install missing tools
# ======================================

RUN apt-get update && apt-get install -y -q \
    python3.6 \
    python3-pip \
    cl-asdf \
    cl-hunchentoot \
    firefox \
    gnuplot



# ===============================
# Install jfricas.pip from Github
# ===============================

RUN cd /root && \
    pip3 install jupyter && \
    git clone $JFRICAS_GITHUB && \
    cd jfricas.pip && \
    pip3 install . && \
    python3 -m jfricas.install_kernel && \
    cd /root







    
    
    

