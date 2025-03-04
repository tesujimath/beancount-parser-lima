FROM ghcr.io/pyo3/maturin

ARG PROTOC_VERSION=25.6

RUN \
    curl -L -o protoc.zip https://github.com/protocolbuffers/protobuf/releases/download/v${PROTOC_VERSION}/protoc-${PROTOC_VERSION}-linux-x86_64.zip && \
    unzip -d /usr/local protoc.zip && \
    rm -f protoc.zip
