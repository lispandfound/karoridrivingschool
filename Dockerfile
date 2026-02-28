FROM docker.io/library/golang:1.26-bookworm AS go-builder
RUN apt-get update && apt-get install -y git libsqlite3-dev libfuse-dev pkg-config

RUN git clone https://git.sr.ht/~marcc/filed/ /filed-src \
    && cd /filed-src \
    && go build -o /usr/local/bin/filed . \
    && go build -o /usr/local/bin/filed-launch ./cmd/filed-launch
# --- Stage 2: Your original Haskell builder ---
FROM docker.io/library/haskell:9.12 AS builder
WORKDIR /build
RUN cabal update
COPY karoridrivingschool.cabal .
RUN cabal build --only-dependencies -j
COPY CHANGELOG.md LICENSE .

COPY app app
RUN cabal install --install-method=copy --installdir=/usr/local/bin

FROM docker.io/library/debian:bookworm-slim

RUN apt-get update && apt-get install -y \
    msmtp \
    fuse3 \
    ca-certificates \
    libsqlite3-0 \
    libgmp10 \
    && rm -rf /var/lib/apt/lists/*

COPY --from=go-builder /usr/local/bin/filed /usr/local/bin/
COPY --from=go-builder /usr/local/bin/filed-launch /usr/local/bin/
COPY --from=builder /usr/local/bin/karoridrivingschool /usr/local/bin/

EXPOSE 3000

RUN groupadd -g 1001 appuser && \
    useradd -r -u 1001 -g 1001 appuser
RUN mkdir -p /jobs && chown -R appuser:appuser /jobs
RUN mkdir -p /db && chown -R appuser:appuser /db && chmod 700 /db

COPY scripts/send-email.sh /usr/bin/send-email.sh
RUN chmod +x /usr/bin/send-email.sh

COPY scripts/cleanup-email.sh /usr/bin/cleanup-email.sh
RUN chmod +x /usr/bin/cleanup-email.sh

COPY scripts/entrypoint.sh /entrypoint.sh
RUN chmod +x /entrypoint.sh

COPY ./config/fuse.conf /etc/fuse.conf

RUN mkdir -p /logs && chown -R appuser:appuser /logs
RUN mkdir -p /home/appuser
RUN chown -R appuser:appuser /home/appuser

ENTRYPOINT ["/entrypoint.sh"]
