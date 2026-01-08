# ベースイメージとして Ubuntu 22.04 を使用
FROM ubuntu:22.04 AS base

# 環境変数の設定
ARG NODE_MAJOR=22
ENV DEBIAN_FRONTEND=noninteractive \
    LANG=ja_JP.UTF-8 \
    LC_ALL=ja_JP.UTF-8 \
    LC_CTYPE=ja_JP.UTF-8 \
    NODE_VER=$NODE_MAJOR

# ユーザーの設定
ARG USERNAME=developer
ARG USER_UID=1000
ARG USER_GID=$USER_UID

RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
    && apt-get update \
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

# ロケールのセットアップ
RUN apt-get update && apt-get install -y \
    language-pack-ja-base \
    language-pack-ja \
    && update-locale LANG=ja_JP.UTF-8 LANGUAGE=ja_JP:ja \
    && rm -rf /var/lib/apt/lists/*

# 基本的なパッケージのインストール
 RUN apt-get update && \
     apt-get install -y \
            build-essential \
            zip \
            unzip \
            git \
            curl \
            wget \
            vim \
            tmux \
            xz-utils \
            && apt-get clean \
            && rm -rf /var/lib/apt/lists/*

# Nixのインストール
ENV NIX_INSTALL_DIR=/nix
RUN mkdir -m 0755 $NIX_INSTALL_DIR && chown $USERNAME $NIX_INSTALL_DIR
USER $USERNAME
ENV USER=$USERNAME
ENV HOME=/home/$USERNAME
RUN curl -L https://nixos.org/nix/install | sh -s -- --no-daemon \
    && echo '. /home/'$USERNAME'/.nix-profile/etc/profile.d/nix.sh' >> /home/$USERNAME/.bashrc \
    && mkdir -p /home/$USERNAME/.config/nix \
    && echo "experimental-features = nix-command flakes" >> /home/$USERNAME/.config/nix/nix.conf

# Nix環境変数の設定
ENV PATH="/home/$USERNAME/.nix-profile/bin:/nix/var/nix/profiles/default/bin:${PATH}" \
    NIX_PATH="/home/$USERNAME/.nix-profile/etc/profile.d/nix.sh:/nix/var/nix/profiles/default/etc/profile.d/nix.sh" \
    NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt

USER root

# Node.jsのインストール
RUN curl -fsSL https://deb.nodesource.com/setup_$NODE_VER.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g yarn \
    && mkdir -p /home/$USERNAME/.nvm \
    && chown -R $USERNAME:$USERNAME /home/$USERNAME/.nvm \
    && echo '#!/bin/bash\n\
curl -o- https://raw.githubusercontent.com/nvm-sh/nvm/v0.37.2/install.sh | bash\n\
. /home/'$USERNAME'/.nvm/nvm.sh\n\
nvm install "'$NODE_VER'"\n\
nvm use "'$NODE_VER'"' > /tmp/nvm_install.sh \
    && chmod +x /tmp/nvm_install.sh \
    && su - $USERNAME -c /tmp/nvm_install.sh \
    && rm /tmp/nvm_install.sh

# Gemini CLIのインストール
RUN npm install -g @google/gemini-cli

# Claude Codeのインストール
RUN npm install -g @anthropic-ai/claude-code

# Copilot CLIのインストール
RUN npm install -g @github/copilot

# すべてのインストールが完了した後、ユーザーのホームディレクトリの所有権を確保
RUN chown -R $USERNAME:$USERNAME /home/$USERNAME

# 作業ディレクトリの設定
WORKDIR /srv

# ユーザーを設定したユーザーに切り替える
USER $USERNAME

# デフォルトのシェルを bash に設定
SHELL ["/bin/bash", "-c"]