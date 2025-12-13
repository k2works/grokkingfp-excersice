# =============================================================================
# Grokking Functional Programming - Multi-Language Development Container
# Supports: Scala, Java, F#, C#, Haskell, Clojure, Elixir, Rust, Python, TypeScript, Ruby
# =============================================================================

FROM ubuntu:22.04 AS base

# Environment variables
ENV DEBIAN_FRONTEND=noninteractive \
    LANG=ja_JP.UTF-8 \
    LC_ALL=ja_JP.UTF-8 \
    LC_CTYPE=ja_JP.UTF-8 \
    # Version settings
    NODE_VER=22 \
    JAVA_VERSION=21 \
    SBT_VERSION=1.10.6 \
    GRADLE_VERSION=8.11.1 \
    DOTNET_VERSION=8.0 \
    RUBY_VERSION=3.3 \
    PYTHON_VERSION=3.11 \
    # Paths
    SDKMAN_DIR=/home/developer/.sdkman \
    PATH="/home/developer/.local/bin:/home/developer/.cargo/bin:/home/developer/.ghcup/bin:$PATH"

# User configuration
ARG USERNAME=developer
ARG USER_UID=1000
ARG USER_GID=$USER_UID

# Create user with sudo privileges
RUN groupadd --gid $USER_GID $USERNAME \
    && useradd --uid $USER_UID --gid $USER_GID -m $USERNAME \
    && apt-get update \
    && apt-get install -y sudo \
    && echo $USERNAME ALL=\(root\) NOPASSWD:ALL > /etc/sudoers.d/$USERNAME \
    && chmod 0440 /etc/sudoers.d/$USERNAME

# =============================================================================
# Locale Setup
# =============================================================================
RUN apt-get update && apt-get install -y \
    language-pack-ja-base \
    language-pack-ja \
    && update-locale LANG=ja_JP.UTF-8 LANGUAGE=ja_JP:ja \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# Base Packages
# =============================================================================
RUN apt-get update && apt-get install -y \
    build-essential \
    zip \
    unzip \
    git \
    curl \
    wget \
    vim \
    tmux \
    ca-certificates \
    gnupg \
    lsb-release \
    apt-transport-https \
    software-properties-common \
    libssl-dev \
    libffi-dev \
    libreadline-dev \
    zlib1g-dev \
    libyaml-dev \
    libncurses5-dev \
    libgmp-dev \
    pkg-config \
    && apt-get clean \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# Node.js & TypeScript
# =============================================================================
RUN curl -fsSL https://deb.nodesource.com/setup_${NODE_VER}.x | bash - \
    && apt-get install -y nodejs \
    && npm install -g yarn typescript ts-node \
    && npm cache clean --force

# =============================================================================
# Java (OpenJDK 21) - Required for Scala, Java, Clojure
# =============================================================================
RUN apt-get update \
    && apt-get install -y openjdk-${JAVA_VERSION}-jdk \
    && rm -rf /var/lib/apt/lists/*

ENV JAVA_HOME=/usr/lib/jvm/java-${JAVA_VERSION}-openjdk-amd64

# =============================================================================
# Scala (sbt)
# =============================================================================
RUN echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list \
    && echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list \
    && curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add \
    && apt-get update \
    && apt-get install -y sbt \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# Gradle (for Java)
# =============================================================================
RUN wget -q https://services.gradle.org/distributions/gradle-${GRADLE_VERSION}-bin.zip -O /tmp/gradle.zip \
    && unzip -q /tmp/gradle.zip -d /opt \
    && rm /tmp/gradle.zip \
    && ln -s /opt/gradle-${GRADLE_VERSION}/bin/gradle /usr/local/bin/gradle

# =============================================================================
# .NET SDK (for F# and C#)
# =============================================================================
RUN wget https://packages.microsoft.com/config/ubuntu/22.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb \
    && dpkg -i packages-microsoft-prod.deb \
    && rm packages-microsoft-prod.deb \
    && apt-get update \
    && apt-get install -y dotnet-sdk-${DOTNET_VERSION} \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# Clojure
# =============================================================================
RUN curl -L -O https://github.com/clojure/brew-install/releases/latest/download/linux-install.sh \
    && chmod +x linux-install.sh \
    && ./linux-install.sh \
    && rm linux-install.sh

# Leiningen
RUN wget https://raw.githubusercontent.com/technomancy/leiningen/stable/bin/lein -O /usr/local/bin/lein \
    && chmod +x /usr/local/bin/lein

# =============================================================================
# Elixir & Erlang (using Ubuntu packages for reliability)
# =============================================================================
RUN apt-get update \
    && apt-get install -y erlang elixir \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# Python
# =============================================================================
RUN add-apt-repository -y ppa:deadsnakes/ppa \
    && apt-get update \
    && apt-get install -y \
        python${PYTHON_VERSION} \
        python${PYTHON_VERSION}-venv \
        python${PYTHON_VERSION}-dev \
        python3-pip \
    && update-alternatives --install /usr/bin/python3 python3 /usr/bin/python${PYTHON_VERSION} 1 \
    && update-alternatives --install /usr/bin/python python /usr/bin/python${PYTHON_VERSION} 1 \
    && rm -rf /var/lib/apt/lists/*

# Install Poetry
RUN curl -sSL https://install.python-poetry.org | python3 - \
    && ln -s /root/.local/bin/poetry /usr/local/bin/poetry || true

# =============================================================================
# Ruby
# =============================================================================
RUN apt-get update \
    && apt-get install -y \
        ruby-full \
        ruby-bundler \
    && rm -rf /var/lib/apt/lists/*

# =============================================================================
# Haskell (GHCup)
# =============================================================================
USER $USERNAME
WORKDIR /home/$USERNAME

RUN curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=9.4.8 \
    BOOTSTRAP_HASKELL_CABAL_VERSION=3.10.2.1 \
    BOOTSTRAP_HASKELL_INSTALL_STACK=1 \
    BOOTSTRAP_HASKELL_INSTALL_HLS=1 \
    sh

ENV PATH="/home/$USERNAME/.ghcup/bin:$PATH"

# =============================================================================
# Rust
# =============================================================================
RUN curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y \
    && . $HOME/.cargo/env \
    && rustup default stable

ENV PATH="/home/$USERNAME/.cargo/bin:$PATH"

# =============================================================================
# Switch back to root for final setup
# =============================================================================
USER root

# =============================================================================
# CLI Tools (Gemini CLI, Claude Code)
# =============================================================================
RUN npm install -g @google/gemini-cli @anthropic-ai/claude-code

# =============================================================================
# MkDocs for documentation
# =============================================================================
RUN pip3 install \
    mkdocs \
    mkdocs-material \
    plantuml-markdown \
    pymdown-extensions

# =============================================================================
# Final Setup
# =============================================================================

# Ensure proper ownership
RUN chown -R $USERNAME:$USERNAME /home/$USERNAME

# Set working directory
WORKDIR /srv

# Switch to developer user
USER $USERNAME

# Default shell
SHELL ["/bin/bash", "-c"]

# Verify installations
RUN echo "=== Installed Versions ===" \
    && echo "Node.js: $(node --version)" \
    && echo "Java: $(java --version 2>&1 | head -1)" \
    && echo "Scala/sbt: $(sbt --version 2>&1 | head -1 || echo 'sbt installed')" \
    && echo "Gradle: $(gradle --version 2>&1 | grep Gradle)" \
    && echo ".NET: $(dotnet --version)" \
    && echo "Clojure: $(clojure --version 2>&1 || echo 'Clojure installed')" \
    && echo "Elixir: $(elixir --version 2>&1 | head -1)" \
    && echo "Python: $(python --version)" \
    && echo "Ruby: $(ruby --version)" \
    && echo "GHC: $(ghc --version)" \
    && echo "Rust: $(rustc --version)" \
    && echo "=========================="

CMD ["/bin/bash"]
