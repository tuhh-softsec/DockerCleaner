FROM ubuntu:bionic AS resource
RUN apt-get update \
 && apt-get install --no-install-recommends curl=7.58.0-2ubuntu3.24 git=1:2.17.1-1ubuntu0.17 gnupg=2.2.4-1ubuntu1.6 gzip=1.6-5ubuntu1.2 jq=1.5+dfsg-2 openssl=1.1.1-1ubuntu2.1~18.04.21 libssl-dev=1.1.1-1ubuntu2.1~18.04.21 make=4.1-9.1ubuntu1 g++=4:7.4.0-1ubuntu2.3 openssh-client=1:7.6p1-4ubuntu0.7 libstdc++6=8.4.0-1ubuntu1~18.04 -y \
 && rm -rf /var/lib/apt/lists/*
RUN curl -s https://packagecloud.io/install/repositories/github/git-lfs/script.deb.sh | bash
RUN apt-get install --no-install-recommends git-lfs=2.3.4-1 -y
WORKDIR /root
RUN git clone https://github.com/proxytunnel/proxytunnel.git \
 && cd proxytunnel \
 && make -j4 \
 && install -c proxytunnel /usr/bin/proxytunnel \
 && cd .. \
 && rm -rf proxytunnel
RUN git config --global user.email "git@localhost"
RUN git config --global user.name "git"
COPY assets/ /opt/resource/
RUN chmod +x /opt/resource/*
COPY scripts/install_git_crypt.sh install_git_crypt.sh
RUN ./install_git_crypt.sh \
 && rm ./install_git_crypt.sh
WORKDIR /usr/libexec/git-core
RUN rm -f git-add git-add--interactive git-annotate git-apply git-archimport git-archive git-bisect--helper git-blame git-branch git-bundle git-credential-cache git-credential-cache--daemon git-credential-store git-cat-file git-check-attr git-check-ignore git-check-mailmap git-check-ref-format git-checkout git-checkout-index git-cherry git-cherry-pick git-clean git-clone git-column git-commit git-commit-tree git-config git-count-objects git-credential git-cvsexportcommit git-cvsimport git-cvsserver git-describe git-diff git-diff-files git-diff-index git-diff-tree git-difftool git-fast-export git-fast-import git-fetch git-fetch-pack git-fmt-merge-msg git-for-each-ref git-format-patch git-fsck git-fsck-objects git-gc git-get-tar-commit-id git-grep git-hash-object git-help git-http-backend git-imap-send git-index-pack git-init git-init-db git-lfs git-log git-ls-files git-ls-remote git-ls-tree git-mailinfo git-mailsplit git-merge git-mktag git-mktree git-mv git-name-rev git-notes git-p4 git-pack-objects git-pack-redundant git-pack-refs git-patch-id git-peek-remote git-prune git-prune-packed git-push git-read-tree git-reflog git-relink git-remote git-remote-ext git-remote-fd git-remote-testsvn git-repack git-replace git-repo-config git-rerere git-reset git-rev-list git-rev-parse git-revert git-rm git-send-email git-send-pack git-shortlog git-show git-show-branch git-show-index git-show-ref git-stage git-show-ref git-stage git-status git-stripspace git-svn git-symbolic-ref git-tag git-tar-tree git-unpack-file git-unpack-objects git-update-index git-update-ref git-update-server-info git-upload-archive git-var git-verify-pack git-verify-tag git-whatchanged git-write-tree
WORKDIR /usr/bin
RUN rm -f git-cvsserver git-shell git-receive-pack git-upload-pack git-upload-archive \
 && ln -s git git-upload-archive \
 && ln -s git git-merge \
 && ln -s git git-crypt
WORKDIR /usr/share
RUN rm -rf gitweb locale perl
WORKDIR /usr/lib
RUN rm -rf perl
FROM resource AS tests
COPY test/ /tests
RUN /tests/all.sh
FROM resource AS integrationtests
RUN apt-get update \
 && apt-get install --no-install-recommends squid=3.5.27-1ubuntu1.14 -y
COPY test/ /tests/test
COPY integration-tests /tests/integration-tests
RUN /tests/integration-tests/integration.sh
FROM resource
RUN groupadd --system docker-user ; useradd --system --gid docker-user docker-user
USER docker-user
# Please add your HEALTHCHECK here!!!
