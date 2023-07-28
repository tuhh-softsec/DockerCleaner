# DockerCleaner

## Required Tools

* Haskell Stack (https://docs.haskellstack.org/en/stable/README/)

## Installation

In this folder run:

```bash
stack install
```

this will build the tool and make it available as a command line tool runnable as

```bash
dockercleaner
```

To temporarily get bash complitions for the parameters of the tool run the following:

```bash
source <(dockercleaner --bash-completion-script `which dockercleaner`)
```

or add the output of
```bash
dockercleaner --bash-completion-script `which dockercleaner`
```
to `/etc/bash_completion.d/` to permanantly get these.

For more infos and support for other command line environments see https://github.com/pcapriotti/optparse-applicative#bash-zsh-and-fish-completions

## Run

For information on how to use the tool see

```bash
dockercleaner --help
```

### Examples
```bash
dockercleaner -i wordpress.dockerfile -o wordpress_smells.dockerfile --inject
```

```bash
dockercleaner -i wordpress.dockerfile -o wordpress_fixed.dockerfile --fix
```

```bash
dockercleaner -i wordpress.dockerfile -o wordpress_injected_and_then_fixed.dockerfile --inject --fix
```

```bash
dockercleaner -i wordpress.dockerfile -o wordpress_fixed.dockerfile --fix --smell use-no-install-recommends --seed 12345
```

```bash
dockercleaner -i wordpress.dockerfile -o wordpress_fixed.dockerfile --fix --smell use-no-install-recommends --smell pin-package-manager-versions-apt-get
```

```bash
dockercleaner -i wordpress.dockerfile -o wordpress_fixed.dockerfile --fix --inject --random-smells --seed 4 --metadata --metadata-file wordpress_fixed.dockerfile.json
```

### smells

* do-not-have-secrets
* do-not-use-apt-get-update-alone
* have-a-healthcheck
* pin-package-manager-versions-apk
* pin-package-manager-versions-apt-get
* pin-package-manager-versions-gem
* pin-package-manager-versions-npm
* pin-package-manager-versions-pip
* use-copy-instead-of-add
* use-no-install-recommends
* use-wget-instead-of-add

## Development REPL

```bash
stack ghci
```

## Run tests

```bash
stack test
```
