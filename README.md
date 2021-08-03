# Fp with Scala

## Getting started

### System requirements

- git
- docker
- docker-compose

## Preflight

To install all project dependencies, you have to run our preflight script `./setup.sh`.

## Startup

To startup this project you have to run the script `./start.sh`.

## Bash alias

We suggest to add the following alias to your `~/.bashrc` or `~/.zshrc`.

```bash
alias fp_scala="docker exec -ti fp_with_scala_fp_dev zsh"
alias fp_scala_start="docker exec -ti fp_with_scala_fp_dev sbt run"
```

## Commands

Use:

- `fp_scala` to enter in backend container
- `fp_scala_start` to start backend project

## Start project

- Use this command `fp_scala_start`.
