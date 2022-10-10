# Overview

Simple PasteBin-like service.

# Build

```
  nix build .
```

It is highly recommended to [setup a binary cache](https://input-output-hk.github.io/haskell.nix/tutorials/getting-started-flakes.html#setting-up-the-binary-cache) before proceeding with the build 

# Usage

## Prepare Environment

Start Postgresql instance
```
  docker-compose up
```

## Create new entry

```
  curl -X POST -H "Content-Type: application/json" -d '{"entryTitle":"entry1", "entryBody":"body1"}' "http://localhost:3001/paste"
```

## List paste items

```
  curl "http://localhost:3001/paste"
```

## Get paste item

```
  curl "http://localhost:3001/paste/<item-id>"
```

## Delete paste item

```
  curl -X DELETE "http://localhost:3001/paste/<item-id>"
```
