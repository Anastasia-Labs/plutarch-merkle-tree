<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
# Table of Contents

- [Plutarch Merkle Tree](#plutarch-merkle-tree)
  - [Introduction](#introduction)
  - [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Building and Developing](#building-and-developing)
  - [Tutorial](#tutorial)
    - [How to use](#how-to-use)
- [Acknowledgments](#acknowledgments)

<!-- markdown-toc end -->

# Plutarch Merkle Tree

## Introduction

The Plutarch Merkle Tree project provides a Plutarch-based implementation of Merkle Trees for the Cardano blockchain. This project allows developers to leverage the security and efficiency of Merkle Trees in their Cardano smart contracts, ensuring data integrity and efficient data verification.

This project is funded by the Cardano Treasury in [Catalyst Fund 10](https://projectcatalyst.io/funds/10/f10-osde-open-source-dev-ecosystem/anastasia-labs-the-trifecta-of-data-structures-merkle-trees-tries-and-linked-lists-for-cutting-edge-contracts) and is aimed at enhancing the capabilities of Cardano smart contracts in handling complex data structures.

## Getting Started

### Prerequisites

Before you begin, ensure you have [Nix](https://nixos.org/download.html) installed on your system. Nix is used for package management and to provide a consistent development environment.
To install run the following command:

```sh
sh <(curl -L https://nixos.org/nix/install) --daemon
```

and follow the instructions.

```sh
$ nix --version
nix (Nix) 2.18.1
```

Make sure to enable [Nix Flakes](https://nixos.wiki/wiki/Flakes#Enable_flakes) by editing either `~/.config/nix/nix.conf` or `/etc/nix/nix.conf` on
your machine and add the following configuration entries:

```yaml
experimental-features = nix-command flakes ca-derivations
allow-import-from-derivation = true
```

Optionally, to improve build speed, it is possible to set up binary caches
maintained by IOHK and Plutonomicon by setting additional configuration entries:

```yaml
substituters = https://cache.nixos.org https://iohk.cachix.org https://cache.iog.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ= iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo=
```

To facilitate seamlessly moving between directories and associated Nix development shells we use [direnv](https://direnv.net) and [nix-direnv](https://github.com/nix-community/nix-direnv):

Your shell and editors should pick up on the `.envrc` files in different directories and prepare the environment accordingly. Use `direnv allow` to enable the direnv environment and `direnv reload` to reload it when necessary. Otherwise, the `.envrc` file contains a proper Nix target you can use with the `nix develop` command.

To install both using `nixpkgs`:

```sh
nix profile install nixpkgs#direnv
nix profile install nixpkgs#nix-direnv
```

### Building and developing

Once Nix is installed, you should be able to seamlessly use the repository to
develop, build and run packages.

Download the Git repository:

```sh
git clone https://github.com/Anastasia-Labs/plutarch-merkle-tree.git
```

Navigate to the repository directory:

```sh
cd plutarch-merkle-tree
direnv allow
```

Activate the development environment with Nix:

```sh
nix develop .
```

Additionally, when you run `nix run .#help` you'll get a list of scripts you can run, the Github CI (nix flake check) is setup in a way where it checks the project builds successfully, haskell format is done correctly, and commit message follows conventional commits. Before pushing you should run `cabal run` , `nix run .#haskellFormat` (automatically formats all haskell files, including cabal), if you want to commit a correct format message you can run `cz commit`

Build:

```sh
cabal build all
```

Execute the test suite:

```sh
cabal test
```

![plutarch-merkle-tree.gif](/assets/images/plutarch-merkle-tree.gif)

### Tutorial


## How to use

```haskell
import Plutarch.MerkleTree (pmember)
import Plutus.MerkleTree (fromList, mkProof, rootHash, member)
import Data.Maybe (fromJust)
import PlutusTx qualified
import Plutarch.Prelude
import Utils

-- Create your MerkleTree
myMerkleTree = fromList ["a","b","c","d","e","f","g","h","1","2","3","4"]

-- Create your Proof for "e" member
myProof = fromJust $ mkProof "e" myMerkleTree

-- Extract the Root Hash from MerkleTree
root = rootHash myMerkleTree

```

## Plutarch On Chain

```haskell
pmyProof = pmap # plam (\x -> pdata x) #$ pconstant myProof

evalT $ pmember # (pconstant "e") # (pconstant root) # pmyProof

```

## Haskell On Chain

```haskell
member (PlutusTx.Prelude.encodeUtf8 "e") root myProof

```

# Acknowledgments

This library takes the PlutusTx code from the Hydra project, developed and maintained by Input Output Hong Kong (IOHK).

The repository can be found at <https://github.com/input-output-hk/hydra/tree/master/plutus-merkle-tree>
