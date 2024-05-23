<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
# Table of Contents

- [Plutarch Merkle Tree](#plutarch-merkle-tree)
  - [Introduction](#introduction)
  - [Documentation](#documentation)
    - [Merkle Tree](#merkle-tree)
    - [Plutarch Merkle Tree](#plutarch-merkle-tree-implementation)
  - [Getting Started](#getting-started)
    - [Prerequisites](#prerequisites)
    - [Building and Developing](#building-and-developing)
  - [Tutorial](#tutorial)
    - [How to use](#how-to-use)
  - [Testing](#testing)
  - [Case study](#case-study)
- [Acknowledgments](#acknowledgments)

<!-- markdown-toc end -->

# Plutarch Merkle Tree

## Introduction

The Plutarch Merkle Tree project provides a Plutarch-based implementation of Merkle Trees for the Cardano blockchain. This project allows developers to leverage the security and efficiency of Merkle Trees in their Cardano smart contracts, ensuring data integrity and efficient data verification.

This project is funded by the Cardano Treasury in [Catalyst Fund 10](https://projectcatalyst.io/funds/10/f10-osde-open-source-dev-ecosystem/anastasia-labs-the-trifecta-of-data-structures-merkle-trees-tries-and-linked-lists-for-cutting-edge-contracts) and is aimed at enhancing the capabilities of Cardano smart contracts in handling complex data structures.

## Documentation

### Merkle Tree

A Merkle tree, named after its inventor Ralph Merkle, is a fundamental data structure in computer science and cryptography. It's particularly well-suited for managing and verifying large data structures, especially in distributed systems like blockchain technologies. Here's a detailed explanation:

#### Basic concept

A Merkle tree is a type of binary tree, consisting of nodes. Here's how it's structured:

- **Leaf Nodes**: These are the bottom-most nodes in the tree and contain hashed data. The data could be transactions (as in blockchain), files, or any data chunks.
- **Non-Leaf (Intermediate) Nodes**: These nodes store a cryptographic hash of the combined data of their child nodes.
- **Root Node**: The single node at the top of the tree contains a hash formed by its child nodes, ultimately representing the hashes of all lower levels.

#### Hash function

The core of a Merkle tree is the hash function (like SHA-256 in Bitcoin). This function takes digital data of any size and produces a fixed-size string of bytes, typically a unique digital fingerprint of the input data.

#### Construction

- **Hashing the Data**: First, each piece of data at the leaf level is hashed.
- **Pairing and Hashing Upwards**: These hashes are then paired and concatenated, and the resultant string is hashed again. This process continues until you reach the single hash at the top - the root hash.
- **Tree Structure**: This process creates a tree-like structure where each parent node is a hash of its children, providing a secure and efficient means of verifying the contents of the tree.

#### Features

- **Efficiency in Verification**: To verify any single data chunk's integrity, you don't need to download the entire tree. You only need the hashes of the nodes along the path from your data chunk to the root.
- **Tamper-Proof**: Any change in a leaf node (data) will result in a completely different root hash through a cascading effect of changes in the intermediate hashes. This makes it easy to detect alterations.
- **Concurrency Friendly**: Multiple branches of the tree can be processed simultaneously, making Merkle trees highly efficient for parallel processing.

#### Example

Consider a Merkle tree with four leaf nodes (A, B, C, D).

```
                      Merkle Root
                            |
                +-----------+-----------+
                |                       |
            Hash(A+B)               Hash(C+D)
                |                       |
            +---+---+               +---+---+
            |       |               |       |
            Hash(A) Hash(B)     Hash(C) Hash(D)
```

1. Each of A, B, C, and D is hashed: Hash(A), Hash(B), Hash(C), Hash(D).
2. The hashes of A and B are combined and hashed: Hash(Hash(A) + Hash(B)). Similarly for C and D.
3. The hash results from step 2 are combined and hashed to give the Merkle root.

Thus, the Merkle root is a digest of all the data in the leaf nodes.

In conclusion, Merkle trees offer a secure and efficient way to summarize and verify large data sets.

### Plutarch Merkle Tree implementation

The Plutarch Merkle Tree implementation provides several functions to create and manipulate Merkle Trees. Below is a brief overview of each function:

-`fromList`: Constructs a Merkle Tree from a list of serialized data.

-`toList`: Deconstructs a Merkle Tree back into a list of elements.

-`rootHash`: Retrieves the root hash of a Merkle Tree.

-`isNull`: Checks if a Merkle Tree is empty.

-`size`: Returns the number of leaf nodes in a Merkle Tree.

-`mkProof`: Generates a proof of membership for an element in the Merkle Tree.

-`member`: Verifies if an element is part of a Merkle Tree using a proof.

-`hash`: Computes a SHA-256 hash of a given byte string.

-`combineHash`: Combines two hashes into a new one.

-`addLeaf`: Adds a new leaf to the Merkle Tree.

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

## Demeter Workspace 

To provide a seamless experience for running and trying out our application, click the workspace button below. This will start a workspace in Demeter with our repository code automatically cloned.


[![Code in Workspace](https://demeter.run/code/badge.svg)](https://demeter.run/code?repository=https://github.com/Anastasia-Labs/plutarch-merkle-tree&template=plutus&size=large) 

# Tutorial

## How to use

This guide demonstrates how to use the Merkle Tree for blockchain applications using Plutarch and Plutus.

### Setting up the environment

First, ensure you have the necessary imports:

```haskell
import Plutarch.MerkleTree (pmember)
import Plutus.MerkleTree (fromList, mkProof, rootHash, member)
import Data.Maybe (fromJust)
import PlutusTx qualified
import Plutarch.Prelude
import Utils
```

### Creating and Utilizing a Merkle Tree

1.Constructing the Merkle Tree

```haskell
-- Create your MerkleTree
myMerkleTree = fromList ["a","b","c","d","e","f","g","h","1","2","3","4"]
```

2.Gererating a Proof for a member

To validate the presence of an element(e.g., "e"), generate a proof for it.

```haskell
-- Create your Proof for "e" member
myProof = fromJust $ mkProof "e" myMerkleTree
```

3.Extracting the Root Hash
Obtain the root hash of your Merkle Tree, which is used for verification.

```haskell
root = rootHash myMerkleTree
```

### Plutarch On Chain

To perform validation on-chain using Plutarch, follow these steps:

```haskell
pmyProof = pmap # plam (\x -> pdata x) #$ pconstant myProof

evalT $ pmember # (pconstant "e") # (pconstant root) # pmyProof

```

This script maps your Haskell proof to Plutarch data type and evaluates whether the element "e" is a member of the tree with the given root and proof

### Haskell On Chain

For on-chain validation using Plutus, using the following:

```haskell
member (PlutusTx.Prelude.encodeUtf8 "e") root myProof

```

This line of code checks if the string "e", after being converted to UTF-8 format, is a member of the Merkle Tree given the root and the proof.

### Sample validator

For a complete example, including tests and further explanations, reger to the provided sample validator: [MerkleTreeSpec.hs](/test/Spec/MerkleTreeSpec.hs).

## Testing

For comprehensive information on testing the Plutarch Merkle Tree implementation, including unit tests and property-based tests, please refer to our [test documentation](/test/README.md).

# Case study

For an in-depth real-world case study on the application of Merkle Trees within the Cardano blockchain environment, particularly in the context of sidechain to main chain token transfers, refer to the following resource:

[Cardano Sidechain Toolkit - Main Chain Plutus Scripts](https://docs.cardano.org/cardano-sidechains/sidechain-toolkit/mainchain-plutus-scripts/)

This case study provides valuable insights into how Merkle Trees are integrated into blockchain transactions, offering practical examples and detailed workflows.

# Acknowledgments

This library takes the PlutusTx code from the Hydra project, developed and maintained by Input Output Hong Kong (IOHK).

The repository can be found at <https://github.com/input-output-hk/hydra/tree/master/plutus-merkle-tree>
