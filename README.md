# MerkleTree

The Plutarch Merkle Tree project implements a Merkle tree in Plutarch, providing a valuable tool for proving the presence of data within a tree structure efficiently. This implementation is particularly useful for blockchain applications, such as minting policies for NFT platforms.

## Prerequisites

Ensure the following tools are installed to work with this repository:

- [Nix Package Manager](https://nixos.org/download.html)

- (Optional) [direnv](https://direnv.net/), which aids in environment management and can be installed via Nix.

## Setup

To set up the Plutarch Merkle Tree repository:

1.Clone the repository:

```sh
git clone https://github.com/Anastasia-Labs/plutarch-merkle-tree.git
```

2.Navigate to the repository directory:

```sh
cd plutarch-merkle-tree
```

3.Activate the development environment with Nix:

```sh
nix develop .
```

4.Build:

```sh
cabal build all
```

5.Execute the test suite:

```sh
cabal test
```

![plutarch-merkle-tree.gif](/assets/images/plutarch-merkle-tree.gif)

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

This library takes the PlutusTx code from the Hydra project, developed and maintained by Input Output Hong Kong (IOHK).

The repository can be found at <https://github.com/input-output-hk/hydra/tree/master/plutus-merkle-tree>
