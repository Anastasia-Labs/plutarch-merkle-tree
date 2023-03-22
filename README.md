# MerkleTree 

## How to use
```haskell
import Plutarch.MerkleTree
import Data.Maybe
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
# Plutarch On Chain
```haskell
pmyProof = pmap # plam (\x -> pdata x) #$ pconstant myProof

evalT $ pmember # (pconstant "e") # (pconstant root) # pmyProof

```

# Haskell On Chain

```haskell
member (PlutusTx.Prelude.encodeUtf8 "e") root myProof

```
