# MerkleTree 


```
myMerkleTree = fromList $ PlutusTx.Prelude.encodeUtf8 <$> ["1","2","3"]
myRootHash = rootHash myMerkleTree
myProof = fromJust $ mkProof (PlutusTx.Prelude.encodeUtf8 "2") myMerkleTree
```

```
member (PlutusTx.Prelude.encodeUtf8 "2") rh p

```
