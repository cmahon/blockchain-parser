# Blockchain parser

Experimental sandbox for blockchain parsing / analytics. Early stage work in progress.

# Sample output (minor manual formatting applied)

{/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00000.dat open}
{/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00000.dat closed}
{/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00001.dat open}
{/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00001.dat closed}
...
{/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00384.dat closed}

Blocks processed: 386182
Blocks in main chain: 386162
ChainStats
{ chainstatsNumBlocks = 386162
, chainstatsNumTxns = 94704517
, chainstatsMeanTxnsPerBlock = 245.24556274309745
, chainstatsMinTxnsPerBlock = Just 0
, chainstatsMaxTxnsPerBlock = Just 12238
}

# To do

* Handle blocks padded with zeros (I think this is the reason it stopped at blk00384.dat)
* Calc and output duration
* Data structures - try hashmap, vector, etc.
* Add stats requiring deeper interrogation of txns to check performance

# References/sources:

* [BitD](https://github.com/benma/bitd)
* [Haskoin](https://github.com/haskoin/haskoin)
