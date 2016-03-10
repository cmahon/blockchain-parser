# Blockchain Parser

Experimental sandbox for blockchain parsing / analytics. Early stage work in progress.

## Sample output (minor manual formatting applied)

    {/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00000.dat open}
    {/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00000.dat closed}
    {/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00001.dat open}
    {/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00001.dat closed}
    ...
    {/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00384.dat open}
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

## To do

* Possible local block corruption: 8 byte sequence (comprising block marker and an incorrect block size) between 2 valid blocks in blk00384.dat. Try redownloading blocks.
* Calc duration of analysis
* Data structures - try hashmap, vector, etc.
* Add stats requiring deeper interrogation of txns to check performance
* Derive longest chain based on difficulty rather than depth
* Command line parameters
* Bundle a set of standard analyses
* Charts
* Factor out binary decoder pipe into a dedicated library
* Rename PipesExtras
* Transform and persist to a queryable data store
* Add references to blockchain data format / parsing articles and related repos (e.g. Bitcoin wiki, Bitcoin.org dev ref, Znort, Radcliff, etc.)

## References/sources:

* [BitD](https://github.com/benma/bitd)
* [Haskoin](https://github.com/haskoin/haskoin)

