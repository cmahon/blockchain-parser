# Blockchain Parser

Experimental sandbox for blockchain parsing / analytics. Early stage work in progress.

## Sample output (minor manual formatting applied)

    {/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00000.dat open}
    {/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00000.dat closed}
    {/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00001.dat open}
    {/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00001.dat closed}
    ...
    {/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00465.dat open}
    {/Users/Chris/Library/Application Support/Bitcoin/blocks/blk00465.dat closed}
    
    Blocks processed: 402086
    Blocks in main chain: 402086

    ChainStats
    { chainstatsNumBlocks = 402086
    , chainstatsNumTxns = 115145839
    , chainstatsMeanTxnsPerBlock = 286.37117183886033
    , chainstatsMinTxnsPerBlock = Just 0
    , chainstatsMaxTxnsPerBlock = Just 12238
    }

## To do

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
* Add references to blockchain data format / parsing articles and related repos

## References/sources:

* [BitD](https://github.com/benma/bitd)
* [Haskoin](https://github.com/haskoin/haskoin)
