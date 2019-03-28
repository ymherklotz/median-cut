# Median Cut

## Build and run

To compile and run, one has to first download
[stack](https://docs.haskellstack.org/en/stable/README/)

The simplest way to do this is by executing the following command:

```
curl -sSL https://get.haskellstack.org/ | sh
```

Then run setup in this directory:

```
stack setup
```

Finally the executable can be built and run using the following:

```
stack build --exec median-cut
```

This project relies on a open source library that I wrote to load 
PFM files which is hosted at [PFM](https://github.com/ymherklotz/pfm).
It will automatically get downloaded when built with stack.
