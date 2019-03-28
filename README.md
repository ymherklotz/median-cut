# Median Cut

The median cut algorithm is a method to deterministically sample an environment
map. This is achieved by splitting the environment map along the longest
dimension so that there is equal energy in both halves. This is repeated _n_
times recursively in each partition. Once there have been _n_ iterations, the
lights are placed in the centroid of each region. Below is an example with 6
splits, meaning there are 2^6 = 64 partitions.

![median cut](/data/median_cut6.jpg)

The average colour of each region is assigned to each light source that was
created in each region.

![median cut lights](/data/median_cut_radiance6.png)

Finally, these discrete lights can be used to light diffuse objects efficiently,
by only having to sample a few lights.

![lighting](/data/simple_sphere64.png)

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
