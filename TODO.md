- Re-compress example data:

```
tools::resaveRdaFiles("data", compress = "xz")
```

- Add to DESCRIPTION:
```
LazyData: true
LazyDataCompression: xz 
```
