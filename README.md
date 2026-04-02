# ReSources

### Access to online version:
- MAIN version: https://isomemoapp.com/app/resources
- BETA version: https://isomemoapp.com/app/resources-beta

### Documentation
- https://pandora-isomemo.github.io/ReSources/

### Installation instructions
- https://pandora-isomemo.github.io/docs/apps.html#resources---food-reconstruction-using-isotopic-transferred-signals

### Release notes:
- see `NEWS.md`

### Folder for online models
- [`inst/app/predefinedModels`](https://github.com/Pandora-IsoMemo/resources/tree/main/inst/app/predefinedModels)

## Notes for developers

When adding information to the _help_ sites, _docstrings_ or the _vignette_ of this 
package, please update documentation locally as follows. The documentation of
the main branch is built automatically via GitHub Actions.

```R
devtools::document() # or CTRL + SHIFT + D in RStudio
devtools::build_site()
```

When testing with a local docker container, please make sure to rebuild the docker image after changes in the R code or dependencies. You can do this from the root of the repository via:

```bash
docker build -t resources-app:latest .
```

After that, start the container as usual via:

```bash
docker run -p 3838:3838 resources-app:latest
```

and access the app in your browser at `http://localhost:3838/`. Stop the container with `CTRL + C` in the terminal.

**Optional:**

Add `-it` for interactive mode, or `--rm` to remove the container after stopping.
