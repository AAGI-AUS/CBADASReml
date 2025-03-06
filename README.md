# CBADA Package for helping with small-plot analysis, ofe analysis and paddock summaries

## Installation

You can install {CBADA} like so:

```r
if (!require("remotes"))
  install.packages("devtools")
devtools::install_github("CCDM-CBADA/CBADA")
```

## Usage

### Cleaning Farm Data

Below is the current, definitely improvable, outline of how to use this package
to clean some farm data.

```r
## Libraries
library(CBADA)
## library(tidyverse), etc.
```

Define data path, names of farms and years to clean.

```r
datapath <- file.path("R:", "path", "to", "Data")
farm_names <- list("Farm1", "Farm2")
years <- list("2015", "2017", "2020")
```

Create a custom data-reading function. This is because a lot of the data comes
in different formats/directory structures. It is easier to just change the
reading function that to rearrange many files/directories in the filesystem.

This example is used to read in John Deere-recorded data.

This example probably won't work as-is for any given datasets. Please ensure
that the process works for your dataset. The purpose of this is to read in any
dataset, and transform them such that they are in the proper format to be fed
into the cleaning functions.

```r
reading_fun <- function(year, datapath, farm_names) {
    ## Use list files to find all files for given year and farm
    shapefiles <-
        map(farm_names, \(farm) {
            list.files(
                file.path(datapath, "raw", year, "Yield"),
                pattern = paste0("(", substr(farm, 1, 12), ").*shp$"),
                recursive = TRUE,
                full.names = TRUE
            )
        }) %>%
        set_names(farm_names)

    ## Remove empty farms from list of files to read
    ## Change locs to only include existing farms for this year
    shapefiles <- shapefiles[lengths(shapefiles) > 0]
    farm_names <- names(shapefiles)

    ## Read shapefiles
    raw <-
        map(shapefiles, \(farm_file_list) {
            map(farm_file_list, \(padd_file_list) {
                read_sf(
                    padd_file_list,
                    stringsAsFactors = TRUE
                ) %>%
                    st_transform(32750) %>%
                    ## Some data have different names for the below features.
                    rename(Product = "Crop") %>%
                    ## Need to recode the Product col, as it is encoded in
                    ## numbers thanks to John Deere operations centre..
                    mutate(
                        Product = as.character(Product),
                        Product = case_match(
                            Product,
                            "174" ~ "lupin",
                            "30" ~ "lupin", # Gunyidi Lupins?
                            "28" ~ "wheat", # Corack wheat?
                            "26" ~ "wheat", # Corack wheat?
                            "25" ~ "wheat", # Scepter what
                            "24" ~ "wheat",
                            "11" ~ "oats",  # Carrolup Oats
                            "5" ~ "canola", # IH30 Canola?
                            "2" ~ "barley"  # LaTrobe Malt Barley?
                        ) %>% as.factor()
                    )
            }) %>%
                bind_rows() %>%
                st_transform(32750)
        })
    return(raw)
}
```

Next we need to load boundary files. These may be downloaded from JD operations
centre, given by the farmer, or hand drawn if neccessary.

The boundaries are extremely important for all data processing. Keep these
consistent forever, and stored somewhere easy to remember.

Some boundaries have different labels for fields and farms etc. Ensure fields
are in the feature "Field", and farms in "Farm". It is also preferable to have
them stored in a list of farms, where each element is an `sf` object of the
farm's boundaries as `POLYGON`s/`MULTIPOLYGON`s.

For example, we can load in our boundaries:

```r
bounds_poly <- read_sf(file.path(datapath, "boundaries", "boundaries.gpkg"))
```

Which should look as follows:

```r
bounds_poly
## $Farm1
## Simple feature collection with 13 features and 4 fields
## Geometry type: MULTIPOLYGON
## Dimension:     XY
## Bounding box:  xmin: xxx ymin: yyy xmax: xxx ymax: yyy
## Projected CRS: WGS 84 / UTM zone 50S
## # A tibble: 13 × 4
##    CLIENT_NAM Farm  Field                  geom
##    <chr>      <chr> <chr>    <MULTIPOLYGON [m]>
##  1 Client1    Farm1 Field1   (((xxx yyy, xxx...
##  2 Client1    Farm1 Field2   (((xxx yyy, xxx...
##  3 Client1    Farm1 Field3   (((xxx yyy, xxx...
##  4 Client1    Farm1 Field4   (((xxx yyy, xxx...
##  5 Client1    Farm1 Field5   (((xxx yyy, xxx...
##  6 Client1    Farm1 Field6   (((xxx yyy, xxx...
##  7 Client1    Farm1 Field7   (((xxx yyy, xxx...
##  8 Client1    Farm1 Field8   (((xxx yyy, xxx...
##  9 Client1    Farm1 Field9   (((xxx yyy, xxx...
## 10 Client1    Farm1 Field10  (((xxx yyy, xxx...
## 11 Client1    Farm1 Field11  (((xxx yyy, xxx...
## 12 Client1    Farm1 Field12  (((xxx yyy, xxx...
## 13 Client1    Farm1 Field13  (((xxx yyy, xxx...
##
## $Farm2
## ...

```

And finally we run the cleaning functions. `farm_cleaning_wrapper` does a lot
of stuff, and at some stage will hopefully be changed to run on much smaller
amounts of data. At the moment it must take in a list of farms for a given
year. Ideally it should be taking in one paddock at a time, and the user
controls the farm and year it runs on via loops or whatever else. This was just
the quickest way to get this into a function at the time, and since then have
not had enough time to change this to be more flexible. Any changes will be
reflected in the README when the time comes.

```r
map(years, \(year) {
    raw <- reading_fun(year, datapath, farm_names)

    farm_cleaning_wrapper(
        farm_list = dat,
        bounds_poly = bounds_poly,
        year = year,
        datapath = datapath,
        Bearing = "Track_deg_",
        Swath_Width = "Swth_Wdth_",
        Distance = "Distance_m",
        Speed = "Speed_km_h",
        TRT = NULL
    )
})
```
---

### Rasterizing Cleaned Yield

After the yield data has been cleaned, we will need to rasterize it. Rasterized
data is easier to work with, and we can track a single pixel through multiple
years which allows us to calculate production zones.

This process typically goes as follows:

Define important variables such as data path, and farms to rasterize:

```r
farm_names <- list("Farm1", "Farm2", "Farm3")
datapath <- file.path("R:", "path", "to", "Data")
outpath <- file.path(datapath, "..", "Output")
years <- 2015:2020
```

Read in our boundaries

```r
## Read boundary files
bounds <-
    read_sf(file.path(
        datapath,
        "boundaries",
        "boundaries.gpkg"
    ))
```

Now we use the boundaries to calculate consistent grids, we rasterize on these
points so that whenever we do this process we are always creating rasters at
the exact same positions.

This is one of the reasons good, consistent boundaries are very important.

```r
grids <-
    bounds |>
    split(~ Field) |>
    purrr::map(\(padd_bound) {
        grid <- st_make_grid(padd_bound, cellsize = 5, what = "centers")
        return(st_intersection(grid, padd_bound))
    })
```

Now we can finally do the rasterization. This used to use `furrr` to speed up
the calculation time as this step is very slow, however it is very inconsistent
memory-wise as some paddocks are much larger than others.

This step iterates over each paddock and saves the output after it is
calculated.

```r
for (year in years) {
    files <- list.files(file.path(
        datapath, "cleaned", year, "Cleaned-Yld_Mass_D"
    ))

    ## Extract only the files whose names end with ".gpkg"
    farms <- na.omit(stringr::str_extract(files, ".*(?=\\.gpkg$)"))

    for (farm in farms) {
        ## Read in geopackage
        dat <- sf::read_sf(file.path(
                       datapath, "cleaned", year, "Cleaned-Yld_Mass_D",
                       paste0(farm, ".gpkg")
                   ))

        paddocks <- unique(dat$Field)

        ## Block kriging
        for (paddock in paddocks) {
            dat_padd <- dat[dat$Field == paddock, ]

            ## Kriging
            ras <- CBADA::bkrige(dat_padd, grids[[paddock]])

            ## Save output raster
            savepath <- file.path(datapath, "geotiff", farm, paddock)
            dir.create(savepath, recursive = TRUE)
            ras_filepath <- file.path(savepath, paste0(year, ".tif"))
            terra::writeRaster(ras, ras_filepath, overwrite = TRUE)
        }
    }
}
```

---

### Production Zone Calculations and Visualisation

To calculate production zones, read some rasters using `terra`, and set them up
such that each layer represents a different year and is named accordingly:

```r
ras
## class       : SpatRaster
## dimensions  : 425, 328, 8  (nrow, ncol, nlyr)
## resolution  : 5, 5  (x, y)
## extent      : xxx, xxx, yyy, yyy  (xmin, xmax, ymin, ymax)
## coord. ref. : +proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs
## source(s)   : memory
## names       :      2015,      2016,      2017,      2018,      2019,      2020, ...
## min values  : 0.2360485, 0.2167844, 0.2087293, 0.3106788, 0.2074086, 0.2491829, ...
## max values  : 2.9917262, 2.9968791, 1.8548290, 3.1918111, 3.1855433, 1.7570404, ...
```

This raster can be directly passed to a few different function depending on
which stage of the process we want to visualise. `padd_productivity` and
`padd_stability` will calculate rasters for the productivity and stability over
the years respectively.

- `padd_productivity`: Calculates productivity by min-max scaling the raster,
  and taking the mean value of each pixel over all years. Each pixel is
  classified as Low, Average or High productivity based on the global mean and
  the standard deviation. The function has argument `full_padd` for if we want
  to calculate zones only within stable regions, or the whole paddock.
- `padd_stability`: Calculates stability by min-max scaling the raster, and
  takes the coefficient of variation of each pixel over all years. If this
  value is less than the argument `cv_th`, then it will be classified as
  consistent.

```r
prod <- padd_productivity(ras)
stab <- padd_stability(ras)
```

We can also pass `ras` directly to `find_prod_zones` which calculates
productivity zones using the productivity and stability zones calculated by
`padd_productivity` and `padd_stability`. This is then put into `terra::sieve`
to smooth out the results to improve the visualisation.

```r
pz <- find_prod_zones(ras)
```

Which will return a `SpatRaster` with classified pixels. 0 represents
"Inconsistent" areas, and 1 - 3 represents Low, Average and High productivity
zones which are "Consistent".

#### Probability of Exceedance

Using this, we can plot a map of probability of exceedance of a given threshold
yield:

```r
plt <- plot_exceedance_map(pz, 1.5)
ggsave(file.path(datapath, "..", "Outputs", "exceedance.png"), plt)
```

### Plotting Historical Yield

First we read in our historical yield rasters:

```r
hyras <-
    padd_paths %>%
    list.files(full.names = TRUE) %>%
    terra::rast()
hyras
## class       : SpatRaster
## dimensions  : 425, 328, 8  (nrow, ncol, nlyr)
## resolution  : 5, 5  (x, y)
## extent      : xxx, xxx, yyy, yyy  (xmin, xmax, ymin, ymax)
## coord. ref. : WGS 84 / UTM zone 50S (EPSG:32750)
## sources     : 2015.tif
##               2016.tif
##               2017.tif
##               ... and 5 more source(s)
## names       : 2015, 2016, 2017, 2018, 2019, 2020, ...
```

We have to load in a product history `.csv` file. This file should
be created using `metadata_from_files`, *after* running the farm cleaning code.

```r
padd_crop_key <- read_product_history(
    file.path(datapath, "product_history.csv")
)

## Filter it for our paddock and years
key <-
    padd_crop_key %>%
    filter(
        Field == <paddock name> &
        year %in% names(hyras) &
        product %in% c("wheat", "barley")
    ) %>%
    mutate(year = as.character(year)) %>%
    mutate(coln = paste0(year, "-", product))
```

Prepare the rasters and paddock boundaries, then run `plot_historical`:

```r
## Rename hyras to <year>-<product> for ease-of-viewing output
names(hyras) <- key$coln
## Filter the boundaries
bnd <- padd_bounds[padd_bounds$FIELD_NAME == <paddock name>, ]

plot_historical(hyras, bnd)
```

---

### Soil Data Cleaning

Below is the current pipeline of how to clean soil data.

```r
## Libraries
library(CBADA)
## library(tidyverse) etc.
```

Define our important variables:

```r
datapath <- file.path(
    "R:", "path", "to", "Data"
)
## Assuming soil files are structured:
## datapath/
##     soil_data/
##         vrts-<farmer>-<farm>-<year>-raw-<em/gr>-vrt/
##             data-<farmer>-<farm>-<year>-raw-<em/gr>-vrt.csv
soil_filenames <-
    list.files(
        file.path(datapath, "soil_data"),
        pattern = "\\.csv$",
        recursive = TRUE,
        full.names = TRUE
    )
farm_names <- list("Farm1", "Farm2")
```

Read the data and prepare it such that it is a list: Each element is an `sf`
object of a single farm.

```r
soil_raw_em <- map(farm_names, \(farm_name) {
    ## Only read selected farms
    farm_files <- grep(
        farm_name,
        soil_filenames,
        value = TRUE,
        ignore.case = TRUE
    )
    ## Only read em
    em <- read_sf(
        grep("raw-em", farm_files, value = TRUE),
        stringsAsFactors = TRUE
    ) %>%
        st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326)) %>%
        rename(
            Elevation = "Elevation[m]",
            DualEM150 = "DualEM 150cm[mS/m]",
            DualEM50 = "DualEM 50cm[mS/m]"
        ) %>%
        mutate(across(c(Elevation, DualEM150, DualEM50), as.numeric)) %>%
        mutate(Season = as.character(Season)) %>%
        st_transform(32750)
    return(em)
}) %>% set_names(farm_names)
```

Read in the paddock boundaries:

```r
soil_bounds <-
    read_sf(
        file.path(datapath, "boundaries", "boundaries.gpkg"),
        stringsAsFactors = TRUE
    ) %>%
    rename(Farm = "FARM_NAME", Field = "FIELD_NAME") %>%
    split(~Farm) %>%
    map(\(farm) {
        farm %>%
            st_make_valid() %>%
            group_by(Farm, Field) %>%
            summarise(geom = st_union(geom)) %>%
            st_transform(32750)
    })
```

Run the `soil_cleaning` function on each farm:

```r
dat <-
    imap(soil_raw_em, \(farm, farm_name) {
        CBADA::soil_cleaning(
                   farm,
                   soil_bounds[[farm_name]],
                   c("DualEM150", "DualEM50")
               )
    })
```

We can then rasterize:

```r
krigs <-
    imap(dat, \(farm, farm_name) {
        imap(farm, \(padd, padd_name) {
            grid <- grids[[farm_name]] %>% filter(Field == padd_name)
            map(padd, \(padd_test) {
                CBADA::bkrige(padd_test, grid, response = "resp")
            })
        })
    })
```

And then plot the maps:

```r
ycol <- viridis::viridis_pal()(9)
name_units <- list(
    DualEM150 = "DualEM at 150mm (mS/m)",
    DualEM50 = "DualEM at 50mm (mS/m)"
)

em_plots <-
    imap(krigs, \(farms, farm_name) {
        imap(farms, \(farm_krigs, padd_names) {
            bound <- soil_bounds[[farm_name]] %>% filter(Field == padd_names)
            imap(farm_krigs, \(soil_test, test_name) {
                resp_name <- name_units[[test_name]]
                df <- soil_test %>% as.data.frame(xy = TRUE)
                df[[resp_name]] <- df$resp
                CBADA::plot_soil_map(df, bound, resp_name, ycol)
            }) %>% set_names(names(farm_krigs))
        })
    })
```

### Profit

The profit function is very simple, currently evaluating the following equation:

```math
\text{Profit} = \text{Yield (t/ha)} \cdot \text{Sell Price (\$/t)} - \text{Production Costs (\$/ha)}
```

And is run as follows:

```R
calc_profit(yield_raster, sell_price, production_costs)
```

Where `yield_raster` is a `terra` `SpatRaster` object, and the price and cost
are numerics.

At some stage this function will be expanded to allow for calculation including
application costs and rates.
