# Listing Datasets
installed.packages() |> 
  rownames() |> 
  lapply(function(v) {
    tmp <- data(package=v)$results |> as.data.frame() |> dplyr::select(-c(LibPath))
  }) |>
  do.call(what='rbind') -> lds

# Cleaning and adding dimensions
lds$Dim <- NA
for(i in 1:nrow(lds)){
  lds$Dim[i] <- tryCatch(eval(parse(text=paste0(lds$Package[i], '::', lds$Item[i]))) |> 
                           dim() |> paste0(collapse='x'), error=function(e) NA)
} 
na.omit(lds) -> lds

# Extracting the first two dimensions
lds |> 
  dplyr::mutate(
    dim=(gregexpr('x', Dim) |> sapply(length))+1,
    nrow = gsub('(^[0-9]+)x[0-9]+.*', '\\1', Dim) |> as.integer(),
    ncol = gsub('^[0-9]+x([0-9]+).*', '\\1', Dim) |> as.integer()
  ) |>
  na.omit() |> arrange(desc(ncol)) |> 
  #filter(ncol<20, ncol>5, nrow>500, nrow<2000) |> # filter if needed
  write.csv('R-Datasets.csv')

# Interesting datasets:
MASS::Boston
mosaicData::CPS85
mosaicData::Galton

carData::KosteckiDillon |> NACCdata::describe_ds()
