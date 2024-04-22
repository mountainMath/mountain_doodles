
# data
from_path <- here::here("data/*")
to_path <- here::here("_site/data/")

system(paste("cp -r",from_path,to_path))

# html
from_path <- here::here("html/*")
to_path <- here::here("_site/html/")

if (!dir.exists(to_path)) {
  dir.create(to_path)
}
system(paste("cp -r",from_path,to_path))

