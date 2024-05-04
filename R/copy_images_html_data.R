
# data
from_path <- here::here("data/*")
to_path <- here::here("_site/data/")

system(paste("cp -r",from_path,to_path))

# images
from_path <- here::here("images/*")
to_path <- here::here("_site/images/")

system(paste("cp -r",from_path,to_path))

# lib

from_path <- here::here("lib/*")
to_path <- here::here("_site/lib/")
if (!dir.exists(to_path)) {
  dir.create(to_path)
}

system(paste("cp -r",from_path,to_path))

# html
from_path <- here::here("html/*")
to_path <- here::here("_site/html/")

if (!dir.exists(to_path)) {
  dir.create(to_path)
}
system(paste("cp -r",from_path,to_path))

