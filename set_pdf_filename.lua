function Meta(meta)
  if FORMAT == "pdf" then
    local filename = meta["slug"]
    if filename then
      meta["output-file"] = filename 
      return meta
    end
  end
end
